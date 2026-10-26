module Domain
  ( module Domain.Id
  , Envelope
  , Receipt
  , Commit
  , Decider
  , Aggregate
  , Step
  , View
  , ViewState
  , emptyViewState
  , Projection
  , Policy
  , PolicyM
  , Process
  , CommandRequest
  , CommandEnvelope
  , Changes(..)
  , changesVersion
  , changesValues
  , Stream
  , emptyStream
  , mkEnvelope
  , replay
  , commit
  , applyView
  , applyViews
  , commandsFrom
  , commandsFromM
  , viewEmitted
  , streamHistory
  ) where

import Prelude (class Eq, class Functor, class Ord, class Show, Unit, bind, map, pure, show, (<$>), (<=), (<>))

import Data.Array (fromFoldable, length, mapWithIndex)
import Data.CatQueue (CatQueue)
import Data.CatQueue as CatQueue
import Data.DateTime.Instant (Instant)
import Data.Either (Either)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (class Traversable)
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Domain.Id
  ( CommandId
  , IdError(..)
  , Sequence
  , StreamId
  , TenantId
  , Version
  , commandIdText
  , firstSequence
  , nextSequence
  , nextVersion
  , nextVersionBy
  , parseCommandId
  , parseSequence
  , parseStreamId
  , parseTenantId
  , parseVersion
  , sequenceNumber
  , streamIdText
  , tenantIdText
  , versionNumber
  , zeroVersion
  )
import Domain.Id.Internal as Internal

-- | Where an event came from. `id` is the aggregate instance id, `stream`
-- | is its full stream name, and `seq` is the position in that stream.
-- | The command id and timestamp make a committed event self-describing.
type Envelope id e =
  { id :: id
  , stream :: StreamId
  , seq :: Sequence
  , commandId :: CommandId
  , at :: Instant
  , event :: e
  }

-- | What a caller gets after a command commits. The raw events and the
-- | addressed envelopes are both present so views can consume one and
-- | callers can inspect the other.
type Receipt id e b =
  { commandId :: CommandId
  , stream :: StreamId
  , version :: Version
  , events :: Array e
  , envelopes :: Array (Envelope id e)
  , reply :: b
  }

-- | A pure decision, before the runtime adds addresses and timestamps.
type Commit s e b =
  { state :: s
  , stream :: StreamId
  , version :: Version
  , events :: Array e
  , reply :: b
  }

-- | A pure decider. `decide` may refuse; `evolve` may not.
-- | `evolve` is total because events that already happened are facts.
type Decider s c e r b =
  { initial :: s
  , decide :: s -> c -> Either r (Array e)
  , evolve :: s -> e -> s
  , reply :: s -> c -> b
  }

-- | An aggregate is a decider plus a way to name its streams.
type Aggregate id s c e r b =
  { stream :: id -> StreamId
  , decider :: Decider s c e r b
  }

-- | One step of a stateful process: a new state and the values it emits.
-- | A view, a projection, and a saga all have this shape.
type Step s i o = s -> i -> Tuple s (Array o)

-- | A keyed read model. `keyOf` routes an event to a key; returning
-- | `Nothing` means the event is not for this view.
type View k s id e out =
  { initial :: s
  , keyOf :: Envelope id e -> Maybe k
  , step :: Step s (Envelope id e) out
  }

-- | A view's state plus its emitted history and the sequence it has seen
-- | from each source stream. The sequence map makes at-least-once delivery
-- | harmless even when the view's own state is not idempotent.
type ViewState s out =
  { state :: s
  , emitted :: CatQueue out
  , applied :: Map StreamId Sequence
  }

emptyViewState :: forall s out. s -> ViewState s out
emptyViewState initial =
  { state: initial
  , emitted: CatQueue.empty
  , applied: Map.empty
  }

-- | An effectful consumer of committed events. Rebuilding a projection is
-- | replaying it over the event log.
type Projection m id e =
  { on :: Envelope id e -> m Unit
  }

-- | A policy reacts to an event by asking for more commands. The request
-- | has no command id; `commandsFrom` derives one from the triggering
-- | event so a retry addresses the same command. `target` is the aggregate
-- | id type, so a policy cannot address the wrong stream.
type CommandRequest target c =
  { target :: target
  , command :: c
  }

-- | An addressed command, ready for the runtime.
type CommandEnvelope target c =
  { commandId :: CommandId
  , target :: target
  , command :: c
  }

type Policy id e target c =
  { on :: Envelope id e -> Array (CommandRequest target c) }

type PolicyM m id e target c =
  { on :: Envelope id e -> m (Array (CommandRequest target c)) }

-- | A process manager (saga): a Mealy machine whose inputs are events
-- | and whose outputs are command requests.
type Process k s id e target c =
  { initial :: s
  , keyOf :: Envelope id e -> Maybe k
  , step :: Step s (Envelope id e) (CommandRequest target c)
  }

-- | A batch that keeps its version while its values move. `traverse` lets a
-- | projection validate or encode every value in one pass.
data Changes a = Changes
  { version :: Version
  , values :: Array a
  }

derive instance eqChanges :: Eq a => Eq (Changes a)

instance showChanges :: Show a => Show (Changes a) where
  show (Changes changes) =
    "Changes { version: " <> show changes.version
      <> ", values: " <> show changes.values
      <> " }"

changesVersion :: forall a. Changes a -> Version
changesVersion (Changes changes) = changes.version

changesValues :: forall a. Changes a -> Array a
changesValues (Changes changes) = changes.values

instance functorChanges :: Functor Changes where
  map f (Changes changes) = Changes changes { values = map f changes.values }

instance foldableChanges :: Foldable Changes where
  foldr f acc (Changes changes) = Foldable.foldr f acc changes.values
  foldl f acc (Changes changes) = Foldable.foldl f acc changes.values
  foldMap f (Changes changes) = Foldable.foldMap f changes.values

instance traversableChanges :: Traversable Changes where
  traverse f (Changes changes) =
    (\values -> Changes changes { values = values })
      <$> Traversable.traverse f changes.values
  sequence (Changes changes) =
    (\values -> Changes changes { values = values })
      <$> Traversable.sequence changes.values

type Stream id s e =
  { state :: s
  , version :: Version
  , history :: CatQueue (Envelope id e)
  }

emptyStream :: forall id s e. s -> Stream id s e
emptyStream initial =
  { state: initial
  , version: zeroVersion
  , history: CatQueue.empty
  }

mkEnvelope
  :: forall id e
   . id
  -> StreamId
  -> Sequence
  -> CommandId
  -> Instant
  -> e
  -> Envelope id e
mkEnvelope id stream seq commandId at event =
  { id, stream, seq, commandId, at, event }

-- | Replay is a fold. The law worth testing:
-- | `replay evolve (xs <> ys) s == replay evolve ys (replay evolve xs s)`.
replay :: forall s e. (s -> e -> s) -> s -> Array e -> s
replay evolve = Foldable.foldl evolve

commit
  :: forall s c e r b
   . Decider s c e r b
  -> { state :: s, version :: Version, stream :: StreamId }
  -> c
  -> Either r (Commit s e b)
commit decider context command = do
  events <- decider.decide context.state command
  let
    state = replay decider.evolve context.state events
    version = nextVersionBy context.version (length events)
  pure
    { state
    , stream: context.stream
    , version
    , events
    , reply: decider.reply state command
    }

applyView
  :: forall k s id e out
   . Ord k
  => View k s id e out
  -> Map k (ViewState s out)
  -> Envelope id e
  -> Map k (ViewState s out)
applyView view states envelope =
  case view.keyOf envelope of
    Nothing -> states
    Just key ->
      let
        current =
          fromMaybe
            (emptyViewState view.initial)
            (Map.lookup key states)
      in
        case Map.lookup envelope.stream current.applied of
          Just seen | envelope.seq <= seen -> states
          _ ->
            let
              Tuple nextState emitted = view.step current.state envelope
              next =
                { state: nextState
                , emitted: Foldable.foldl CatQueue.snoc current.emitted emitted
                , applied:
                    Map.insert envelope.stream envelope.seq current.applied
                }
            in
              Map.insert key next states

applyViews
  :: forall k s id e out
   . Ord k
  => View k s id e out
  -> Map k (ViewState s out)
  -> Array (Envelope id e)
  -> Map k (ViewState s out)
applyViews view states envelopes =
  Foldable.foldl (applyView view) states envelopes

commandsFrom
  :: forall id e target c
   . Policy id e target c
  -> Envelope id e
  -> Array (CommandEnvelope target c)
commandsFrom policy envelope =
  address envelope (policy.on envelope)

commandsFromM
  :: forall m id e target c
   . Functor m
  => PolicyM m id e target c
  -> Envelope id e
  -> m (Array (CommandEnvelope target c))
commandsFromM policy envelope =
  map (address envelope) (policy.on envelope)

address
  :: forall id e target c
   . Envelope id e
  -> Array (CommandRequest target c)
  -> Array (CommandEnvelope target c)
address envelope =
  mapWithIndex \index request ->
    { commandId:
        Internal.CommandId
          ( commandIdText envelope.commandId <> "@"
              <> show (sequenceNumber envelope.seq)
              <> "#"
              <> show index
          )
    , target: request.target
    , command: request.command
    }

viewEmitted :: forall s out. ViewState s out -> Array out
viewEmitted viewState = fromFoldable viewState.emitted

streamHistory :: forall id s e. Stream id s e -> Array (Envelope id e)
streamHistory stream = fromFoldable stream.history
