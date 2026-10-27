-- | Runs a bounded context. `start` walks the context record and returns
-- | one handle per entry, under the same label:
-- |
-- | ```purescript
-- | rt <- start bank defaults
-- | receipt <- rt.account.send commandId accountId (cmd @"depositMoney" { amount: 100 })
-- | balance <- rt.summary.read accountId
-- | rt.stop
-- | ```
-- |
-- | An aggregate becomes `{ send, history }`, a view becomes `{ read,
-- | readAt }`, and a policy is wired to the aggregates its spec types name.
-- | Commands for one stream run one at a time; different streams run
-- | concurrently. A view applies a stream's events inside that stream's
-- | turn, so it never lags its source or reorders it.
module Domain.Runtime
  ( Options
  , Hooks
  , Span
  , defaults
  , start
  , AggregateHandle
  , ViewHandle
  , class Hosts
  , makeHosts
  , class Wire
  , wire
  , class FindAggregate
  ) where

import Prelude

import Data.Array (concatMap, intercalate, length, zipWith)
import Data.CatQueue (CatQueue)
import Data.CatQueue as CatQueue
import Data.DateTime.Instant (Instant, diff)
import Data.Either (Either(..))
import Data.Foldable (foldl, for_, sum)
import Data.Array as Array
import Data.JSDate as JSDate
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Domain
  ( class Spec
  , Aggregate
  , Command
  , Envelope
  , Event
  , Policy
  , Receipt
  , Rejection
  , View
  , commit
  , decider
  , orders
  , runView
  , tagOf
  )
import Domain.Id
  ( CommandId
  , Sequence
  , StreamId
  , Version
  , advance
  , commandIdText
  , sequenceNumber
  , sequencesAfter
  , streamIdText
  , streamOf
  , versionNumber
  , zeroVersion
  )
import Domain.Runtime.Serial (Serial)
import Domain.Runtime.Serial as Serial
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Aff (Aff, delay, finally, throwError)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Partial.Unsafe (unsafeCrashWith)
import Effect.Ref as Ref
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.TypeError (class Fail, Above, Beside, Quote, QuoteLabel, Text)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

-- | One timed command. Attribute names are fixed by the runtime; their
-- | values come from labels, so no user code lists them.
type Span =
  { name :: String
  , startedAt :: Instant
  , duration :: Milliseconds
  , attributes :: Map String String
  }

-- | Test seams around each command's turn on its stream. A failing
-- | `beforeCommand` aborts before the commit; a failing `afterCommand`
-- | reports a failure after it.
type Hooks =
  { beforeCommand :: StreamId -> Aff Unit
  , afterCommand :: StreamId -> Aff Unit
  }

type Options =
  { now :: Aff Instant
  , trace :: Span -> Aff Unit
  , capacity :: Int
  , hooks :: Hooks
  }

defaults :: Options
defaults =
  { now: do
      date <- liftEffect JSDate.now
      maybe (throwError (error "Domain.Runtime: system clock out of range")) pure
        (JSDate.toInstant date)
  , trace: \_ -> pure unit
  , capacity: 64
  , hooks: { beforeCommand: \_ -> pure unit, afterCommand: \_ -> pure unit }
  }

type AggregateHandle spec id b =
  { send :: CommandId -> id -> Command spec -> Aff (Either (Rejection spec) (Receipt (Event spec) b))
  , history :: id -> Aff (Array (Envelope id (Event spec)))
  }

type ViewHandle id v =
  { read :: id -> Aff (Maybe v)
  -- | Waits briefly for the entity's view to reach `version`, for a reader
  -- | that holds a receipt from another request.
  , readAt :: Version -> id -> Aff (Maybe v)
  }

-- | Start every aggregate, view, and policy in `ctx`. The result has one
-- | handle per aggregate and view label, plus `stop`, which refuses new
-- | commands, lets in-flight commands and their policy cascades finish,
-- | and drains every stream.
start
  :: forall ctx rl hosts rt
   . RowToList ctx rl
  => Hosts rl ctx hosts
  => Wire rl rl ctx hosts rt
  => Row.Lacks "stop" rt
  => Record ctx
  -> Options
  -> Aff (Record (stop :: Aff Unit | rt))
start ctx options = liftEffect do
  system <- newSystem options
  hosts <- Builder.buildFromScratch <$> makeHosts (Proxy :: Proxy rl) ctx system
  handles <- Builder.buildFromScratch <$> wire (Proxy :: Proxy rl) (Proxy :: Proxy rl) ctx hosts
  pure (Record.insert (Proxy :: Proxy "stop") (stopSystem system) handles)

-- | State every aggregate in one context shares.
type System =
  { options :: Options
  , serial :: Serial
  , closed :: Ref Boolean
  , inFlight :: Ref Int
  , idle :: AVar Unit
  }

newSystem :: Options -> Effect System
newSystem options = do
  serial <- Serial.new options.capacity
  closed <- Ref.new false
  inFlight <- Ref.new 0
  idle <- EffectAVar.empty
  pure { options, serial, closed, inFlight, idle }

-- | Admit an external command, or refuse it once the system is stopping.
-- | Policy commands skip this gate: they run inside the external command
-- | that caused them, which `stopSystem` already waits for.
admitted :: forall a. System -> Aff a -> Aff a
admitted system action = do
  admit <- liftEffect do
    closed <- Ref.read system.closed
    unless closed (Ref.modify_ (_ + 1) system.inFlight)
    pure (not closed)
  unless admit (throwError (error "Domain.Runtime: the runtime is stopped"))
  finally (liftEffect leave) action
  where
  leave = do
    remaining <- Ref.modify (_ - 1) system.inFlight
    closed <- Ref.read system.closed
    when (closed && remaining == 0) (void (EffectAVar.tryPut unit system.idle))

stopSystem :: System -> Aff Unit
stopSystem system = do
  busy <- liftEffect do
    Ref.write true system.closed
    (_ > 0) <$> Ref.read system.inFlight
  when busy (AVar.read system.idle)
  Serial.close system.serial

-- | One aggregate's store and subscribers. `handle` is what callers get;
-- | `send` skips the stop gate and is for policies only.
type Host spec id b =
  { handle :: AggregateHandle spec id b
  , send :: CommandId -> id -> Command spec -> Aff (Either (Rejection spec) (Receipt (Event spec) b))
  , onCommit :: (Array (Envelope id (Event spec)) -> Effect Unit) -> Effect Unit
  , afterCommit :: (Array (Envelope id (Event spec)) -> Aff Int) -> Effect Unit
  }

type Stream id s spec =
  { state :: s
  , version :: Version
  , history :: CatQueue (Envelope id (Event spec))
  }

type Stored id spec b =
  { receipt :: Receipt (Event spec) b
  , envelopes :: Array (Envelope id (Event spec))
  }

-- | Receipts are kept per aggregate: a command id names one command to one
-- | aggregate, so a retry finds its receipt and commits nothing new.
type Store id s spec b =
  { streams :: Map String (Stream id s spec)
  , receipts :: Map CommandId (Stored id spec b)
  }

data Outcome id spec b
  = Committed (Stored id spec b)
  | Duplicate (Stored id spec b)
  | Refused (Rejection spec)

newHost
  :: forall spec id s c e r b
   . Spec spec id s c e r b
  => Newtype id String
  => System
  -> String
  -> Aggregate spec
  -> Effect (Host spec id b)
newHost system name aggregate = do
  store <- Ref.new ({ streams: Map.empty, receipts: Map.empty } :: Store id s spec b)
  views <- Ref.new ([] :: Array (Array (Envelope id (Event spec)) -> Effect Unit))
  policies <- Ref.new ([] :: Array (Array (Envelope id (Event spec)) -> Aff Int))
  let
    options = system.options

    empty :: Stream id s spec
    empty = { state: (decider aggregate).initial, version: zeroVersion, history: CatQueue.empty }

    -- Runs in one synchronous effect, so the commit, the receipt, and every
    -- view update land together.
    decide :: CommandId -> id -> StreamId -> Instant -> Command spec -> Effect (Outcome id spec b)
    decide commandId id stream at sent = do
      current <- Ref.read store
      case Map.lookup commandId current.receipts of
        Just stored -> pure (Duplicate stored)
        Nothing -> do
          let streamState = fromMaybe empty (Map.lookup (unwrap id) current.streams)
          case commit aggregate streamState.state sent of
            Left refused -> pure (Refused refused)
            Right committed -> do
              let
                count = length committed.events
                envelopes =
                  zipWith
                    (\seq fact -> { id, stream, seq, commandId, at, event: fact })
                    (sequencesAfter streamState.version count)
                    committed.events
                version = advance streamState.version count
                stored =
                  { receipt: { commandId, version, events: committed.events, reply: committed.reply }
                  , envelopes
                  }
                next =
                  { state: committed.state
                  , version
                  , history: foldl CatQueue.snoc streamState.history envelopes
                  }
              Ref.write
                { streams: Map.insert (unwrap id) next current.streams
                , receipts: Map.insert commandId stored current.receipts
                }
                store
              Ref.read views >>= \subscribers -> for_ subscribers (_ $ envelopes)
              pure (Committed stored)

    -- Policies run after the stream's turn, so an order can target any
    -- stream without deadlock. A duplicate re-runs them: orders carry
    -- derived command ids, so a retry heals a crash between commit and
    -- dispatch without sending anything twice.
    propagate :: Array (Envelope id (Event spec)) -> Aff Int
    propagate envelopes = do
      subscribers <- liftEffect (Ref.read policies)
      sum <$> for subscribers (_ $ envelopes)

    send :: CommandId -> id -> Command spec -> Aff (Either (Rejection spec) (Receipt (Event spec) b))
    send commandId id sent = do
      let stream = streamOf name (unwrap id)
      startedAt <- options.now
      outcome <- Serial.run system.serial (streamIdText stream) do
        options.hooks.beforeCommand stream
        at <- options.now
        decided <- liftEffect (decide commandId id stream at sent)
        options.hooks.afterCommand stream
        pure decided
      ordered <- case outcome of
        Committed stored -> propagate stored.envelopes
        Duplicate stored -> propagate stored.envelopes
        Refused _ -> pure 0
      finishedAt <- options.now
      options.trace
        { name: "command." <> name
        , startedAt
        , duration: diff finishedAt startedAt
        , attributes: attributes commandId stream sent outcome ordered
        }
      pure case outcome of
        Committed stored -> Right stored.receipt
        Duplicate stored -> Right stored.receipt
        Refused refused -> Left refused

    history :: id -> Aff (Array (Envelope id (Event spec)))
    history id = liftEffect do
      current <- Ref.read store
      pure (maybe [] (Array.fromFoldable <<< _.history) (Map.lookup (unwrap id) current.streams))

  pure
    { handle: { send: \commandId id sent -> admitted system (send commandId id sent), history }
    , send
    , onCommit: \subscriber -> Ref.modify_ (_ <> [ subscriber ]) views
    , afterCommit: \subscriber -> Ref.modify_ (_ <> [ subscriber ]) policies
    }

attributes
  :: forall spec id s c e r b
   . Spec spec id s c e r b
  => CommandId
  -> StreamId
  -> Command spec
  -> Outcome id spec b
  -> Int
  -> Map String String
attributes commandId stream sent outcome ordered =
  Map.fromFoldable
    ( [ Tuple "command.id" (commandIdText commandId)
      , Tuple "stream.id" (streamIdText stream)
      , Tuple "command.name" (tagOf sent)
      , Tuple "policy.orders" (show ordered)
      ] <> case outcome of
        Committed stored -> committedAttributes "committed" stored
        Duplicate stored -> committedAttributes "duplicate" stored
        Refused refused ->
          [ Tuple "outcome" "rejected", Tuple "rejection" (tagOf refused) ]
    )
  where
  committedAttributes label stored =
    [ Tuple "outcome" label
    , Tuple "version" (show (versionNumber stored.receipt.version))
    , Tuple "events" (intercalate "," (map tagOf stored.receipt.events))
    ]

-- | A view's state per entity, with the last sequence it applied. The
-- | sequence makes redelivery harmless even when `step` is not idempotent.
type Applied v = { state :: v, seen :: Sequence }

newView
  :: forall spec id s c e r b v
   . Spec spec id s c e r b
  => Newtype id String
  => View spec v
  -> Host spec id b
  -> Effect (ViewHandle id v)
newView definition host = do
  states <- Ref.new (Map.empty :: Map String (Applied v))
  host.onCommit \envelopes -> Ref.modify_ (\current -> foldl apply current envelopes) states
  let
    read id = liftEffect (map _.state <<< Map.lookup (unwrap id) <$> Ref.read states)

    readAt version id = poll 50
      where
      poll attempts = do
        found <- liftEffect (Map.lookup (unwrap id) <$> Ref.read states)
        case found of
          Just applied | sequenceNumber applied.seen >= versionNumber version -> pure (Just applied.state)
          _ | attempts <= 0 -> pure Nothing
          _ -> delay (Milliseconds 1.0) *> poll (attempts - 1)
  pure { read, readAt }
  where
  fold = runView definition

  apply current envelope =
    let
      key = unwrap envelope.id
      previous = Map.lookup key current
    in
      case previous of
        Just applied | envelope.seq <= applied.seen -> current
        _ ->
          Map.insert key
            { state: fold.step (maybe fold.initial _.state previous) envelope.event
            , seen: envelope.seq
            }
            current

wirePolicy
  :: forall src sid ss sc se sr sb tgt tid ts tc te tr tb
   . Spec src sid ss sc se sr sb
  => Spec tgt tid ts tc te tr tb
  => String
  -> Policy src tgt
  -> Host src sid sb
  -> Host tgt tid tb
  -> Effect Unit
wirePolicy name definition source target =
  source.afterCommit \envelopes -> do
    let addressed = concatMap (orders name definition) envelopes
    for_ addressed \placed -> target.send placed.commandId placed.to placed.command
    pure (length addressed)

-- | Pass one: a host per aggregate entry, keyed by the entry's label.
class Hosts :: RowList Type -> Row Type -> Row Type -> Constraint
class Hosts rl ctx hosts | rl -> hosts where
  makeHosts :: Proxy rl -> Record ctx -> System -> Effect (Builder {} (Record hosts))

instance Hosts Nil ctx () where
  makeHosts _ _ _ = pure identity

instance
  ( IsSymbol l
  , Spec spec id s c e r b
  , Newtype id String
  , Row.Cons l (Aggregate spec) ctxTail ctx
  , Hosts t ctx tailHosts
  , Row.Cons l (Host spec id b) tailHosts hosts
  , Row.Lacks l tailHosts
  ) =>
  Hosts (Cons l (Aggregate spec) t) ctx hosts where
  makeHosts _ ctx system = do
    host <- newHost system (reflectSymbol (Proxy :: Proxy l)) (Record.get (Proxy :: Proxy l) ctx)
    rest <- makeHosts (Proxy :: Proxy t) ctx system
    pure (Builder.insert (Proxy :: Proxy l) host <<< rest)

else instance Hosts t ctx hosts => Hosts (Cons l entry t) ctx hosts where
  makeHosts _ = makeHosts (Proxy :: Proxy t)

-- | Pass two: a handle per aggregate and view, and the policy wiring.
-- | `all` is the whole context, where views and policies find their
-- | aggregates by spec.
class Wire :: RowList Type -> RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class Wire rl all ctx hosts rt | rl -> rt where
  wire :: Proxy rl -> Proxy all -> Record ctx -> Record hosts -> Effect (Builder {} (Record rt))

instance Wire Nil all ctx hosts () where
  wire _ _ _ _ = pure identity

instance
  ( IsSymbol l
  , Spec spec id s c e r b
  , Row.Cons l (Host spec id b) hostsTail hosts
  , Wire t all ctx hosts tailRt
  , Row.Cons l (AggregateHandle spec id b) tailRt rt
  , Row.Lacks l tailRt
  ) =>
  Wire (Cons l (Aggregate spec) t) all ctx hosts rt where
  wire _ all ctx hosts = do
    let host = Record.get (Proxy :: Proxy l) hosts
    rest <- wire (Proxy :: Proxy t) all ctx hosts
    pure (Builder.insert (Proxy :: Proxy l) host.handle <<< rest)

else instance
  ( IsSymbol l
  , Spec spec id s c e r b
  , Newtype id String
  , FindAggregate spec all source
  , IsSymbol source
  , Row.Cons source (Host spec id b) hostsTail hosts
  , Row.Cons l (View spec v) ctxTail ctx
  , Wire t all ctx hosts tailRt
  , Row.Cons l (ViewHandle id v) tailRt rt
  , Row.Lacks l tailRt
  ) =>
  Wire (Cons l (View spec v) t) all ctx hosts rt where
  wire _ all ctx hosts = do
    handle <- newView (Record.get (Proxy :: Proxy l) ctx) (Record.get (Proxy :: Proxy source) hosts)
    rest <- wire (Proxy :: Proxy t) all ctx hosts
    pure (Builder.insert (Proxy :: Proxy l) handle <<< rest)

else instance
  ( IsSymbol l
  , Spec src sid ss sc se sr sb
  , Spec tgt tid ts tc te tr tb
  , FindAggregate src all source
  , FindAggregate tgt all target
  , IsSymbol source
  , IsSymbol target
  , Row.Cons source (Host src sid sb) sourceTail hosts
  , Row.Cons target (Host tgt tid tb) targetTail hosts
  , Row.Cons l (Policy src tgt) ctxTail ctx
  , Wire t all ctx hosts rt
  ) =>
  Wire (Cons l (Policy src tgt) t) all ctx hosts rt where
  wire _ all ctx hosts = do
    wirePolicy (reflectSymbol (Proxy :: Proxy l)) (Record.get (Proxy :: Proxy l) ctx)
      (Record.get (Proxy :: Proxy source) hosts)
      (Record.get (Proxy :: Proxy target) hosts)
    wire (Proxy :: Proxy t) all ctx hosts

else instance
  Fail
    ( Beside (Text "a context entry must be an Aggregate, a View, or a Policy, but ")
        (Beside (QuoteLabel l) (Beside (Text " is ") (Quote entry)))
    ) =>
  Wire (Cons l entry t) all ctx hosts rt where
  wire _ _ _ _ = unsafeCrashWith "unreachable: rejected at compile time"

-- | The label of the aggregate in `rl` whose spec is `spec`.
class FindAggregate :: Type -> RowList Type -> Symbol -> Constraint
class FindAggregate spec rl l | spec rl -> l

instance FindAggregate spec (Cons l (Aggregate spec) t) l
else instance FindAggregate spec t l => FindAggregate spec (Cons m entry t) l
else instance
  Fail
    ( Above (Text "a view or policy reads an aggregate this context does not contain:")
        (Quote spec)
    ) =>
  FindAggregate spec Nil l
