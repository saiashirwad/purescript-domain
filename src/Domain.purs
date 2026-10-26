module Domain
  ( Envelope
  , Receipt
  , Decider
  , Aggregate
  , View
  , ViewState
  , Step
  , Policy
  , Process
  , CommandEnvelope
  , Stream
  , emptyStream
  , mkEnvelope
  , replay
  , commit
  , applyView
  ) where

import Prelude

import Data.Array (foldl, length)
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple, fst, snd)

-- | Where an event came from. `id` is the aggregate instance id,
-- | `stream` is its full stream name, and `seq` is the position in that stream.
type Envelope e =
  { id :: String
  , stream :: String
  , seq :: Int
  , event :: e
  }

-- | What a caller gets after a command commits.
type Receipt e b =
  { stream :: String
  , version :: Int
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
  { stream :: id -> String
  , decider :: Decider s c e r b
  }

-- | One step of a stateful process: a new state and the values it emits.
-- | A view, a projection, and a saga all have this shape.
type Step s i o = s -> i -> Tuple s (Array o)

-- | A keyed read model. `keyOf` routes an event to a key; returning
-- | `Nothing` means the event is not for this view.
type View k s e out =
  { initial :: s
  , keyOf :: Envelope e -> Maybe k
  , step :: Step s (Envelope e) out
  }

type ViewState s out =
  { state :: s
  , emitted :: Array out
  }

-- | A policy reacts to an event by asking for more commands.
type CommandEnvelope c =
  { target :: String
  , command :: c
  }

type Policy e c =
  { on :: Envelope e -> Array (CommandEnvelope c) }

-- | A process manager (saga): a Mealy machine whose inputs are events
-- | and whose outputs are commands.
type Process k s e c =
  { initial :: s
  , keyOf :: Envelope e -> Maybe k
  , step :: Step s (Envelope e) (CommandEnvelope c)
  }

type Stream s e =
  { state :: s
  , version :: Int
  , history :: Array (Envelope e)
  }

emptyStream :: forall s e. s -> Stream s e
emptyStream initial =
  { state: initial
  , version: 0
  , history: []
  }

mkEnvelope :: forall e. String -> String -> Int -> e -> Envelope e
mkEnvelope id stream seq event =
  { id
  , stream
  , seq
  , event
  }

-- | Replay is a fold. The law worth testing:
-- | `replay evolve (xs <> ys) s == replay evolve ys (replay evolve xs s)`.
replay :: forall s e. (s -> e -> s) -> s -> Array e -> s
replay evolve = foldl evolve

commit
  :: forall s c e r b
   . Decider s c e r b
  -> { state :: s, version :: Int, stream :: String }
  -> c
  -> Either r { state :: s, receipt :: Receipt e b }
commit decider context command = do
  events <- decider.decide context.state command
  let
    state = replay decider.evolve context.state events
    version = context.version + length events
  pure
    { state
    , receipt:
        { stream: context.stream
        , version
        , events
        , reply: decider.reply state command
        }
    }

applyView
  :: forall k s e out
   . Ord k
  => View k s e out
  -> Map k (ViewState s out)
  -> Envelope e
  -> Map k (ViewState s out)
applyView view states envelope =
  case view.keyOf envelope of
    Nothing -> states
    Just key ->
      let
        current =
          fromMaybe
            { state: view.initial, emitted: [] }
            (Map.lookup key states)
        result = view.step current.state envelope
        next =
          { state: fst result
          , emitted: current.emitted <> snd result
          }
      in
        Map.insert key next states
