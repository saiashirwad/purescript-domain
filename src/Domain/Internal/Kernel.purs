-- | Aggregates, views, and policies. Each is an ordinary function or fold
-- | over the user's own command, event, and rejection types; `Domain`
-- | re-exports the part users need.
module Domain.Internal.Kernel
  ( Aggregate(..)
  , commit
  , replay
  , Change
  , View
  , byId
  , keyed
  , runView
  , Policy(..)
  , Order
  , Envelope
  , Receipt
  )
  where

import Prelude

import Data.Array (last, scanl)
import Data.DateTime.Instant (Instant)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Domain.Fold (Fold)
import Domain.Internal.Id (CommandId, Sequence, StreamId, Version)

-- | A decider. `decide` answers a command from the current state with the
-- | events it causes or a rejection. `evolve` folds one event into the state,
-- | and is total because the event has already happened. `reply` is what
-- | the sender gets back once the events are folded in. `id` names the
-- | aggregate's streams.
newtype Aggregate :: Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype Aggregate id c e r s b = Aggregate
  { initial :: s
  , decide :: c -> s -> Either r (Array e)
  , evolve :: s -> e -> s
  , reply :: s -> b
  }

-- | Decide, then fold the new events into the state. `states` holds the
-- | state right after each event.
commit ::
  forall id c e r s b.
  Aggregate id c e r s b ->
  s ->
  c ->
  Either r { state :: s, states :: Array s, events :: Array e, reply :: b }
commit (Aggregate a) state sent = do
  events <- a.decide sent state
  let
    states = scanl a.evolve state events
    next = fromMaybe state (last states)
  pure { state: next, states, events, reply: a.reply next }

-- | Replay is a fold, so `replay a s (xs <> ys) == replay a (replay a s xs) ys`.
replay :: forall id c e r s b. Aggregate id c e r s b -> s -> Array e -> s
replay (Aggregate a) = foldl a.evolve

-- | A committed event, the stream it landed on, and that stream's state
-- | right after it. Views and policies read changes.
type Change id s e = { id :: id, state :: s, event :: e }

-- | A read model: one fold per key. `read k` returns the fold's result over
-- | every change routed to `k`.
newtype View :: Type -> Type -> Type -> Type
newtype View i k v = View { key :: i -> Maybe k, fold :: Fold i v }

-- | One row per stream.
byId :: forall id s e v. Fold (Change id s e) v -> View (Change id s e) id v
byId = keyed (Just <<< _.id)

-- | One row per key. `Nothing` skips the change.
keyed :: forall i k v. (i -> Maybe k) -> Fold i v -> View i k v
keyed key fold = View { key, fold }

runView :: forall i k v. View i k v -> { key :: i -> Maybe k, fold :: Fold i v }
runView (View v) = v

-- | Reacts to a change by ordering commands. The runtime derives each
-- | order's command id from the change that caused it, so a retry places
-- | the same orders again.
newtype Policy :: Type -> Type -> Type -> Type
newtype Policy i id c = Policy (i -> Array (Order id c))

-- | A command for the stream `to`.
type Order id c = { to :: id, command :: c }

-- | A committed event with its address.
type Envelope id e =
  { id :: id
  , stream :: StreamId
  , seq :: Sequence
  , commandId :: CommandId
  , at :: Instant
  , event :: e
  }

-- | What a committed command returns. Retrying the command id returns the
-- | same receipt.
type Receipt e b = { commandId :: CommandId, version :: Version, events :: Array e, reply :: b }
