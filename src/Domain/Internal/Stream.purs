-- | What one command does to one stream, as a pure function of the
-- | stream, the command, its id, and the time.
module Domain.Internal.Stream
  (Stream, Stored, Fact, Turn(..), Refusal(..), Addressed, open, turn, change, orders)
  where

import Prelude

import Data.Array (length, mapWithIndex, zipWith)
import Data.CatQueue (CatQueue)
import Data.CatQueue as CatQueue
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Domain.Internal.Id
  (CommandId, StreamId, Version, advance, derivedCommandId, sequencesAfter, zeroVersion)
import Data.Generic.Rep (class Generic)
import Domain.Internal.Kernel (Aggregate(..), Change, Envelope, Policy(..), Receipt, commit)
import Domain.Internal.Labels (class LabelOf, labelOf)
import Type.Equality (class TypeEquals, to)

-- | One stream's state. Receipts are kept per stream: a command id names
-- | one command to one stream, so the same id sent to another stream is a
-- | different command.
type Stream id e s b =
  { id :: id
  , stream :: StreamId
  , state :: s
  , version :: Version
  , history :: CatQueue (Envelope id e)
  , receipts :: Map CommandId (Stored id e s b)
  }

-- | A committed command's label, its receipt, and the facts it appended,
-- | which a retry hands to the policies again. The label tells a retry from
-- | an id reused for another command.
type Stored id e s b =
  { command :: String
  , receipt :: Receipt e b
  , facts :: Array (Fact id e s)
  }

-- | A committed event with its stream's state right after it, which is
-- | what a keyed view reads its row key from.
type Fact id e s = { envelope :: Envelope id e, state :: s }

-- | A refusal stores nothing, so the same command id may be decided again.
data Turn id e r s b
  = Committed { stream :: Stream id e s b, stored :: Stored id e s b }
  | Duplicate (Stored id e s b)
  | Refused (Refusal r)

-- | Why a command did not commit. `Rejected` is the aggregate's answer.
-- | `Reused` means the command id already names another command on this
-- | stream, the one labelled `command`.
data Refusal r = Rejected r | Reused { commandId :: CommandId, command :: String }

instance Eq r => Eq (Refusal r) where
  eq (Rejected x) (Rejected y) = x == y
  eq (Reused x) (Reused y) = x == y
  eq _ _ = false

instance Show r => Show (Refusal r) where
  show (Rejected rejected) = "(Rejected " <> show rejected <> ")"
  show (Reused reused) = "(Reused " <> show reused <> ")"

-- | A stream that holds no events yet.
open :: forall id c e r s b. Aggregate id c e r s b -> StreamId -> id -> Stream id e s b
open (Aggregate a) stream id =
  { id
  , stream
  , state: a.initial
  , version: zeroVersion
  , history: CatQueue.empty
  , receipts: Map.empty
  }

turn ::
  forall id c crep e r s b.
  Generic c crep =>
  LabelOf crep =>
  Aggregate id c e r s b ->
  Instant ->
  CommandId ->
  c ->
  Stream id e s b ->
  Turn id e r s b
turn aggregate at commandId sent current = case Map.lookup commandId current.receipts of
  Just stored
    | stored.command == labelOf sent -> Duplicate stored
    | otherwise -> Refused (Reused { commandId, command: stored.command })
  Nothing -> case commit aggregate current.state sent of
    Left rejected -> Refused (Rejected rejected)
    Right committed ->
      let
        count = length committed.events
        envelopes = zipWith
          (\seq event -> { id: current.id, stream: current.stream, seq, commandId, at, event })
          (sequencesAfter current.version count) committed.events
        version = advance current.version count
        stored =
          { command: labelOf sent
          , receipt: { commandId, version, events: committed.events, reply: committed.reply }
          , facts: zipWith { envelope: _, state: _ } envelopes committed.states
          }
      in
        Committed
          { stream:
              current
                { state = committed.state
                , version = version
                , history = foldl CatQueue.snoc current.history envelopes
                , receipts = Map.insert commandId stored current.receipts
                }
          , stored
          }

-- | What views and policies read from a fact.
change :: forall id e s. Fact id e s -> Change id s e
change { envelope, state } = { id: envelope.id, state, event: envelope.event }

-- | An order with its command id derived from the event that caused it.
type Addressed id c = { commandId :: CommandId, to :: id, command :: c }

-- | The orders the policy known as `name` places for one fact. Each id is
-- | `name:stream@seq#index`, so a retry derives the same ids again.
orders ::
  forall i id e s tid c.
  TypeEquals (Change id s e) i => String -> Policy i tid c -> Fact id e s -> Array (Addressed tid c)
orders name (Policy react) fact = mapWithIndex address (react (to (change fact)))
  where
  address index placed =
    { commandId: derivedCommandId name fact.envelope.stream fact.envelope.seq index
    , to: placed.to
    , command: placed.command
    }
