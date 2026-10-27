-- | What one command does to one stream, as a pure function of the
-- | stream, the command, its id, and the time.
module Domain.Internal.Stream (Stream, Stored, Fact, Turn(..), Refusal(..), open, turn) where

import Prelude

import Data.Array (length, zipWith)
import Data.CatQueue (CatQueue)
import Data.CatQueue as CatQueue
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Domain.Internal.Id (CommandId, StreamId, Version, advance, sequencesAfter, zeroVersion)
import Domain.Internal.Kernel
  (class Spec, Aggregate, Command, Envelope, Event, Receipt, Rejection, commit, decider, labelOf)

-- | One stream's state. Receipts are kept per stream: a command id names
-- | one command to one stream, so the same id sent to another stream is a
-- | different command.
type Stream id s spec b =
  { id :: id
  , stream :: StreamId
  , state :: s
  , version :: Version
  , history :: CatQueue (Envelope id (Event spec))
  , receipts :: Map CommandId (Stored id spec b)
  }

-- | A committed command's label, its receipt, and the envelopes it
-- | appended, which a retry hands to the policies again. The label tells a
-- | retry from an id reused for another command.
type Stored id spec b =
  { command :: String
  , receipt :: Receipt (Event spec) b
  , envelopes :: Array (Envelope id (Event spec))
  }

-- | A committed event with its stream's state right after it, which is
-- | what a keyed view reads its row key from.
type Fact id s spec = { envelope :: Envelope id (Event spec), state :: s }

-- | A refusal stores nothing, so the same command id may be decided again.
data Turn id s spec b
  = Committed
      { stream :: Stream id s spec b, stored :: Stored id spec b, facts :: Array (Fact id s spec) }
  | Duplicate (Stored id spec b)
  | Refused (Refusal spec)

-- | Why a command did not commit. `Rejected` is the aggregate's answer.
-- | `Reused` means the command id already names another command on this
-- | stream, the one labelled `command`.
data Refusal spec = Rejected (Rejection spec) | Reused { commandId :: CommandId, command :: String }

instance Eq (Rejection spec) => Eq (Refusal spec) where
  eq (Rejected x) (Rejected y) = x == y
  eq (Reused x) (Reused y) = x == y
  eq _ _ = false

instance Show (Rejection spec) => Show (Refusal spec) where
  show (Rejected rejected) = "(Rejected " <> show rejected <> ")"
  show (Reused reused) = "(Reused " <> show reused <> ")"

-- | A stream that holds no events yet.
open ::
  forall spec id s c e r b.
  Spec spec id s c e r b => Aggregate spec -> StreamId -> id -> Stream id s spec b
open aggregate stream id =
  { id
  , stream
  , state: (decider aggregate).initial
  , version: zeroVersion
  , history: CatQueue.empty
  , receipts: Map.empty
  }

turn ::
  forall spec id s c e r b.
  Spec spec id s c e r b =>
  Aggregate spec ->
  Instant ->
  CommandId ->
  Command spec ->
  Stream id s spec b ->
  Turn id s spec b
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
          , envelopes
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
          , facts: zipWith { envelope: _, state: _ } envelopes committed.states
          }
