-- | What one command does to one stream, as a pure function. The runtime
-- | is a host around `turn`: it reads the clock, runs one turn per stream at
-- | a time, keeps the stream a commit returns, applies views to its facts,
-- | and dispatches the policies' orders.
module Domain.Stream (Stream, Stored, Fact, Turn(..), open, turn) where

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
import Domain
  (class Spec, Aggregate, Command, Envelope, Event, Receipt, Rejection, commit, decider, tagOf)
import Domain.Id (CommandId, StreamId, Version, advance, sequencesAfter, zeroVersion)

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
-- | `Conflict` is a command id that already names a command with another
-- | label on this stream.
data Turn id s spec b
  = Committed
      { stream :: Stream id s spec b, stored :: Stored id spec b, facts :: Array (Fact id s spec) }
  | Duplicate (Stored id spec b)
  | Conflict (Stored id spec b)
  | Refused (Rejection spec)

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
turn aggregate at commandId command current = case Map.lookup commandId current.receipts of
  Just stored
    | stored.command == tagOf command -> Duplicate stored
    | otherwise -> Conflict stored
  Nothing -> case commit aggregate current.state command of
    Left refused -> Refused refused
    Right committed ->
      let
        count = length committed.events
        envelopes = zipWith
          (\seq fact -> { id: current.id, stream: current.stream, seq, commandId, at, event: fact })
          (sequencesAfter current.version count) committed.events
        version = advance current.version count
        stored =
          { command: tagOf command
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
