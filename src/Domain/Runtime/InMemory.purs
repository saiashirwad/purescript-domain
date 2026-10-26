module Domain.Runtime.InMemory
  ( InMemory
  , Clock
  , systemClock
  , new
  , newWithClock
  , send
  , load
  , snapshot
  , history
  , receipt
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.CatQueue as CatQueue
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.JSDate as JSDate
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Domain
  ( Aggregate
  , CommandId
  , Commit
  , Envelope
  , Receipt
  , Stream
  , StreamId
  , commit
  , emptyStream
  , mkEnvelope
  , streamHistory
  , versionNumber
  )
import Domain.Id.Internal (Sequence(..))
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref

type Clock =
  { now :: Aff Instant
  }

systemClock :: Clock
systemClock =
  { now: do
      date <- liftEffect JSDate.now
      case JSDate.toInstant date of
        Just instant -> pure instant
        Nothing ->
          throwError
            (error "Domain.Runtime.InMemory: system clock out of range")
  }

newtype InMemory :: Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype InMemory id s c e r b = InMemory
  { store :: Ref (Store id s e b)
  , clock :: Clock
  }

type Store id s e b =
  { streams :: Map StreamId (Stream id s e)
  , receipts :: Map CommandId (Receipt id e b)
  }

emptyStore :: forall id s e b. Store id s e b
emptyStore =
  { streams: Map.empty
  , receipts: Map.empty
  }

new
  :: forall id s c e r b
   . Aff (InMemory id s c e r b)
new = newWithClock systemClock

newWithClock
  :: forall id s c e r b
   . Clock
  -> Aff (InMemory id s c e r b)
newWithClock clock = do
  store <- liftEffect (Ref.new emptyStore)
  pure (InMemory { store, clock })

send
  :: forall id s c e r b
   . InMemory id s c e r b
  -> Aggregate id s c e r b
  -> CommandId
  -> id
  -> c
  -> Aff (Either r (Receipt id e b))
send (InMemory runtime) aggregate commandId id command = do
  at <- runtime.clock.now
  liftEffect
    ( Ref.modify'
        (commitCommand aggregate commandId id command at)
        runtime.store
    )

commitCommand
  :: forall id s c e r b
   . Aggregate id s c e r b
  -> CommandId
  -> id
  -> c
  -> Instant
  -> Store id s e b
  -> { state :: Store id s e b, value :: Either r (Receipt id e b) }
commitCommand aggregate commandId id command at store =
  case Map.lookup commandId store.receipts of
    Just existing ->
      { state: store, value: Right existing }
    Nothing ->
      let
        streamId = aggregate.stream id
        stream = streamAt aggregate streamId store
        decision =
          commit aggregate.decider
            { state: stream.state
            , version: stream.version
            , stream: streamId
            }
            command
      in
        case decision of
          Left rejection ->
            { state: store, value: Left rejection }
          Right committed ->
            let
              committedReceipt =
                receiptFor id streamId commandId at stream committed
              nextStream =
                advanceStream stream committed committedReceipt.envelopes
            in
              { state:
                  record streamId nextStream committedReceipt store
              , value: Right committedReceipt
              }

streamAt
  :: forall id s c e r b
   . Aggregate id s c e r b
  -> StreamId
  -> Store id s e b
  -> Stream id s e
streamAt aggregate streamId store =
  fromMaybe
    (emptyStream aggregate.decider.initial)
    (Map.lookup streamId store.streams)

receiptFor
  :: forall id s e b
   . id
  -> StreamId
  -> CommandId
  -> Instant
  -> Stream id s e
  -> Commit s e b
  -> Receipt id e b
receiptFor id streamId commandId at stream committed =
  { commandId
  , stream: streamId
  , version: committed.version
  , events: committed.events
  , envelopes
  , reply: committed.reply
  }
  where
  envelopes = mapWithIndex
    ( \index event ->
        mkEnvelope id streamId
          (Sequence (versionNumber stream.version + 1 + index))
          commandId
          at
          event
    )
    committed.events

advanceStream
  :: forall id s e b
   . Stream id s e
  -> Commit s e b
  -> Array (Envelope id e)
  -> Stream id s e
advanceStream stream committed envelopes =
  { state: committed.state
  , version: committed.version
  , history: foldl CatQueue.snoc stream.history envelopes
  }

record
  :: forall id s e b
   . StreamId
  -> Stream id s e
  -> Receipt id e b
  -> Store id s e b
  -> Store id s e b
record streamId stream committedReceipt store =
  { streams: Map.insert streamId stream store.streams
  , receipts:
      Map.insert committedReceipt.commandId committedReceipt store.receipts
  }

snapshot
  :: forall id s c e r b
   . InMemory id s c e r b
  -> StreamId
  -> Aff (Maybe (Stream id s e))
snapshot (InMemory runtime) streamId = liftEffect do
  store <- Ref.read runtime.store
  pure (Map.lookup streamId store.streams)

load
  :: forall id s c e r b
   . InMemory id s c e r b
  -> Aggregate id s c e r b
  -> id
  -> Aff (Stream id s e)
load runtime aggregate id = do
  stored <- snapshot runtime (aggregate.stream id)
  pure (fromMaybe (emptyStream aggregate.decider.initial) stored)

history
  :: forall id s c e r b
   . InMemory id s c e r b
  -> Aggregate id s c e r b
  -> id
  -> Aff (Array (Envelope id e))
history runtime aggregate id = do
  stream <- snapshot runtime (aggregate.stream id)
  pure (maybe [] streamHistory stream)

receipt
  :: forall id s c e r b
   . InMemory id s c e r b
  -> CommandId
  -> Aff (Maybe (Receipt id e b))
receipt (InMemory runtime) commandId = liftEffect do
  store <- Ref.read runtime.store
  pure (Map.lookup commandId store.receipts)
