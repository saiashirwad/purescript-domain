-- | A keyed serial executor: work for one key runs one job at a time in
-- | arrival order, and different keys run concurrently. Each key has a
-- | bounded mailbox, so a fast producer waits instead of growing memory.
module Domain.Runtime.Serial
  ( Serial
  , new
  , run
  , close
  ) where

import Prelude

import Data.CatQueue (CatQueue)
import Data.CatQueue as CatQueue
import Data.Either (either)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Aff (Aff, Fiber, attempt, joinFiber, launchAff, throwError)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref

newtype Serial = Serial
  { capacity :: Int
  , workers :: Ref (Map String Worker)
  , closed :: Ref Boolean
  }

type Worker = { mailbox :: Mailbox, fiber :: Fiber Unit }

new :: Int -> Effect Serial
new capacity = do
  workers <- Ref.new Map.empty
  closed <- Ref.new false
  pure (Serial { capacity: max 1 capacity, workers, closed })

-- | Run `job` after every job already queued for `key`. Fails once the
-- | executor is closed.
run :: forall a. Serial -> String -> Aff a -> Aff a
run serial@(Serial state) key job = do
  worker <- workerFor serial key
  result <- liftEffect EffectAVar.empty
  reserve worker.mailbox
  -- The closed check and the post share one effect: `close` either finds
  -- this job queued or this caller observes the close.
  accepted <- liftEffect do
    closed <- Ref.read state.closed
    if closed then unreserve worker.mailbox $> false
    else post worker.mailbox (attempt job >>= \outcome -> AVar.put outcome result) $> true
  unless accepted (throwError (error ("Domain.Runtime: " <> key <> " is shut down")))
  AVar.take result >>= either throwError pure

-- | Refuse new work, finish queued work, and wait for every worker to exit.
close :: Serial -> Aff Unit
close (Serial state) = do
  workers <- liftEffect do
    Ref.write true state.closed
    current <- Ref.read state.workers
    for_ current \worker -> wake worker.mailbox
    pure current
  for_ workers \worker -> joinFiber worker.fiber

workerFor :: Serial -> String -> Aff Worker
workerFor serial@(Serial state) key = do
  closed <- liftEffect (Ref.read state.closed)
  when closed (throwError (error ("Domain.Runtime: " <> key <> " is shut down")))
  liftEffect do
    current <- Ref.read state.workers
    case Map.lookup key current of
      Just worker -> pure worker
      Nothing -> do
        mailbox <- mkMailbox state.capacity
        fiber <- launchAff (loop serial mailbox)
        let worker = { mailbox, fiber }
        Ref.modify_ (Map.insert key worker) state.workers
        pure worker

loop :: Serial -> Mailbox -> Aff Unit
loop serial@(Serial state) mailbox = do
  next <- liftEffect (claim mailbox)
  case next of
    Just job -> job *> loop serial mailbox
    Nothing -> do
      closed <- liftEffect (Ref.read state.closed)
      unless closed (awaitWork mailbox *> loop serial mailbox)

-- | A bounded FIFO. Posting and waking share one effect so a queued job is
-- | never left without a wake token; claiming a job frees its slot.
newtype Mailbox = Mailbox
  { pending :: Ref (CatQueue (Aff Unit))
  , ready :: AVar Unit
  , slots :: Semaphore
  }

mkMailbox :: Int -> Effect Mailbox
mkMailbox capacity = do
  pending <- Ref.new CatQueue.empty
  ready <- EffectAVar.empty
  slots <- mkSemaphore capacity
  pure (Mailbox { pending, ready, slots })

post :: Mailbox -> Aff Unit -> Effect Unit
post (Mailbox box) job = do
  Ref.modify_ (\pending -> CatQueue.snoc pending job) box.pending
  void (EffectAVar.tryPut unit box.ready)

claim :: Mailbox -> Effect (Maybe (Aff Unit))
claim (Mailbox box) = do
  pending <- Ref.read box.pending
  case CatQueue.uncons pending of
    Nothing -> pure Nothing
    Just (Tuple job rest) -> do
      Ref.write rest box.pending
      release box.slots
      pure (Just job)

awaitWork :: Mailbox -> Aff Unit
awaitWork (Mailbox box) = AVar.take box.ready

wake :: Mailbox -> Effect Unit
wake (Mailbox box) = void (EffectAVar.tryPut unit box.ready)

reserve :: Mailbox -> Aff Unit
reserve (Mailbox box) = acquire box.slots

unreserve :: Mailbox -> Effect Unit
unreserve (Mailbox box) = release box.slots

-- | A counting semaphore whose counter changes only inside synchronous
-- | effects. `gate` is full exactly when a permit is available.
newtype Semaphore = Semaphore { permits :: Ref Int, gate :: AVar Unit }

mkSemaphore :: Int -> Effect Semaphore
mkSemaphore permits = do
  remaining <- Ref.new permits
  gate <- EffectAVar.new unit
  pure (Semaphore { permits: remaining, gate })

acquire :: Semaphore -> Aff Unit
acquire (Semaphore state) = do
  AVar.take state.gate
  liftEffect do
    remaining <- Ref.modify (_ - 1) state.permits
    when (remaining > 0) (void (EffectAVar.tryPut unit state.gate))

release :: Semaphore -> Effect Unit
release (Semaphore state) = do
  permits <- Ref.modify (_ + 1) state.permits
  when (permits == 1) (void (EffectAVar.tryPut unit state.gate))
