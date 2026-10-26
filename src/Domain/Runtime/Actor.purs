module Domain.Runtime.Actor
  ( Actor
  , Hooks
  , defaultHooks
  , defaultCapacity
  , new
  , newWithCapacity
  , newWithHooks
  , send
  , shutdown
  , load
  , history
  , shutdownError
  ) where

import Prelude

import Data.CatQueue (CatQueue)
import Data.CatQueue as CatQueue
import Data.Either (Either, either)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Domain
  ( Aggregate
  , CommandId
  , Envelope
  , Receipt
  , Stream
  , StreamId
  , emptyStream
  , streamIdText
  )
import Domain.Runtime.InMemory (InMemory)
import Domain.Runtime.InMemory as InMemory
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Aff (Aff, Fiber, attempt, finally, joinFiber, launchAff, throwError)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref (Ref)
import Effect.Ref as Ref

-- | Deterministic seams around command execution. A failing `beforeCommand`
-- | aborts the command before it commits; a failing `afterCommand` reports a
-- | failure after the commit already happened.
type Hooks =
  { beforeCommand :: StreamId -> Aff Unit
  , afterCommand :: StreamId -> Aff Unit
  }

defaultHooks :: Hooks
defaultHooks =
  { beforeCommand: \_ -> pure unit
  , afterCommand: \_ -> pure unit
  }

defaultCapacity :: Int
defaultCapacity = 64

type Job id s c e r b =
  { aggregate :: Aggregate id s c e r b
  , commandId :: CommandId
  , target :: id
  , command :: c
  , result :: AVar (Either Error (Either r (Receipt id e b)))
  }

type Worker id s c e r b =
  { mailbox :: Mailbox (Job id s c e r b)
  , fiber :: Fiber Unit
  }

newtype Actor id s c e r b = Actor
  { runtime :: InMemory id s c e r b
  , capacity :: Int
  , hooks :: Hooks
  , workers :: Ref (Map StreamId (Worker id s c e r b))
  , closed :: Ref Boolean
  , finished :: AVar Unit
  }

-- | A counting semaphore whose counter is only mutated in synchronous effect
-- | blocks. `gate` is filled exactly when a permit is available.
newtype Semaphore = Semaphore
  { permits :: Ref Int
  , gate :: AVar Unit
  }

mkSemaphore :: Int -> Aff Semaphore
mkSemaphore permits = do
  remaining <- liftEffect (Ref.new permits)
  gate <- AVar.new unit
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
  -- Only the empty-to-one edge can have a blocked producer.
  when (permits == 1) (void (EffectAVar.tryPut unit state.gate))

-- | A bounded FIFO mailbox. Enqueueing and signalling the worker share one
-- | effect so a queued command is never left without a wake token, and
-- | claiming a command frees its slot in the same step.
newtype Mailbox a = Mailbox
  { pending :: Ref (CatQueue a)
  , ready :: AVar Unit
  , slots :: Semaphore
  }

mkMailbox :: forall a. Int -> Aff (Mailbox a)
mkMailbox capacity = do
  pending <- liftEffect (Ref.new CatQueue.empty)
  ready <- AVar.empty
  slots <- mkSemaphore capacity
  pure (Mailbox { pending, ready, slots })

post :: forall a. Mailbox a -> a -> Effect Unit
post (Mailbox box) item = do
  Ref.modify_ (\pending -> CatQueue.snoc pending item) box.pending
  void (EffectAVar.tryPut unit box.ready)

claim :: forall a. Mailbox a -> Effect (Maybe a)
claim (Mailbox box) = do
  pending <- Ref.read box.pending
  case CatQueue.uncons pending of
    Nothing -> pure Nothing
    Just (Tuple item rest) -> do
      Ref.write rest box.pending
      release box.slots
      pure (Just item)

awaitWork :: forall a. Mailbox a -> Aff Unit
awaitWork (Mailbox box) = AVar.take box.ready

wake :: forall a. Mailbox a -> Effect Unit
wake (Mailbox box) = void (EffectAVar.tryPut unit box.ready)

reserve :: forall a. Mailbox a -> Aff Unit
reserve (Mailbox box) = acquire box.slots

unreserve :: forall a. Mailbox a -> Effect Unit
unreserve (Mailbox box) = release box.slots

new
  :: forall id s c e r b
   . InMemory id s c e r b
  -> Aff (Actor id s c e r b)
new = newWithCapacity defaultCapacity

newWithCapacity
  :: forall id s c e r b
   . Int
  -> InMemory id s c e r b
  -> Aff (Actor id s c e r b)
newWithCapacity capacity = newWithHooks capacity defaultHooks

newWithHooks
  :: forall id s c e r b
   . Int
  -> Hooks
  -> InMemory id s c e r b
  -> Aff (Actor id s c e r b)
newWithHooks capacity hooks runtime = do
  -- A capacity below one could never admit a command.
  let capacity' = max 1 capacity
  finished <- AVar.empty
  liftEffect do
    workers <- Ref.new Map.empty
    closed <- Ref.new false
    pure
      ( Actor
          { runtime
          , capacity: capacity'
          , hooks
          , workers
          , closed
          , finished
          }
      )

send
  :: forall id s c e r b
   . Actor id s c e r b
  -> Aggregate id s c e r b
  -> CommandId
  -> id
  -> c
  -> Aff (Either r (Receipt id e b))
send actor@(Actor _) aggregate commandId target command = do
  let streamId = aggregate.stream target
  stopped <- liftEffect (isClosed actor)
  when stopped (throwError (shutdownError streamId))
  worker <- workerFor actor streamId
  result <- liftEffect EffectAVar.empty
  let job = { aggregate, commandId, target, command, result }
  reserve worker.mailbox
  -- The closed check and the post share one effect: shutdown either finds
  -- this command in the queue or this sender observes the shutdown.
  accepted <- liftEffect do
    closed <- isClosed actor
    if closed then do
      unreserve worker.mailbox
      pure false
    else do
      post worker.mailbox job
      pure true
  unless accepted (throwError (shutdownError streamId))
  outcome <- AVar.take result
  either throwError pure outcome

load
  :: forall id s c e r b
   . Actor id s c e r b
  -> Aggregate id s c e r b
  -> id
  -> Aff (Stream id s e)
load (Actor state) aggregate target = do
  stored <- InMemory.snapshot state.runtime (aggregate.stream target)
  pure (fromMaybe (emptyStream aggregate.decider.initial) stored)

history
  :: forall id s c e r b
   . Actor id s c e r b
  -> Aggregate id s c e r b
  -> id
  -> Aff (Array (Envelope id e))
history (Actor state) aggregate target =
  InMemory.history state.runtime aggregate target

shutdown :: forall id s c e r b. Actor id s c e r b -> Aff Unit
shutdown actor@(Actor state) = do
  owner <- liftEffect do
    Ref.modify' (\wasClosed -> { state: true, value: not wasClosed }) state.closed
  if owner then finally (AVar.put unit state.finished) (drain actor)
  else AVar.read state.finished

-- | Closing the actor precedes waking the workers, so every worker that sees
-- | an empty queue after a wake also sees the actor as closed and exits.
drain :: forall id s c e r b. Actor id s c e r b -> Aff Unit
drain (Actor state) = do
  workers <- liftEffect do
    current <- Ref.read state.workers
    for_ current \worker -> wake worker.mailbox
    pure current
  for_ workers \worker -> joinFiber worker.fiber

shutdownError :: StreamId -> Error
shutdownError streamId =
  error
    ( "Domain.Runtime.Actor: the actor for "
        <> streamIdText streamId
        <> " is shut down"
    )

isClosed :: forall id s c e r b. Actor id s c e r b -> Effect Boolean
isClosed (Actor state) = Ref.read state.closed

workerFor
  :: forall id s c e r b
   . Actor id s c e r b
  -> StreamId
  -> Aff (Worker id s c e r b)
workerFor actor@(Actor state) streamId = do
  current <- liftEffect (Ref.read state.workers)
  case Map.lookup streamId current of
    Just worker -> pure worker
    Nothing -> do
      mailbox <- mkMailbox state.capacity
      liftEffect do
        latest <- Ref.read state.workers
        case Map.lookup streamId latest of
          Just worker -> pure worker
          Nothing -> do
            fiber <- launchAff (workLoop actor streamId mailbox)
            let worker = { mailbox, fiber }
            Ref.modify_ (Map.insert streamId worker) state.workers
            pure worker

workLoop
  :: forall id s c e r b
   . Actor id s c e r b
  -> StreamId
  -> Mailbox (Job id s c e r b)
  -> Aff Unit
workLoop actor streamId mailbox = do
  next <- nextJob actor mailbox
  case next of
    Nothing -> pure unit
    Just job -> do
      outcome <- attempt (runJob actor streamId job)
      AVar.put outcome job.result
      workLoop actor streamId mailbox

nextJob
  :: forall id s c e r b
   . Actor id s c e r b
  -> Mailbox (Job id s c e r b)
  -> Aff (Maybe (Job id s c e r b))
nextJob actor mailbox = do
  next <- liftEffect (claim mailbox)
  case next of
    Just job -> pure (Just job)
    Nothing -> do
      closed <- liftEffect (isClosed actor)
      if closed then pure Nothing
      else awaitWork mailbox *> nextJob actor mailbox

runJob
  :: forall id s c e r b
   . Actor id s c e r b
  -> StreamId
  -> Job id s c e r b
  -> Aff (Either r (Receipt id e b))
runJob (Actor state) streamId job = do
  state.hooks.beforeCommand streamId
  receipt <-
    InMemory.send state.runtime job.aggregate job.commandId job.target
      job.command
  state.hooks.afterCommand streamId
  pure receipt
