module Test.Actor (testActor) where

import Prelude

import Bank.Account
  ( AccountCommand
  , AccountEvent
  , AccountId(..)
  , AccountRejection
  , AccountState(..)
  , accountDecider
  , accountOpened
  , accountStream
  , depositMoney
  , moneyDeposited
  , notOpen
  , openAccount
  )
import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (isNothing)
import Data.Time.Duration (Milliseconds(..))
import Domain
  ( Aggregate
  , Receipt
  , versionNumber
  )
import Domain.Runtime.Actor as Actor
import Domain.Runtime.InMemory as InMemory
import Effect.Aff (Aff, attempt, delay, forkAff, joinFiber, throwError)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Test.Support (assertEqual, commandId)

type AccountAggregate =
  Aggregate AccountId AccountState AccountCommand AccountEvent AccountRejection
    { balance :: Int }

type AccountReceipt = Receipt AccountId AccountEvent { balance :: Int }

accountAggregate :: AccountAggregate
accountAggregate = { stream: accountStream, decider: accountDecider }

accountId :: AccountId
accountId = AccountId "a-1"

testActor :: Aff Unit
testActor = do
  testSameKeySerialization
  testDifferentKeysOverlap
  testBoundedQueue
  testFailureIsolation
  testShutdown

-- | The first command parks inside `beforeCommand`; the second is only sent
-- | once the first is provably running. A deposit can only succeed if it sees
-- | the open, so the receipt versions and history pin the order.
testSameKeySerialization :: Aff Unit
testSameKeySerialization = do
  runtime <- InMemory.new
  firstStarted <- AVar.empty
  let
    hooks = Actor.defaultHooks
      { beforeCommand = \_ -> void (AVar.tryPut unit firstStarted) }
  actor <- Actor.newWithHooks 8 hooks runtime
  opening <- forkAff
    (Actor.send actor accountAggregate (commandId "cmd-open") accountId
      (openAccount "sam")
    )
  AVar.take firstStarted
  depositing <- forkAff
    (Actor.send actor accountAggregate (commandId "cmd-deposit") accountId
      (depositMoney 100)
    )
  opened <- joinFiber opening >>= expectReceipt "open"
  deposited <- joinFiber depositing >>= expectReceipt "deposit"
  assertEqual "same-key control" 1 (versionNumber opened.version)
  assertEqual "same-key deposit" 2 (versionNumber deposited.version)
  assertEqual "open reply" 0 opened.reply.balance
  assertEqual "deposit reply" 100 deposited.reply.balance
  events <- Actor.history actor accountAggregate accountId
  assertEqual "same-key history keeps enqueue order"
    [ accountOpened "sam", moneyDeposited 100 100 ]
    (map _.event events)
  stream <- Actor.load actor accountAggregate accountId
  assertEqual "load sees the replayed state"
    (Open { owner: "sam", balance: 100 })
    stream.state

-- | Both keys must sit inside `beforeCommand` at the same time. If the actor
-- | serialized across keys, `bothStarted` would stay empty and the delayed
-- | check below would fail.
testDifferentKeysOverlap :: Aff Unit
testDifferentKeysOverlap = do
  runtime <- InMemory.new
  started <- liftEffect (Ref.new 0)
  bothStarted <- AVar.empty
  gate <- AVar.empty
  let
    hooks = Actor.defaultHooks
      { beforeCommand = \_ -> do
          count <- liftEffect (Ref.modify (_ + 1) started)
          when (count == 2) (void (AVar.tryPut unit bothStarted))
          AVar.read gate
      }
  actor <- Actor.newWithHooks 8 hooks runtime
  first <- forkAff
    (Actor.send actor accountAggregate (commandId "cmd-a") (AccountId "a")
      (openAccount "a")
    )
  second <- forkAff
    (Actor.send actor accountAggregate (commandId "cmd-b") (AccountId "b")
      (openAccount "b")
    )
  delay (Milliseconds 50.0)
  overlapped <- AVar.tryRead bothStarted
  AVar.put unit gate
  when (isNothing overlapped)
    (throwError (error "FAIL overlap: different keys did not overlap"))
  _ <- joinFiber first >>= expectReceipt "key a"
  _ <- joinFiber second >>= expectReceipt "key b"
  pure unit

-- | Capacity one. The first command holds the hook, the second waits in the
-- | queue, and the third blocks in the capacity semaphore. Nothing is dropped
-- | and the queue drains in order once the gate opens.
testBoundedQueue :: Aff Unit
testBoundedQueue = do
  runtime <- InMemory.new
  gate <- AVar.empty
  started <- AVar.empty
  isFirst <- liftEffect (Ref.new true)
  completed <- liftEffect (Ref.new 0)
  let
    hooks = Actor.defaultHooks
      { beforeCommand = \_ -> do
          first <- liftEffect
            (Ref.modify' (\pending -> { state: false, value: pending }) isFirst)
          when first do
            void (AVar.tryPut unit started)
            AVar.read gate
      }
  actor <- Actor.newWithHooks 1 hooks runtime
  let
    sendAndCount commandKey command = do
      receipt <-
        Actor.send actor accountAggregate commandKey accountId command
      liftEffect (Ref.modify_ (_ + 1) completed)
      pure receipt
  opening <- forkAff (sendAndCount (commandId "cmd-open") (openAccount "sam"))
  AVar.take started
  depositing <- forkAff
    (sendAndCount (commandId "cmd-deposit") (depositMoney 100))
  queuedDeposit <- forkAff
    (sendAndCount (commandId "cmd-deposit-2") (depositMoney 5))
  delay (Milliseconds 50.0)
  finishedEarly <- liftEffect (Ref.read completed)
  assertEqual "a full queue does not complete a send" 0 finishedEarly
  AVar.put unit gate
  _ <- joinFiber opening >>= expectReceipt "open"
  _ <- joinFiber depositing >>= expectReceipt "deposit"
  lastReceipt <- joinFiber queuedDeposit >>= expectReceipt "queued deposit"
  assertEqual "bounded queue preserves order" 3 (versionNumber lastReceipt.version)
  events <- Actor.history actor accountAggregate accountId
  assertEqual "bounded queue drains without dropping" 3 (length events)
  stream <- Actor.load actor accountAggregate accountId
  assertEqual "bounded queue applies every command"
    (Open { owner: "sam", balance: 105 })
    stream.state

-- | A hook failure aborts one command and reaches its caller. A domain
-- | rejection is just a `Left`. Neither stops the worker.
testFailureIsolation :: Aff Unit
testFailureIsolation = do
  runtime <- InMemory.new
  shouldFail <- liftEffect (Ref.new true)
  let
    hooks = Actor.defaultHooks
      { beforeCommand = \_ -> do
          failing <- liftEffect
            (Ref.modify' (\pending -> { state: false, value: pending }) shouldFail)
          when failing (throwError (error "hook failure"))
      }
  actor <- Actor.newWithHooks 4 hooks runtime
  failed <- attempt
    (Actor.send actor accountAggregate (commandId "cmd-open") accountId
      (openAccount "sam")
    )
  case failed of
    Left exception ->
      assertEqual "the caller sees the hook failure" "hook failure"
        (message exception)
    Right _ ->
      throwError (error "FAIL isolation: the hook did not fail the send")
  afterFailure <- Actor.history actor accountAggregate accountId
  assertEqual "a failed command commits nothing" 0 (length afterFailure)
  rejected <- Actor.send actor accountAggregate (commandId "cmd-deposit")
    accountId
    (depositMoney 100)
  case rejected of
    Left rejection ->
      assertEqual "a domain rejection survives the actor" notOpen rejection
    Right _ ->
      throwError (error "FAIL isolation: expected a domain rejection")
  opened <- Actor.send actor accountAggregate (commandId "cmd-open") accountId
    (openAccount "sam")
    >>= expectReceipt "open after failure"
  assertEqual "the actor keeps processing" 1 (versionNumber opened.version)
  events <- Actor.history actor accountAggregate accountId
  assertEqual "only the successful command committed" 1 (length events)

-- | Shutdown may start while one command runs and another is queued. Both
-- | must finish; afterwards sends fail with the documented error and repeated
-- | shutdowns return without doing more work.
testShutdown :: Aff Unit
testShutdown = do
  runtime <- InMemory.new
  gate <- AVar.empty
  started <- AVar.empty
  isFirst <- liftEffect (Ref.new true)
  let
    hooks = Actor.defaultHooks
      { beforeCommand = \_ -> do
          first <- liftEffect
            (Ref.modify' (\pending -> { state: false, value: pending }) isFirst)
          when first do
            void (AVar.tryPut unit started)
            AVar.read gate
      }
  actor <- Actor.newWithHooks 8 hooks runtime
  opening <- forkAff
    (Actor.send actor accountAggregate (commandId "cmd-open") accountId
      (openAccount "sam")
    )
  AVar.take started
  depositing <- forkAff
    (Actor.send actor accountAggregate (commandId "cmd-deposit") accountId
      (depositMoney 100)
    )
  delay (Milliseconds 20.0)
  closing <- forkAff (Actor.shutdown actor)
  delay (Milliseconds 20.0)
  AVar.put unit gate
  _ <- joinFiber opening >>= expectReceipt "open before shutdown"
  drained <- joinFiber depositing >>= expectReceipt "queued deposit drained"
  joinFiber closing
  assertEqual "shutdown drains queued work" 2 (versionNumber drained.version)
  rejected <- attempt
    (Actor.send actor accountAggregate (commandId "cmd-after") accountId
      (depositMoney 1)
    )
  case rejected of
    Left exception ->
      assertEqual "sends after shutdown fail"
        (message (Actor.shutdownError (accountStream accountId)))
        (message exception)
    Right _ ->
      throwError (error "FAIL shutdown: a send after shutdown succeeded")
  Actor.shutdown actor
  Actor.shutdown actor
  events <- Actor.history actor accountAggregate accountId
  assertEqual "shutdown commits nothing extra" 2 (length events)

expectReceipt :: String -> Either AccountRejection AccountReceipt -> Aff AccountReceipt
expectReceipt label = case _ of
  Right receipt -> pure receipt
  Left rejection ->
    throwError
      (error ("FAIL " <> label <> ": rejected with " <> show rejection))
