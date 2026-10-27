module Test.Concurrency (testConcurrency) where

import Prelude

import Bank (bank)
import Bank.Account (AccountId(..))
import Bank.Customer (CustomerId(..))
import Bank.Policy (mainAccount)
import Data.Array (length, range)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Domain (cmd, rejection, sequenceNumber, versionNumber)
import Domain.Runtime (Options, start)
import Effect.Aff (Aff, attempt, delay, forkAff, joinFiber, killFiber)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Test.Support (assertEqual, assertThat, commandId, expectRight, fail, isLeft, testOptions)

testConcurrency :: Aff Unit
testConcurrency = do
  sameStreamOrder
  differentStreamsOverlap
  boundedMailbox
  failureIsolation
  shutdownDrains
  shutdownDrainsCascades

-- | The first command parks in `beforeCommand` until every send is queued, so
-- | the mailbox really holds fifty commands when the stream starts to drain.
sameStreamOrder :: Aff Unit
sameStreamOrder = do
  gate <- AVar.empty
  isFirst <- liftEffect (Ref.new true)
  let
    hooks = testOptions.hooks
      { beforeCommand = \_ -> do
          first <- liftEffect (Ref.modify' (\pending -> { state: false, value: pending }) isFirst)
          when first (AVar.read gate)
      }
  rt <- start bank (testOptions { hooks = hooks })
  let
    sam = AccountId "a-1"
    deposits = range 1 40
  opening <- forkAff (rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: "sam" }))
  depositing <- for deposits \n ->
    forkAff (rt.account.send (commandId ("deposit-" <> show n)) sam (cmd @"depositMoney" { amount: n }))
  delay (Milliseconds 20.0)
  AVar.put unit gate
  opened <- joinFiber opening >>= expectRight "open"
  receipts <- for depositing \fiber -> joinFiber fiber >>= expectRight "deposit"
  assertEqual "the open ran first" 1 (versionNumber opened.version)
  assertEqual "deposits commit in send order" (map (_ + 1) deposits) (map (versionNumber <<< _.version) receipts)
  history <- rt.account.history sam
  assertEqual "the sequence has no gaps" (range 1 41) (map (sequenceNumber <<< _.seq) history)
  summary <- rt.summary.read sam
  assertEqual "every deposit applied" (Just (40 * 41 / 2)) (map _.balance summary)
  rt.stop

-- | Both streams must sit inside `beforeCommand` at once. If the runtime
-- | serialised across streams, the second would never arrive.
differentStreamsOverlap :: Aff Unit
differentStreamsOverlap = do
  started <- liftEffect (Ref.new 0)
  bothStarted <- AVar.empty
  gate <- AVar.empty
  let
    hooks = testOptions.hooks
      { beforeCommand = \_ -> do
          count <- liftEffect (Ref.modify (_ + 1) started)
          when (count == 2) (void (AVar.tryPut unit bothStarted))
          AVar.read gate
      }
  rt <- start bank (testOptions { hooks = hooks })
  first <- forkAff (rt.account.send (commandId "open-a") (AccountId "a") (cmd @"openAccount" { owner: "a" }))
  second <- forkAff (rt.account.send (commandId "open-b") (AccountId "b") (cmd @"openAccount" { owner: "b" }))
  delay (Milliseconds 50.0)
  overlapped <- AVar.tryRead bothStarted
  AVar.put unit gate
  assertThat "different streams ran at the same time" (not (isNothing overlapped))
  _ <- joinFiber first >>= expectRight "a"
  _ <- joinFiber second >>= expectRight "b"
  rt.stop

-- | Capacity one. The first command holds the stream, so the second can only
-- | wait in the mailbox and the third must wait outside it. Killing the third
-- | producer while it waits shows it never reached the stream; the second,
-- | already queued, still runs.
boundedMailbox :: Aff Unit
boundedMailbox = do
  gate <- AVar.empty
  started <- AVar.empty
  isFirst <- liftEffect (Ref.new true)
  completed <- liftEffect (Ref.new 0)
  let
    hooks = testOptions.hooks
      { beforeCommand = \_ -> do
          first <- liftEffect (Ref.modify' (\pending -> { state: false, value: pending }) isFirst)
          when first do
            void (AVar.tryPut unit started)
            AVar.read gate
      }
  rt <- start bank (testOptions { hooks = hooks, capacity = 1 })
  let
    sam = AccountId "a-1"
    send key command = do
      receipt <- rt.account.send (commandId key) sam command
      liftEffect (Ref.modify_ (_ + 1) completed)
      pure receipt
  opening <- forkAff (send "open" (cmd @"openAccount" { owner: "sam" }))
  AVar.take started
  queued <- forkAff (send "deposit-1" (cmd @"depositMoney" { amount: 100 }))
  delay (Milliseconds 20.0)
  producer <- forkAff (send "deposit-2" (cmd @"depositMoney" { amount: 5 }))
  delay (Milliseconds 50.0)
  finishedEarly <- liftEffect (Ref.read completed)
  assertEqual "a full mailbox completes nothing while the stream is busy" 0 finishedEarly
  killFiber (error "producer gave up") producer
  AVar.put unit gate
  _ <- joinFiber opening >>= expectRight "open"
  _ <- joinFiber queued >>= expectRight "queued deposit"
  history <- rt.account.history sam
  assertEqual "the waiting producer's command never entered the stream" 2 (length history)
  summary <- rt.summary.read sam
  assertEqual "the queued command ran, the waiting one did not" (Just 100) (map _.balance summary)

  rest <- for (range 1 20) \n ->
    forkAff (rt.account.send (commandId ("more-" <> show n)) sam (cmd @"depositMoney" { amount: 1 }))
  for_' rest \fiber -> joinFiber fiber >>= expectRight "more" >>> void
  drained <- rt.account.history sam
  assertEqual "a small mailbox drops nothing" 22 (length drained)
  rt.stop
  where
  for_' xs f = void (for xs f)

-- | A hook failure aborts one command and reaches its caller. Neither it nor
-- | a domain rejection stops the stream.
failureIsolation :: Aff Unit
failureIsolation = do
  shouldFail <- liftEffect (Ref.new true)
  let
    hooks = testOptions.hooks
      { beforeCommand = \_ -> do
          failing <- liftEffect (Ref.modify' (\pending -> { state: false, value: pending }) shouldFail)
          when failing (fail "hook failure")
      }
  rt <- start bank (testOptions { hooks = hooks })
  let sam = AccountId "a-1"
  failed <- attempt (rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: "sam" }))
  case failed of
    Left exception -> assertThat "the caller sees the hook failure" (contains (Pattern "hook failure") (message exception))
    Right _ -> fail "the hook did not fail the send"
  none <- rt.account.history sam
  assertEqual "a failed command commits nothing" 0 (length none)
  refused <- rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  assertEqual "a domain rejection survives the stream" (Left (rejection @"notOpen" {})) (map _.events refused)
  opened <- expectRight "retry open" =<< rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: "sam" })
  assertEqual "the same id runs again after a hook failure" 1 (versionNumber opened.version)
  rt.stop

-- | Stop while one command runs and another waits behind it. Both finish.
-- | Afterwards sends fail and a second stop returns.
shutdownDrains :: Aff Unit
shutdownDrains = do
  gate <- AVar.empty
  started <- AVar.empty
  isFirst <- liftEffect (Ref.new true)
  let
    hooks = testOptions.hooks
      { beforeCommand = \_ -> do
          first <- liftEffect (Ref.modify' (\pending -> { state: false, value: pending }) isFirst)
          when first do
            void (AVar.tryPut unit started)
            AVar.read gate
      }
  rt <- start bank (testOptions { hooks = hooks })
  let sam = AccountId "a-1"
  opening <- forkAff (rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: "sam" }))
  AVar.take started
  depositing <- forkAff (rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 }))
  delay (Milliseconds 20.0)
  stopping <- forkAff rt.stop
  delay (Milliseconds 20.0)
  late <- attempt (rt.account.send (commandId "late") sam (cmd @"depositMoney" { amount: 1 }))
  assertThat "a send while stopping is refused" (isLeft late)
  AVar.put unit gate
  _ <- joinFiber opening >>= expectRight "open before stop"
  drained <- joinFiber depositing >>= expectRight "queued deposit"
  joinFiber stopping
  assertEqual "stop drains the work already accepted" 2 (versionNumber drained.version)
  afterStop <- attempt (rt.account.send (commandId "after") sam (cmd @"depositMoney" { amount: 1 }))
  assertThat "a send after stop is refused" (isLeft afterStop)
  rt.stop
  history <- rt.account.history sam
  assertEqual "stop commits nothing extra" 2 (length history)

-- | `stop` lets an in-flight command finish its policy cascade, even when
-- | the cascade targets another aggregate.
shutdownDrainsCascades :: Aff Unit
shutdownDrainsCascades = do
  let
    slow :: Options
    slow = testOptions
      { hooks = { beforeCommand: \_ -> delay (Milliseconds 5.0), afterCommand: \_ -> pure unit } }
  rt <- start bank slow
  let ada = CustomerId "c-2"
  registering <- forkAff (rt.customer.send (commandId "register") ada (cmd @"registerCustomer" { name: "ada" }))
  rt.stop
  _ <- joinFiber registering >>= expectRight "register"
  provisioned <- rt.summary.read (mainAccount ada)
  assertEqual "stop waits for the policy cascade" (Just { owner: Just "ada", balance: 0 }) provisioned
