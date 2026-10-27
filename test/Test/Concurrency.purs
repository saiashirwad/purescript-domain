module Test.Concurrency (testConcurrency) where

import Prelude

import Bank (bank)
import Bank.Account (AccountId(..))
import Bank.Customer (CustomerId(..))
import Bank.Policy (mainAccount)
import Data.Array (length, range)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Domain (command, rejection, sequenceNumber, versionNumber)
import Domain.Runtime (Options, Refusal(..), start)
import Effect.Aff (Aff, Fiber, attempt, delay, forkAff, joinFiber, killFiber)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Test.Support
  (assertEqual, assertThat, commandId, expectRight, fail, failingOnce, parked, testOptions)

testConcurrency :: Aff Unit
testConcurrency = do
  sameStreamOrder
  differentStreamsOverlap
  busyStreamWaits
  abandonedCommands
  failureIsolation
  shutdownDrains
  shutdownDrainsCascades

-- | The first command parks in `beforeTurn` until every send is waiting, so
-- | forty deposits really queue for the stream before it starts to drain.
sameStreamOrder :: Aff Unit
sameStreamOrder = do
  held <- parked "account/a-1"
  rt <- start bank testOptions { beforeTurn = held.beforeTurn }
  let
    sam = AccountId "a-1"
    deposits = range 1 40
  opening <- forkAff
    (rt.account.send (commandId "open") sam (command @"openAccount" { owner: CustomerId "sam" }))
  depositing <- for deposits \n -> forkAff
    (rt.account.send (commandId ("deposit-" <> show n)) sam (command @"depositMoney" { amount: n }))
  delay (Milliseconds 20.0)
  held.release
  opened <- joinFiber opening >>= expectRight "open"
  receipts <- for depositing \fiber -> joinFiber fiber >>= expectRight "deposit"
  assertEqual "the open ran first" 1 (versionNumber opened.version)
  assertEqual "deposits commit in send order" (map (_ + 1) deposits)
    (map (versionNumber <<< _.version) receipts)
  history <- rt.account.history sam
  assertEqual "the sequence has no gaps" (range 1 41) (map (sequenceNumber <<< _.seq) history)
  summary <- rt.summary.read sam
  assertEqual "every deposit applied" (Just (40 * 41 / 2)) (map _.balance summary)
  rt.stop

-- | Both streams must sit inside `beforeTurn` at once. If the runtime
-- | serialised across streams, the second would never arrive.
differentStreamsOverlap :: Aff Unit
differentStreamsOverlap = do
  started <- liftEffect (Ref.new 0)
  bothStarted <- AVar.empty
  gate <- AVar.empty
  let
    options =
      testOptions
        { beforeTurn = \_ -> do
            count <- liftEffect (Ref.modify (_ + 1) started)
            when (count == 2) (void (AVar.tryPut unit bothStarted))
            AVar.read gate
        }
  rt <- start bank options
  first <- forkAff
    ( rt.account.send (commandId "open-a") (AccountId "a")
        (command @"openAccount" { owner: CustomerId "a" })
    )
  second <- forkAff
    ( rt.account.send (commandId "open-b") (AccountId "b")
        (command @"openAccount" { owner: CustomerId "b" })
    )
  delay (Milliseconds 50.0)
  overlapped <- AVar.tryRead bothStarted
  AVar.put unit gate
  assertThat "different streams ran at the same time" (not (isNothing overlapped))
  _ <- joinFiber first >>= expectRight "a"
  _ <- joinFiber second >>= expectRight "b"
  rt.stop

-- | The first command holds the stream, so the second waits for it, then
-- | commits after it.
busyStreamWaits :: Aff Unit
busyStreamWaits = do
  held <- parked "account/a-1"
  completed <- liftEffect (Ref.new 0)
  rt <- start bank testOptions { beforeTurn = held.beforeTurn }
  let
    sam = AccountId "a-1"
    send key sent = do
      receipt <- rt.account.send (commandId key) sam sent
      liftEffect (Ref.modify_ (_ + 1) completed)
      pure receipt
  opening <- forkAff (send "open" (command @"openAccount" { owner: CustomerId "sam" }))
  AVar.take held.arrived
  depositing <- forkAff (send "deposit" (command @"depositMoney" { amount: 100 }))
  delay (Milliseconds 20.0)
  finishedEarly <- liftEffect (Ref.read completed)
  assertEqual "nothing completes while the stream is held" 0 finishedEarly
  turns <- held.turns
  assertEqual "the second command has not started its turn" 1 turns
  held.release
  opened <- joinFiber opening >>= expectRight "open"
  deposited <- joinFiber depositing >>= expectRight "deposit"
  assertEqual "the waiting command commits after the one holding the stream" [ 1, 2 ]
    (map (versionNumber <<< _.version) [ opened, deposited ])
  rt.stop

-- | A caller that gives up stops waiting, not the work. Its command, and
-- | the orders its policies place, still commit, and `stop` waits for them.
abandonedCommands :: Aff Unit
abandonedCommands = do
  waitingForTheStream
  duringTheTurn
  afterTheCommit

waitingForTheStream :: Aff Unit
waitingForTheStream = do
  held <- parked "account/a-1"
  rt <- start bank testOptions { beforeTurn = held.beforeTurn }
  let
    sam = AccountId "a-1"
  opening <- forkAff
    (rt.account.send (commandId "open") sam (command @"openAccount" { owner: CustomerId "sam" }))
  AVar.take held.arrived
  waiting <-
    forkAff (rt.account.send (commandId "deposit") sam (command @"depositMoney" { amount: 100 }))
  delay (Milliseconds 5.0)
  abandonThenStop rt waiting held.release
  _ <- joinFiber opening >>= expectRight "open"
  history <- rt.account.history sam
  assertEqual "the abandoned deposit committed after the open" 2 (length history)

duringTheTurn :: Aff Unit
duringTheTurn = do
  held <- parked "customer/c-7"
  rt <- start bank testOptions { beforeTurn = held.beforeTurn }
  let
    eve = CustomerId "c-7"
  registering <- forkAff
    (rt.customer.send (commandId "register") eve (command @"registerCustomer" { name: "eve" }))
  AVar.take held.arrived
  abandonThenStop rt registering held.release
  registered <- rt.customer.history eve
  assertEqual "the abandoned command committed" 1 (length registered)
  provisioned <- rt.account.history (mainAccount eve)
  assertEqual "the abandoned command's order committed" 1 (length provisioned)

afterTheCommit :: Aff Unit
afterTheCommit = do
  held <- parked "account/c-7-main"
  rt <- start bank testOptions { beforeTurn = held.beforeTurn }
  let
    eve = CustomerId "c-7"
  registering <- forkAff
    (rt.customer.send (commandId "register") eve (command @"registerCustomer" { name: "eve" }))
  AVar.take held.arrived
  abandonThenStop rt registering held.release
  provisioned <- rt.account.history (mainAccount eve)
  assertEqual "the order of a command abandoned after its commit committed" 1 (length provisioned)

-- | Kill `caller`, then stop. `stop` must still be waiting when the held
-- | stream is released, and must return once the work drains. The kill is
-- | forked because a runtime that ran the work on the caller's fiber would
-- | make it wait for the stream, and the test would hang instead of fail.
abandonThenStop :: forall r a. { stop :: Aff Unit | r } -> Fiber a -> Aff Unit -> Aff Unit
abandonThenStop rt caller release = do
  _ <- forkAff (killFiber (error "the caller gave up") caller)
  stopped <- liftEffect (Ref.new false)
  stopping <- forkAff (rt.stop *> liftEffect (Ref.write true stopped))
  delay (Milliseconds 20.0)
  early <- liftEffect (Ref.read stopped)
  assertThat "stop waits for the abandoned command" (not early)
  release
  joinFiber stopping

-- | A hook failure aborts one command and reaches its caller. Neither it nor
-- | a domain rejection stops the stream.
failureIsolation :: Aff Unit
failureIsolation = do
  failing <- failingOnce "account/a-1" "hook failure"
  rt <- start bank testOptions { beforeTurn = failing }
  let
    sam = AccountId "a-1"
  failed <- attempt
    (rt.account.send (commandId "open") sam (command @"openAccount" { owner: CustomerId "sam" }))
  case failed of
    Left exception -> assertThat "the caller sees the hook failure"
      (contains (Pattern "hook failure") (message exception))
    Right _ -> fail "the hook did not fail the send"
  none <- rt.account.history sam
  assertEqual "a failed command commits nothing" 0 (length none)
  refused <- rt.account.send (commandId "deposit") sam (command @"depositMoney" { amount: 100 })
  assertEqual "a domain rejection survives the stream" (Left (Rejected (rejection @"notOpen" {})))
    (map _.events refused)
  opened <-
    expectRight "retry open" =<< rt.account.send (commandId "open") sam
      (command @"openAccount" { owner: CustomerId "sam" })
  assertEqual "the same id runs again after a hook failure" 1 (versionNumber opened.version)
  rt.stop

-- | Stop while one command runs and another waits behind it. Both finish.
-- | Afterwards sends fail and a second stop returns.
shutdownDrains :: Aff Unit
shutdownDrains = do
  held <- parked "account/a-1"
  rt <- start bank testOptions { beforeTurn = held.beforeTurn }
  let
    sam = AccountId "a-1"
  opening <- forkAff
    (rt.account.send (commandId "open") sam (command @"openAccount" { owner: CustomerId "sam" }))
  AVar.take held.arrived
  depositing <-
    forkAff (rt.account.send (commandId "deposit") sam (command @"depositMoney" { amount: 100 }))
  delay (Milliseconds 20.0)
  stopping <- forkAff rt.stop
  delay (Milliseconds 20.0)
  late <- attempt (rt.account.send (commandId "late") sam (command @"depositMoney" { amount: 1 }))
  assertThat "a send while stopping is refused" (isLeft late)
  held.release
  _ <- joinFiber opening >>= expectRight "open before stop"
  drained <- joinFiber depositing >>= expectRight "queued deposit"
  joinFiber stopping
  assertEqual "stop drains the work already accepted" 2 (versionNumber drained.version)
  afterStop <-
    attempt (rt.account.send (commandId "after") sam (command @"depositMoney" { amount: 1 }))
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
    slow = testOptions { beforeTurn = \_ -> delay (Milliseconds 5.0) }
  rt <- start bank slow
  let
    ada = CustomerId "c-2"
  registering <- forkAff
    (rt.customer.send (commandId "register") ada (command @"registerCustomer" { name: "ada" }))
  rt.stop
  provisioned <- rt.summary.read (mainAccount ada)
  assertEqual "stop waits for the policy cascade" (Just { owner: Just ada, balance: 0 }) provisioned
  _ <- joinFiber registering >>= expectRight "register"
  pure unit
