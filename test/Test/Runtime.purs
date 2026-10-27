module Test.Runtime (testRuntime) where

import Prelude

import Bank (bank)
import Bank.Account (AccountId(..))
import Bank.Customer (CustomerId(..))
import Bank.Policy (mainAccount)
import Data.Array (length, range)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Domain (cmd, event, rejection, sequenceNumber, versionNumber)
import Domain.Runtime (start)
import Effect.Aff (Aff, forkAff, joinFiber)
import Test.Support (assertEqual, commandId, expectRight, isLeft, testOptions)

testRuntime :: Aff Unit
testRuntime = do
  exactlyOnce
  rejectionStoresNothing
  views
  dashboard
  dashboardUnderConcurrency
  policy

exactlyOnce :: Aff Unit
exactlyOnce = do
  rt <- start bank testOptions
  let
    sam = AccountId "a-1"
  opened <- expectRight "open"
    =<< rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: CustomerId "sam" })
  assertEqual "open records the event" [ event @"accountOpened" { owner: CustomerId "sam" } ]
    opened.events
  deposited <- expectRight "deposit"
    =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  assertEqual "deposit replies with the balance" 100 deposited.reply.balance
  assertEqual "deposit is version 2" 2 (versionNumber deposited.version)

  retried <- expectRight "retry"
    =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  assertEqual "a retry returns the stored receipt" (show deposited) (show retried)
  history <- rt.account.history sam
  assertEqual "a retry appends nothing" [ 1, 2 ] (map (sequenceNumber <<< _.seq) history)

  changed <- expectRight "retry with other payload"
    =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 999 })
  assertEqual "a command id names one command: the stored receipt wins" (show deposited)
    (show changed)
  stillTwo <- rt.account.history sam
  assertEqual "a retry with another payload appends nothing" 2 (length stillTwo)
  rt.stop

rejectionStoresNothing :: Aff Unit
rejectionStoresNothing = do
  rt <- start bank testOptions
  let
    sam = AccountId "a-1"
  _ <- expectRight "open"
    =<< rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: CustomerId "sam" })
  refused <- rt.account.send (commandId "overdraw") sam (cmd @"withdrawMoney" { amount: 500 })
  assertEqual "an overdraft is refused"
    (Left (rejection @"insufficientFunds" { balance: 0, requested: 500 })) (map _.events refused)
  again <- rt.account.send (commandId "overdraw") sam (cmd @"withdrawMoney" { amount: 500 })
  assertEqual "a refused command id is decided again, not replayed"
    (Left (rejection @"insufficientFunds" { balance: 0, requested: 500 })) (map _.events again)
  _ <- expectRight "fund"
    =<< rt.account.send (commandId "fund") sam (cmd @"depositMoney" { amount: 1000 })
  retried <- expectRight "retry after funding"
    =<< rt.account.send (commandId "overdraw") sam (cmd @"withdrawMoney" { amount: 500 })
  assertEqual "the same id now commits because it was never stored" 3
    (versionNumber retried.version)
  rt.stop

views :: Aff Unit
views = do
  rt <- start bank testOptions
  let
    sam = AccountId "a-1"
  before <- rt.summary.read sam
  assertEqual "a view of an unseen entity is empty" Nothing before

  _ <- expectRight "open"
    =<< rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: CustomerId "sam" })
  afterOpen <- rt.summary.read sam
  assertEqual "a read right after send sees the open"
    (Just { owner: Just (CustomerId "sam"), balance: 0 }) afterOpen

  _ <- expectRight "deposit"
    =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  afterDeposit <- rt.summary.read sam
  assertEqual "a read right after send sees the deposit"
    (Just { owner: Just (CustomerId "sam"), balance: 100 }) afterDeposit

  _ <- expectRight "withdraw"
    =<< rt.account.send (commandId "withdraw") sam (cmd @"withdrawMoney" { amount: 30 })
  afterWithdraw <- rt.summary.read sam
  assertEqual "a read right after send sees the withdrawal"
    (Just { owner: Just (CustomerId "sam"), balance: 70 }) afterWithdraw

  _ <- rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  _ <- rt.account.send (commandId "withdraw") sam (cmd @"withdrawMoney" { amount: 30 })
  statement <- rt.statement.read sam
  assertEqual "duplicate delivery does not double-apply"
    ( Just
        [ { kind: "deposit", amount: 100, balance: 100 }
        , { kind: "withdrawal", amount: 30, balance: 70 }
        ]
    )
    statement

  _ <- rt.account.send (commandId "overdraw") sam (cmd @"withdrawMoney" { amount: 500 })
  afterRefusal <- rt.statement.read sam
  assertEqual "a refused command changes no view" 2 (maybe' 0 length afterRefusal)

  other <- rt.summary.read (AccountId "a-2")
  assertEqual "views are kept per entity" Nothing other
  rt.stop
  where
  maybe' default f = case _ of
    Nothing -> default
    Just value -> f value

-- | One row per customer, fed by every account the customer owns. Deposits
-- | do not name the customer; the row is found through the account's state.
dashboard :: Aff Unit
dashboard = do
  rt <- start bank testOptions
  let
    ada = CustomerId "c-1"
    kim = CustomerId "c-2"
    checking = AccountId "a-1"
    savings = AccountId "a-2"
    send key id command = expectRight key =<< rt.account.send (commandId key) id command
  before <- rt.dashboard.read ada
  assertEqual "a customer without accounts has no row" Nothing before

  _ <- send "open-checking" checking (cmd @"openAccount" { owner: ada })
  afterOpen <- rt.dashboard.read ada
  assertEqual "a read right after the open sees the account" (Just { accounts: 1, balance: 0 })
    afterOpen
  _ <- send "open-savings" savings (cmd @"openAccount" { owner: ada })
  _ <- send "deposit-checking" checking (cmd @"depositMoney" { amount: 100 })
  afterDeposit <- rt.dashboard.read ada
  assertEqual "a read right after a deposit sees it" (Just { accounts: 2, balance: 100 })
    afterDeposit
  _ <- send "deposit-savings" savings (cmd @"depositMoney" { amount: 50 })
  _ <- send "withdraw-checking" checking (cmd @"withdrawMoney" { amount: 30 })
  both <- rt.dashboard.read ada
  assertEqual "two accounts update one row" (Just { accounts: 2, balance: 120 }) both

  _ <- send "open-savings" savings (cmd @"openAccount" { owner: ada })
  _ <- send "deposit-checking" checking (cmd @"depositMoney" { amount: 100 })
  _ <- send "deposit-savings" savings (cmd @"depositMoney" { amount: 50 })
  _ <- send "withdraw-checking" checking (cmd @"withdrawMoney" { amount: 30 })
  retried <- rt.dashboard.read ada
  assertEqual "duplicate delivery from either account does not double-apply" both retried

  refused <- rt.account.send (commandId "overdraw") savings (cmd @"withdrawMoney" { amount: 500 })
  assertEqual "the overdraft is refused" true (isLeft refused)
  afterRefusal <- rt.dashboard.read ada
  assertEqual "a refused command changes no row" both afterRefusal

  _ <- send "open-other" (AccountId "a-3") (cmd @"openAccount" { owner: kim })
  _ <- send "deposit-other" (AccountId "a-3") (cmd @"depositMoney" { amount: 7 })
  other <- rt.dashboard.read kim
  assertEqual "another customer's account lands in another row" (Just { accounts: 1, balance: 7 })
    other
  unchanged <- rt.dashboard.read ada
  assertEqual "and leaves the first row alone" both unchanged

  let
    ben = CustomerId "c-3"
  _ <- expectRight "register"
    =<< rt.customer.send (commandId "register") ben (cmd @"registerCustomer" { name: "ben" })
  _ <- expectRight "retry register"
    =<< rt.customer.send (commandId "register") ben (cmd @"registerCustomer" { name: "ben" })
  provisioned <- rt.dashboard.read ben
  assertEqual "the policy's account joins its customer's row once"
    (Just { accounts: 1, balance: 0 }) provisioned
  rt.stop

-- | Deposits to two accounts of one customer run on two streams at once and
-- | all land in the one row.
dashboardUnderConcurrency :: Aff Unit
dashboardUnderConcurrency = do
  rt <- start bank testOptions
  let
    ada = CustomerId "c-1"
    accounts = [ AccountId "a-1", AccountId "a-2" ]
    amounts = range 1 20
  for_ accounts \id ->
    rt.account.send (commandId ("open-" <> show id)) id (cmd @"openAccount" { owner: ada })
  fibers <- for accounts \id -> for amounts \n -> forkAff
    (rt.account.send (commandId (show id <> "-" <> show n)) id (cmd @"depositMoney" { amount: n }))
  for_ (join fibers) joinFiber
  row <- rt.dashboard.read ada
  assertEqual "every deposit on both streams reached the row"
    (Just { accounts: 2, balance: 2 * (20 * 21 / 2) }) row
  rt.stop

policy :: Aff Unit
policy = do
  rt <- start bank testOptions
  let
    ada = CustomerId "c-1"
  registered <- expectRight "register"
    =<< rt.customer.send (commandId "register") ada (cmd @"registerCustomer" { name: "ada" })
  assertEqual "the customer is registered once" 1 (length registered.events)
  provisioned <- rt.summary.read (mainAccount ada)
  assertEqual "the policy opens the main account" (Just { owner: Just ada, balance: 0 }) provisioned

  retried <- expectRight "retry register"
    =<< rt.customer.send (commandId "register") ada (cmd @"registerCustomer" { name: "ada" })
  assertEqual "a retried customer command returns the stored receipt" (show registered)
    (show retried)
  mainHistory <- rt.account.history (mainAccount ada)
  assertEqual "a retried trigger does not open a second account" 1 (length mainHistory)

  again <-
    rt.customer.send (commandId "register-again") ada (cmd @"registerCustomer" { name: "ada" })
  assertEqual "a new command id for a registered customer is refused"
    (Left (rejection @"alreadyRegistered" {})) (map _.events again)
  stillOne <- rt.account.history (mainAccount ada)
  assertEqual "a refused trigger opens nothing" 1 (length stillOne)
  rt.stop
