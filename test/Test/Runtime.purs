module Test.Runtime (testRuntime) where

import Prelude

import Bank (bank)
import Bank.Account (AccountId(..))
import Bank.Customer (CustomerId(..))
import Bank.Policy (mainAccount)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Domain (cmd, event, rejection, sequenceNumber, versionNumber)
import Domain.Runtime (start)
import Effect.Aff (Aff)
import Test.Support (assertEqual, commandId, expectRight, testOptions)

testRuntime :: Aff Unit
testRuntime = do
  exactlyOnce
  rejectionStoresNothing
  views
  policy

exactlyOnce :: Aff Unit
exactlyOnce = do
  rt <- start bank testOptions
  let sam = AccountId "a-1"
  opened <- expectRight "open" =<< rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: "sam" })
  assertEqual "open records the event" [ event @"accountOpened" { owner: "sam" } ] opened.events
  deposited <- expectRight "deposit" =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  assertEqual "deposit replies with the balance" 100 deposited.reply.balance
  assertEqual "deposit is version 2" 2 (versionNumber deposited.version)

  retried <- expectRight "retry" =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  assertEqual "a retry returns the stored receipt" (show deposited) (show retried)
  history <- rt.account.history sam
  assertEqual "a retry appends nothing" [ 1, 2 ] (map (sequenceNumber <<< _.seq) history)

  changed <- expectRight "retry with other payload" =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 999 })
  assertEqual "a command id names one command: the stored receipt wins" (show deposited) (show changed)
  stillTwo <- rt.account.history sam
  assertEqual "a retry with another payload appends nothing" 2 (length stillTwo)
  rt.stop

rejectionStoresNothing :: Aff Unit
rejectionStoresNothing = do
  rt <- start bank testOptions
  let sam = AccountId "a-1"
  _ <- expectRight "open" =<< rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: "sam" })
  refused <- rt.account.send (commandId "overdraw") sam (cmd @"withdrawMoney" { amount: 500 })
  assertEqual "an overdraft is refused"
    (Left (rejection @"insufficientFunds" { balance: 0, requested: 500 }))
    (map _.events refused)
  again <- rt.account.send (commandId "overdraw") sam (cmd @"withdrawMoney" { amount: 500 })
  assertEqual "a refused command id is decided again, not replayed"
    (Left (rejection @"insufficientFunds" { balance: 0, requested: 500 }))
    (map _.events again)
  _ <- expectRight "fund" =<< rt.account.send (commandId "fund") sam (cmd @"depositMoney" { amount: 1000 })
  retried <- expectRight "retry after funding" =<< rt.account.send (commandId "overdraw") sam (cmd @"withdrawMoney" { amount: 500 })
  assertEqual "the same id now commits because it was never stored" 3 (versionNumber retried.version)
  rt.stop

views :: Aff Unit
views = do
  rt <- start bank testOptions
  let sam = AccountId "a-1"
  before <- rt.summary.read sam
  assertEqual "a view of an unseen entity is empty" Nothing before

  opened <- expectRight "open" =<< rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: "sam" })
  afterOpen <- rt.summary.read sam
  assertEqual "a read right after send sees the open" (Just { owner: Just "sam", balance: 0 }) afterOpen
  assertEqual "the open receipt is version 1" 1 (versionNumber opened.version)

  deposited <- expectRight "deposit" =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  afterDeposit <- rt.summary.read sam
  assertEqual "a read right after send sees the deposit" (Just { owner: Just "sam", balance: 100 }) afterDeposit

  withdrawn <- expectRight "withdraw" =<< rt.account.send (commandId "withdraw") sam (cmd @"withdrawMoney" { amount: 30 })
  afterWithdraw <- rt.summary.read sam
  assertEqual "a read right after send sees the withdrawal" (Just { owner: Just "sam", balance: 70 }) afterWithdraw
  caughtUp <- rt.summary.readAt withdrawn.version sam
  assertEqual "readAt sees the version the receipt names" (Just 70) (map _.balance caughtUp)
  atDeposit <- rt.summary.readAt deposited.version sam
  assertEqual "readAt a version already passed returns the current view" (Just 70) (map _.balance atDeposit)

  _ <- rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  _ <- rt.account.send (commandId "withdraw") sam (cmd @"withdrawMoney" { amount: 30 })
  statement <- rt.statement.read sam
  assertEqual "duplicate delivery does not double-apply"
    (Just [ { kind: "deposit", amount: 100, balance: 100 }, { kind: "withdrawal", amount: 30, balance: 70 } ])
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

policy :: Aff Unit
policy = do
  rt <- start bank testOptions
  let ada = CustomerId "c-1"
  registered <- expectRight "register" =<< rt.customer.send (commandId "register") ada (cmd @"registerCustomer" { name: "ada" })
  assertEqual "the customer is registered once" 1 (length registered.events)
  provisioned <- rt.summary.read (mainAccount ada)
  assertEqual "the policy opens the main account" (Just { owner: Just "ada", balance: 0 }) provisioned

  retried <- expectRight "retry register" =<< rt.customer.send (commandId "register") ada (cmd @"registerCustomer" { name: "ada" })
  assertEqual "a retried customer command returns the stored receipt" (show registered) (show retried)
  mainHistory <- rt.account.history (mainAccount ada)
  assertEqual "a retried trigger does not open a second account" 1 (length mainHistory)

  again <- rt.customer.send (commandId "register-again") ada (cmd @"registerCustomer" { name: "ada" })
  assertEqual "a new command id for a registered customer is refused"
    (Left (rejection @"alreadyRegistered" {}))
    (map _.events again)
  stillOne <- rt.account.history (mainAccount ada)
  assertEqual "a refused trigger opens nothing" 1 (length stillOne)
  rt.stop
