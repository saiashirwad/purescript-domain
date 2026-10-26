module Test.Main where

import Prelude

import Bank.Account
  ( AccountCommand
  , AccountEvent
  , AccountId(..)
  , AccountRejection
  , AccountState(..)
  , accountDecider
  , accountOpened
  , alreadyOpen
  , depositMoney
  , insufficientFunds
  , moneyDeposited
  , moneyWithdrawn
  , notOpen
  , openAccount
  , withdrawMoney
  )
import Bank.Customer (CustomerId(..), customerRegistered, registerCustomer)
import Bank.Model
  ( emptyWorld
  , sendAccount
  , sendCustomer
  , statementFor
  , summaryFor
  )
import Bank.Views (StatementLine, Summary)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Domain (replay)
import Domain.Story (Step(..), runStory)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)

accountId :: AccountId
accountId = AccountId "a-1"

customerId :: CustomerId
customerId = CustomerId "c-1"

main :: Effect Unit
main = do
  testOpenAccount
  testOpenTwice
  testDeposit
  testDepositUnopened
  testOverdraft
  testSummary
  testStatement
  testPolicy
  testReplayLaw
  testStoryDeposit
  testStoryOverdraft
  testStoryUnopened
  log "All stories passed."

testOpenAccount :: Effect Unit
testOpenAccount = do
  result <- expectRight (sendAccount accountId (openAccount "sam") emptyWorld)
  assertEqual "opens.events" [ accountOpened "sam" ] result.receipt.events
  assertEqual "opens.version" 1 result.receipt.version
  assertSummary "opens.summary"
    { owner: Just "sam", balance: 0 }
    (summaryFor accountId result.world)

testOpenTwice :: Effect Unit
testOpenTwice = do
  opened <- expectRight (sendAccount accountId (openAccount "sam") emptyWorld)
  rejection <- expectLeft (sendAccount accountId (openAccount "sam") opened.world)
  assertEqual "refuses to open twice" alreadyOpen rejection

testDeposit :: Effect Unit
testDeposit = do
  opened <- expectRight (sendAccount accountId (openAccount "sam") emptyWorld)
  deposited <- expectRight (sendAccount accountId (depositMoney 100) opened.world)
  assertEqual "deposit.events" [ moneyDeposited 100 100 ] deposited.receipt.events
  assertEqual "deposit.reply" 100 deposited.receipt.reply.balance
  assertSummary "deposit.summary"
    { owner: Just "sam", balance: 100 }
    (summaryFor accountId deposited.world)

testDepositUnopened :: Effect Unit
testDepositUnopened = do
  rejection <- expectLeft (sendAccount accountId (depositMoney 100) emptyWorld)
  assertEqual "refuses a deposit into an unopened account" notOpen rejection

testOverdraft :: Effect Unit
testOverdraft = do
  opened <- expectRight (sendAccount accountId (openAccount "sam") emptyWorld)
  deposited <- expectRight (sendAccount accountId (depositMoney 100) opened.world)
  rejection <- expectLeft (sendAccount accountId (withdrawMoney 150) deposited.world)
  assertEqual "refuses an overdraft" (insufficientFunds 100 150) rejection

testSummary :: Effect Unit
testSummary = do
  opened <- expectRight (sendAccount accountId (openAccount "sam") emptyWorld)
  deposited <- expectRight (sendAccount accountId (depositMoney 100) opened.world)
  withdrawn <- expectRight (sendAccount accountId (withdrawMoney 30) deposited.world)
  assertSummary "summary follows the balance"
    { owner: Just "sam", balance: 70 }
    (summaryFor accountId withdrawn.world)

testStatement :: Effect Unit
testStatement = do
  opened <- expectRight (sendAccount accountId (openAccount "sam") emptyWorld)
  deposited <- expectRight (sendAccount accountId (depositMoney 100) opened.world)
  withdrawn <- expectRight (sendAccount accountId (withdrawMoney 30) deposited.world)
  assertEqual "statement has one line per movement"
    [ "deposit:100:100", "withdrawal:30:70" ]
    (map showLine (statementFor accountId withdrawn.world))

testPolicy :: Effect Unit
testPolicy = do
  registered <- expectRight (sendCustomer customerId (registerCustomer "sam") emptyWorld)
  assertEqual "customer.events" [ customerRegistered "sam" ] registered.receipt.events
  assertSummary "policy opens the main account"
    { owner: Just "sam", balance: 0 }
    (summaryFor (AccountId "c-1-main") registered.world)

testReplayLaw :: Effect Unit
testReplayLaw = do
  let
    before = [ accountOpened "sam" ]
    after = [ moneyDeposited 100 100, moneyWithdrawn 30 70 ]
    replayAll = replay accountDecider.evolve Unopened (before <> after)
    replayInSteps =
      replay accountDecider.evolve
        (replay accountDecider.evolve Unopened before)
        after
  assertEqual "replay is a monoid homomorphism" (show replayAll) (show replayInSteps)

testStoryDeposit :: Effect Unit
testStoryDeposit =
  expectAccountStory "deposits into an open account and replies with the balance"
    [ Given (accountOpened "sam")
    , When (depositMoney 100)
    , Then (moneyDeposited 100 100)
    , Replied { balance: 100 }
    ]

testStoryOverdraft :: Effect Unit
testStoryOverdraft =
  expectAccountStory "refuses to withdraw more than the balance"
    [ Given (accountOpened "sam")
    , Given (moneyDeposited 100 100)
    , When (withdrawMoney 150)
    , Rejected (insufficientFunds 100 150)
    ]

testStoryUnopened :: Effect Unit
testStoryUnopened =
  expectAccountStory "refuses a deposit into an account that isn't open"
    [ When (depositMoney 100)
    , Rejected notOpen
    ]

expectAccountStory
  :: String
  -> Array (Step AccountEvent AccountCommand AccountRejection { balance :: Int })
  -> Effect Unit
expectAccountStory label steps =
  case runStory accountDecider steps of
    Left error -> throw ("FAIL " <> label <> ": " <> error)
    Right _ -> pure unit

showLine :: StatementLine -> String
showLine line = line.kind <> ":" <> show line.amount <> ":" <> show line.balance

assertSummary :: String -> { owner :: Maybe String, balance :: Int } -> Maybe Summary -> Effect Unit
assertSummary label expected actual =
  case actual of
    Nothing -> throw ("FAIL " <> label <> ": no summary")
    Just summary -> do
      assertEqual (label <> ".owner") expected.owner summary.owner
      assertEqual (label <> ".balance") expected.balance summary.balance

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEqual label expected actual =
  if expected == actual then pure unit
  else
    throw
      ( "FAIL " <> label
          <> "\n  expected: " <> show expected
          <> "\n  actual:   " <> show actual
      )

expectRight :: forall a b. Show a => Either a b -> Effect b
expectRight = case _ of
  Right value -> pure value
  Left error -> throw ("expected Right, got Left: " <> show error)

expectLeft :: forall a b. Show b => Either a b -> Effect a
expectLeft = case _ of
  Left error -> pure error
  Right value -> throw ("expected Left, got Right: " <> show value)
