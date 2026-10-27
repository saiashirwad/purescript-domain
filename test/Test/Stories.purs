module Test.Stories (testStories) where

import Prelude

import Bank.Account (account)
import Bank.Customer (CustomerId(..))
import Data.Either (Either(..))
import Data.Variant (Variant)
import Domain (Aggregate, Decision, aggregate, emit)
import Domain.Story (emits, given, rejects, replies, sends, story)
import Effect.Aff (Aff)
import Test.Support (fail)

testStories :: Aff Unit
testStories = do
  expectStory "deposits into an open account and replies with the balance" $ story account
    [ given @"accountOpened" { owner: CustomerId "sam" }
    , sends @"depositMoney" { amount: 100 }
    , emits @"moneyDeposited" { amount: 100, balanceAfter: 100 }
    , replies { balance: 100 }
    ]
  expectStory "refuses to withdraw more than the balance" $ story account
    [ given @"accountOpened" { owner: CustomerId "sam" }
    , given @"moneyDeposited" { amount: 100, balanceAfter: 100 }
    , sends @"withdrawMoney" { amount: 150 }
    , rejects @"insufficientFunds" { balance: 100, requested: 150 }
    ]
  expectStory "refuses a deposit into an account that is not open"
    $ story account [ sends @"depositMoney" { amount: 100 }, rejects @"notOpen" {} ]
  expectStory "refuses a deposit of zero" $ story account
    [ given @"accountOpened" { owner: CustomerId "sam" }
    , sends @"depositMoney" { amount: 0 }
    , rejects @"nonPositiveAmount" { amount: 0 }
    ]
  expectStory "refuses a negative withdrawal" $ story account
    [ given @"accountOpened" { owner: CustomerId "sam" }
    , given @"moneyDeposited" { amount: 100, balanceAfter: 100 }
    , sends @"withdrawMoney" { amount: -50 }
    , rejects @"nonPositiveAmount" { amount: -50 }
    ]
  expectStory "refuses to open an account twice" $ story account
    [ given @"accountOpened" { owner: CustomerId "sam" }
    , sends @"openAccount" { owner: CustomerId "kim" }
    , rejects @"alreadyOpen" {}
    ]
  expectFailure "a story with the wrong expected event fails" $ story account
    [ given @"accountOpened" { owner: CustomerId "sam" }
    , sends @"depositMoney" { amount: 100 }
    , emits @"moneyDeposited" { amount: 100, balanceAfter: 999 }
    ]
  expectStory "the emits steps after a command list all its events"
    $ story twice [ sends @"bumpTwice" {}, emits @"bumped" {}, emits @"bumped" {}, replies 2 ]
  expectFailure "a story that leaves an emitted event unlisted fails"
    $ story twice [ sends @"bumpTwice" {}, emits @"bumped" {}, replies 2 ]
  expectFailure "a story that lists an event the command did not emit fails" $ story twice
    [ sends @"bumpTwice" {}, emits @"bumped" {}, emits @"bumped" {}, emits @"bumped" {} ]

-- | One command that emits two events.
type Twice =
  { id :: Unit
  , state :: Int
  , command :: Variant (bumpTwice :: {})
  , event :: Variant (bumped :: {})
  , rejection :: Variant ()
  , reply :: Int
  }

twice :: Aggregate Twice
twice = aggregate
  { initial: 0, decide: { bumpTwice }, evolve: { bumped: \n _ -> n + 1 }, reply: identity }
  where
  bumpTwice :: Int -> {} -> Decision () (bumped :: {})
  bumpTwice _ _ = emit @"bumped" {} <> emit @"bumped" {}

expectStory :: forall s. String -> Either String s -> Aff Unit
expectStory label = case _ of
  Right _ -> pure unit
  Left failure -> fail (label <> ": " <> failure)

expectFailure :: forall s. String -> Either String s -> Aff Unit
expectFailure label = case _ of
  Right _ -> fail label
  Left _ -> pure unit
