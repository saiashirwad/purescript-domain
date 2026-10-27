module Test.Stories (testStories) where

import Prelude

import Bank.Account (account)
import Data.Either (Either(..))
import Domain.Story (command, given, rejects, replies, emits, story)
import Effect.Aff (Aff)
import Test.Support (fail)

testStories :: Aff Unit
testStories = do
  expectStory "deposits into an open account and replies with the balance" $ story account
    [ given @"accountOpened" { owner: "sam" }
    , command @"depositMoney" { amount: 100 }
    , emits @"moneyDeposited" { amount: 100, balanceAfter: 100 }
    , replies { balance: 100 }
    ]
  expectStory "refuses to withdraw more than the balance" $ story account
    [ given @"accountOpened" { owner: "sam" }
    , given @"moneyDeposited" { amount: 100, balanceAfter: 100 }
    , command @"withdrawMoney" { amount: 150 }
    , rejects @"insufficientFunds" { balance: 100, requested: 150 }
    ]
  expectStory "refuses a deposit into an account that is not open"
    $ story account [ command @"depositMoney" { amount: 100 }, rejects @"notOpen" {} ]
  expectStory "refuses to open an account twice" $ story account
    [ given @"accountOpened" { owner: "sam" }
    , command @"openAccount" { owner: "kim" }
    , rejects @"alreadyOpen" {}
    ]
  expectFailure "a story with the wrong expected event fails" $ story account
    [ given @"accountOpened" { owner: "sam" }
    , command @"depositMoney" { amount: 100 }
    , emits @"moneyDeposited" { amount: 100, balanceAfter: 999 }
    ]

expectStory :: forall s. String -> Either String s -> Aff Unit
expectStory label = case _ of
  Right _ -> pure unit
  Left failure -> fail (label <> ": " <> failure)

expectFailure :: forall s. String -> Either String s -> Aff Unit
expectFailure label = case _ of
  Right _ -> fail label
  Left _ -> pure unit
