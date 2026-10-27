module Test.Stories (testStories) where

import Prelude

import Bank.Account (AccountCommand(..), AccountEvent(..), AccountRejection(..), account)
import Bank.Customer (CustomerId(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Domain (Aggregate(..))
import Domain.Story (emits, given, rejects, replies, sends, story)
import Effect.Aff (Aff)
import Test.Support (fail)

testStories :: Aff Unit
testStories = do
  expectStory "deposits into an open account and replies with the balance" $ story account
    [ given (AccountOpened { owner: CustomerId "sam" })
    , sends (DepositMoney { amount: 100 })
    , emits (MoneyDeposited { amount: 100, balanceAfter: 100 })
    , replies { balance: 100 }
    ]
  expectStory "refuses to withdraw more than the balance" $ story account
    [ given (AccountOpened { owner: CustomerId "sam" })
    , given (MoneyDeposited { amount: 100, balanceAfter: 100 })
    , sends (WithdrawMoney { amount: 150 })
    , rejects (InsufficientFunds { balance: 100, requested: 150 })
    ]
  expectStory "refuses a deposit into an account that is not open"
    $ story account [ sends (DepositMoney { amount: 100 }), rejects NotOpen ]
  expectStory "refuses a deposit of zero" $ story account
    [ given (AccountOpened { owner: CustomerId "sam" })
    , sends (DepositMoney { amount: 0 })
    , rejects (NonPositiveAmount { amount: 0 })
    ]
  expectStory "refuses a negative withdrawal" $ story account
    [ given (AccountOpened { owner: CustomerId "sam" })
    , given (MoneyDeposited { amount: 100, balanceAfter: 100 })
    , sends (WithdrawMoney { amount: -50 })
    , rejects (NonPositiveAmount { amount: -50 })
    ]
  expectStory "refuses to open an account twice" $ story account
    [ given (AccountOpened { owner: CustomerId "sam" })
    , sends (OpenAccount { owner: CustomerId "kim" })
    , rejects AlreadyOpen
    ]
  expectFailure "a story with the wrong expected event fails" $ story account
    [ given (AccountOpened { owner: CustomerId "sam" })
    , sends (DepositMoney { amount: 100 })
    , emits (MoneyDeposited { amount: 100, balanceAfter: 999 })
    ]
  expectStory "the emits steps after a command list all its events"
    $ story twice [ sends BumpTwice, emits Bumped, emits Bumped, replies 2 ]
  expectFailure "a story that leaves an emitted event unlisted fails"
    $ story twice [ sends BumpTwice, emits Bumped, replies 2 ]
  expectFailure "a story that lists an event the command did not emit fails" $ story twice
    [ sends BumpTwice, emits Bumped, emits Bumped, emits Bumped ]

-- | One command that emits two events.
data TwiceCommand = BumpTwice

data Bumped = Bumped

derive instance Eq Bumped
derive instance Generic Bumped _
instance Show Bumped where
  show = genericShow

twice :: Aggregate Unit TwiceCommand Bumped Void Int Int
twice = Aggregate
  { initial: 0
  , decide: \BumpTwice _ -> Right [ Bumped, Bumped ]
  , evolve: \n Bumped -> n + 1
  , reply: identity
  }

expectStory :: forall s. String -> Either String s -> Aff Unit
expectStory label = case _ of
  Right _ -> pure unit
  Left failure -> fail (label <> ": " <> failure)

expectFailure :: forall s. String -> Either String s -> Aff Unit
expectFailure label = case _ of
  Right _ -> fail label
  Left _ -> pure unit
