module Test.Replay (testReplay) where

import Prelude

import Bank.Account (Account, AccountState(..), account)
import Bank.Customer (CustomerId(..))
import Data.Array.NonEmpty (cons')
import Data.Either (Either(..))
import Data.Foldable (foldl, sum)
import Data.Variant (match)
import Domain (Command, Event, command, commit, event, replay, toVariant)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck (quickCheck, (===))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, oneOf)

genOwner :: Gen CustomerId
genOwner = CustomerId <$> elements (cons' "sam" [ "kim", "ada" ])

genEvent :: Gen (Event Account)
genEvent = oneOf (cons' (opened <$> genOwner) [ movement moneyDeposited, movement moneyWithdrawn ])
  where
  opened owner = event @"accountOpened" { owner }
  moneyDeposited movement' = event @"moneyDeposited" movement'
  moneyWithdrawn movement' = event @"moneyWithdrawn" movement'
  movement make = do
    amount <- chooseInt 0 1000
    balanceAfter <- chooseInt 0 5000
    pure (make { amount, balanceAfter })

genCommand :: Gen (Command Account)
genCommand = oneOf
  ( cons' (openAccount <$> genOwner)
      [ depositMoney <$> chooseInt (-100) 1000, withdrawMoney <$> chooseInt (-100) 1000 ]
  )
  where
  openAccount owner = command @"openAccount" { owner }
  depositMoney amount = command @"depositMoney" { amount }
  withdrawMoney amount = command @"withdrawMoney" { amount }

genState :: Gen AccountState
genState = oneOf
  ( cons' (pure Unopened)
      [ do
          owner <- genOwner
          balance <- chooseInt 0 5000
          pure (Open { owner, balance })
      ]
  )

-- | Replay is a fold, and any run of commands from a new account leaves a
-- | balance that is the sum of its movements and never negative.
testReplay :: Aff Unit
testReplay = liftEffect do
  quickCheck do
    start <- genState
    before <- arrayOf genEvent
    after <- arrayOf genEvent
    pure
      ( replay account start (before <> after)
          === replay account (replay account start before) after
      )
  quickCheck do
    sent <- arrayOf genCommand
    let
      final = foldl step { state: Unopened, events: [] } sent
      balance = case final.state of
        Unopened -> 0
        Open open -> open.balance
    pure
      ( { balance, overdrawn: balance < 0 }
          === { balance: sum (map movement final.events), overdrawn: false }
      )
  where
  step acc next = case commit account acc.state next of
    Left _ -> acc
    Right committed -> { state: committed.state, events: acc.events <> committed.events }

  movement :: Event Account -> Int
  movement =
    match { accountOpened: \_ -> 0, moneyDeposited: _.amount, moneyWithdrawn: negate <<< _.amount }
      <<< toVariant
