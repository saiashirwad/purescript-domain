module Test.Replay (testReplay) where

import Prelude

import Bank.Account (Account, AccountState(..), account)
import Data.Array.NonEmpty (cons')
import Data.Either (Either(..))
import Domain (Command, Event, cmd, commit, event, replay)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck (quickCheck, (===))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, oneOf)

genOwner :: Gen String
genOwner = elements (cons' "sam" [ "kim", "ada" ])

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
      [ depositMoney <$> chooseInt 0 1000, withdrawMoney <$> chooseInt 0 1000 ]
  )
  where
  openAccount owner = cmd @"openAccount" { owner }
  depositMoney amount = cmd @"depositMoney" { amount }
  withdrawMoney amount = cmd @"withdrawMoney" { amount }

genState :: Gen AccountState
genState = oneOf
  ( cons' (pure Unopened)
      [ do
          owner <- genOwner
          balance <- chooseInt 0 5000
          pure (Open { owner, balance })
      ]
  )

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
    start <- genState
    sent <- genCommand
    pure case commit account start sent of
      Left _ -> true === true
      Right committed -> committed.state === replay account start committed.events
