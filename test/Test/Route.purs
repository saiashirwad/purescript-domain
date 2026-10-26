module Test.Route (testRoute) where

import Prelude

import Bank.Account (AccountId(..))
import Bank.Customer (CustomerId(..))
import Bank.Route
  ( BankRoute(..)
  , methodOf
  , parseRoute
  , printRoute
  )
import Data.Either (Either(..))
import Data.Foldable (for_)
import Domain.Http (Method(..))
import Effect.Aff (Aff)
import Test.Support (assertEqual)

testRoute :: Aff Unit
testRoute = do
  let routes =
        [ OpenAccount (AccountId "a-1")
        , DepositMoney (AccountId "a-1")
        , WithdrawMoney (AccountId "a-1")
        , AccountSummary (AccountId "a-1")
        , AccountStatement (AccountId "a-1")
        , RegisterCustomer (CustomerId "c-1")
        ]
  for_ routes \route ->
    assertEqual ("route round-trips: " <> show route)
      (Right route)
      (parseRoute (printRoute route))
  assertEqual "open is a POST" POST (methodOf (OpenAccount (AccountId "a-1")))
  assertEqual "summary is a GET" GET (methodOf (AccountSummary (AccountId "a-1")))
  case parseRoute "/not/a/bank/route" of
    Left _ -> pure unit
    Right route ->
      assertEqual "unknown route is rejected" "no route" (show route)
