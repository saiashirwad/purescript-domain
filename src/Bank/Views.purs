module Bank.Views
  ( Summary
  , StatementLine
  , summary
  , statement
  ) where

import Prelude

import Bank.Account (AccountEvent)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Domain (View)

type Summary =
  { owner :: Maybe String
  , balance :: Int
  }

summary :: View String Summary AccountEvent Unit
summary =
  { initial: { owner: Nothing, balance: 0 }
  , keyOf: \envelope -> Just envelope.stream
  , step: \state envelope ->
      Tuple
        ( match
            { accountOpened: \event -> state { owner = Just event.owner }
            , moneyDeposited: \event -> state { balance = event.balanceAfter }
            , moneyWithdrawn: \event -> state { balance = event.balanceAfter }
            }
            envelope.event
        )
        []
  }

type StatementLine =
  { kind :: String
  , amount :: Int
  , balance :: Int
  }

statement :: View String Unit AccountEvent StatementLine
statement =
  { initial: unit
  , keyOf: \envelope -> Just envelope.stream
  , step: \_ envelope ->
      Tuple unit
        ( match
            { accountOpened: \_ -> []
            , moneyDeposited: \event ->
                [ { kind: "deposit", amount: event.amount, balance: event.balanceAfter } ]
            , moneyWithdrawn: \event ->
                [ { kind: "withdrawal", amount: event.amount, balance: event.balanceAfter } ]
            }
            envelope.event
        )
  }
