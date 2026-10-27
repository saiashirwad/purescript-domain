module Bank.Views
  ( Summary
  , StatementLine
  , summary
  , statement
  ) where

import Prelude

import Bank.Account (Account)
import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Domain (View, view)

type Summary = { owner :: Maybe String, balance :: Int }

summary :: View Account Summary
summary = view
  { initial: { owner: Nothing, balance: 0 }
  , on:
      { accountOpened: \state { owner } -> state { owner = Just owner }
      , moneyDeposited: \state { balanceAfter } -> state { balance = balanceAfter }
      , moneyWithdrawn: \state { balanceAfter } -> state { balance = balanceAfter }
      }
  }

type StatementLine = { kind :: String, amount :: Int, balance :: Int }

-- | A view subscribes to the events it names and ignores the rest.
statement :: View Account (Array StatementLine)
statement = view
  { initial: []
  , on:
      { moneyDeposited: \lines { amount, balanceAfter } ->
          snoc lines { kind: "deposit", amount, balance: balanceAfter }
      , moneyWithdrawn: \lines { amount, balanceAfter } ->
          snoc lines { kind: "withdrawal", amount, balance: balanceAfter }
      }
  }
