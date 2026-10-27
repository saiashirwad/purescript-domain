module Bank.Views (Summary, StatementLine, Dashboard, summary, statement, dashboard) where

import Prelude

import Bank.Account (Account, AccountId, customerOf)
import Bank.Customer (CustomerId)
import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Domain (View, view)

type Summary = { owner :: Maybe CustomerId, balance :: Int }

summary :: View Account AccountId Summary
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
statement :: View Account AccountId (Array StatementLine)
statement = view
  { initial: []
  , on:
      { moneyDeposited: \lines { amount, balanceAfter } ->
          snoc lines { kind: "deposit", amount, balance: balanceAfter }
      , moneyWithdrawn: \lines { amount, balanceAfter } ->
          snoc lines { kind: "withdrawal", amount, balance: balanceAfter }
      }
  }

type Dashboard = { accounts :: Int, balance :: Int }

-- | One row per customer across all their accounts. Deposits do not name
-- | the customer; `key` asks the account, whose state knows its owner.
dashboard :: View Account CustomerId Dashboard
dashboard = view
  { initial: { accounts: 0, balance: 0 }
  , key: customerOf
  , on:
      { accountOpened: \row _ -> row { accounts = row.accounts + 1 }
      , moneyDeposited: \row { amount } -> row { balance = row.balance + amount }
      , moneyWithdrawn: \row { amount } -> row { balance = row.balance - amount }
      }
  }
