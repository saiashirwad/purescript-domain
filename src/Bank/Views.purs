module Bank.Views (Summary, StatementLine, Dashboard, summary, statement, dashboard) where

import Prelude

import Bank.Account (AccountChange, AccountEvent(..), AccountId, customerOf)
import Bank.Customer (CustomerId)
import Data.Maybe (Maybe(..))
import Domain (View, byId, keyed)
import Domain.Fold as Fold

type Summary = { owner :: Maybe CustomerId, balance :: Int }

summary :: View AccountChange AccountId Summary
summary = byId $ Fold.fold { owner: Nothing, balance: 0 } \row { event } -> case event of
  AccountOpened { owner } -> row { owner = Just owner }
  MoneyDeposited { balanceAfter } -> row { balance = balanceAfter }
  MoneyWithdrawn { balanceAfter } -> row { balance = balanceAfter }

type StatementLine = { kind :: String, amount :: Int, balance :: Int }

statement :: View AccountChange AccountId (Array StatementLine)
statement = byId $ Fold.foldMap \{ event } -> case event of
  AccountOpened _ -> []
  MoneyDeposited { amount, balanceAfter } -> [ { kind: "deposit", amount, balance: balanceAfter } ]
  MoneyWithdrawn { amount, balanceAfter } -> [ { kind: "withdrawal", amount, balance: balanceAfter } ]

type Dashboard = { accounts :: Int, balance :: Int }

-- | One row per customer across all their accounts. Deposits do not name
-- | the customer; the key asks the account, whose state knows its owner.
dashboard :: View AccountChange CustomerId Dashboard
dashboard = keyed (customerOf <<< _.state) $ Fold.fold { accounts: 0, balance: 0 } \row { event } ->
  case event of
    AccountOpened _ -> row { accounts = row.accounts + 1 }
    MoneyDeposited { amount } -> row { balance = row.balance + amount }
    MoneyWithdrawn { amount } -> row { balance = row.balance - amount }
