module Bank.Views
  ( Summary
  , Entry(..)
  , StatementLine
  , Dashboard
  , summary
  , statement
  , dashboard
  ) where

import Prelude

import Bank.Account (AccountChange, AccountEvent(..), AccountId, Movement, customerOf)
import Bank.Customer (CustomerId)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Domain (View, byId, keyed)
import Domain.Fold as Fold

type Summary = { owner :: Maybe CustomerId, balance :: Int }

summary :: View AccountChange AccountId Summary
summary = byId (Fold.fold initial step)
  where
  initial = { owner: Nothing, balance: 0 }

  step row { event } = case event of
    AccountOpened { owner } -> row { owner = Just owner }
    MoneyDeposited { balanceAfter } -> row { balance = balanceAfter }
    MoneyWithdrawn { balanceAfter } -> row { balance = balanceAfter }

-- | Which way money moved. Encodes as `"deposit"` or `"withdrawal"`.
data Entry = Deposit | Withdrawal

derive instance Eq Entry

instance Show Entry where
  show Deposit = "Deposit"
  show Withdrawal = "Withdrawal"

instance EncodeJson Entry where
  encodeJson Deposit = encodeJson "deposit"
  encodeJson Withdrawal = encodeJson "withdrawal"

type StatementLine = { kind :: Entry, amount :: Int, balance :: Int }

statement :: View AccountChange AccountId (Array StatementLine)
statement = byId (Fold.foldMap lines)
  where
  lines { event } = case event of
    AccountOpened _ -> []
    MoneyDeposited movement -> [ line Deposit movement ]
    MoneyWithdrawn movement -> [ line Withdrawal movement ]

  line :: Entry -> Movement -> StatementLine
  line kind { amount, balanceAfter } = { kind, amount, balance: balanceAfter }

type Dashboard = { accounts :: Int, balance :: Int }

-- | One row per customer across all their accounts. Deposits do not name
-- | the customer; the key asks the account, whose state knows its owner.
dashboard :: View AccountChange CustomerId Dashboard
dashboard = keyed owner (Fold.fold initial step)
  where
  owner = customerOf <<< _.state

  initial = { accounts: 0, balance: 0 }

  step row { event } = case event of
    AccountOpened _ -> row { accounts = row.accounts + 1 }
    MoneyDeposited { amount } -> row { balance = row.balance + amount }
    MoneyWithdrawn { amount } -> row { balance = row.balance - amount }
