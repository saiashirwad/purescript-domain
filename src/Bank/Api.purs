-- | Who may touch what. Routes, codecs, and status codes come from
-- | `Domain.Http.serve bank guards`.
module Bank.Api (Principal, guards) where

import Prelude

import Bank.Account (AccountCommand(..), AccountId)
import Bank.Customer (CustomerId)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

type Principal =
  { subject :: String
  , roles :: Set String
  , customer :: Maybe CustomerId
  , accounts :: Set AccountId
  }

-- | No signature: `serve` checks each guard against its entry's types, and
-- | a signature here would restate them.
guards =
  { account: \principal id -> case _ of
      OpenAccount _ -> isAdmin principal
      DepositMoney _ -> ownsAccount principal id
      WithdrawMoney _ -> ownsAccount principal id
  , customer: \principal _ _ -> isAdmin principal
  , summary: ownsAccount
  , statement: ownsAccount
  , dashboard: \principal id -> principal.customer == Just id || isAdmin principal
  }

ownsAccount :: Principal -> AccountId -> Boolean
ownsAccount principal id = Set.member id principal.accounts || isAdmin principal

isAdmin :: Principal -> Boolean
isAdmin principal = Set.member "admin" principal.roles
