-- | Who may touch what. Routes, codecs, and status codes come from
-- | `Domain.Http.serve bank guards`.
module Bank.Api (Principal, guards) where

import Prelude

import Bank.Account (AccountId)
import Bank.Customer (CustomerId)
import Data.Set (Set)
import Data.Set as Set

type Principal = { subject :: String, roles :: Set String, accounts :: Set AccountId }

type Guard id = Principal -> id -> Boolean

guards ::
  { account :: Guard AccountId
  , customer :: Guard CustomerId
  , summary :: Guard AccountId
  , statement :: Guard AccountId
  }
guards =
  { account: ownsAccount
  , customer: \principal _ -> isAdmin principal
  , summary: ownsAccount
  , statement: ownsAccount
  }

ownsAccount :: Guard AccountId
ownsAccount principal id = Set.member id principal.accounts || isAdmin principal

isAdmin :: Principal -> Boolean
isAdmin principal = Set.member "admin" principal.roles
