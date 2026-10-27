-- | Who may touch what. Routes, codecs, and status codes come from
-- | `Domain.Http.serve bank guards`.
module Bank.Api (Principal, guards) where

import Prelude

import Bank.Account (AccountId)
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

-- | No signature: `serve` checks each record against its aggregate's
-- | commands, and a signature here would restate them.
guards =
  { account: { openAccount: admin, depositMoney: ownsAccount, withdrawMoney: ownsAccount }
  , customer: { registerCustomer: admin }
  , summary: ownsAccount
  , statement: ownsAccount
  , dashboard: \principal id -> principal.customer == Just id || isAdmin principal
  }

ownsAccount :: Principal -> AccountId -> Boolean
ownsAccount principal id = Set.member id principal.accounts || isAdmin principal

admin :: forall id. Principal -> id -> Boolean
admin principal _ = isAdmin principal

isAdmin :: Principal -> Boolean
isAdmin principal = Set.member "admin" principal.roles
