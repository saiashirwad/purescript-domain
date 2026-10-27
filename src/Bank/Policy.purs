module Bank.Policy (provisioning, mainAccount) where

import Prelude

import Bank.Account (Account, AccountId(..))
import Bank.Customer (Customer, CustomerId(..))
import Domain (Policy, order, policy)

-- | Whenever a customer registers, open their main account. The runtime
-- | derives each order's command id from the triggering event, so a replay
-- | or a retry addresses the same command.
provisioning :: Policy Customer Account
provisioning = policy
  { customerRegistered: \customerId _ ->
      [ order @"openAccount" (mainAccount customerId) { owner: customerId } ]
  }

mainAccount :: CustomerId -> AccountId
mainAccount (CustomerId id) = AccountId (id <> "-main")
