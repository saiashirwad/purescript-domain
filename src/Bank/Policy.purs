module Bank.Policy (provisioning, mainAccount) where

import Prelude

import Bank.Account (AccountCommand(..), AccountId(..))
import Bank.Customer (CustomerChange, CustomerEvent(..), CustomerId(..))
import Domain (Policy(..))

-- | Whenever a customer registers, open their main account. The runtime
-- | derives each order's command id from the triggering event, so a replay
-- | or a retry addresses the same command.
provisioning :: Policy CustomerChange AccountId AccountCommand
provisioning = Policy \{ id, event } -> case event of
  CustomerRegistered _ -> [ { to: mainAccount id, command: OpenAccount { owner: id } } ]

mainAccount :: CustomerId -> AccountId
mainAccount (CustomerId id) = AccountId (id <> "-main")
