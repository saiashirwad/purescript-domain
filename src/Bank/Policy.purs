module Bank.Policy
  ( mainAccountProvisioning
  ) where

import Prelude

import Bank.Account (AccountCommand, AccountId(..), openAccount)
import Bank.Customer (CustomerEvent, CustomerId, unCustomerId)
import Data.Variant (match)
import Domain (Policy)

-- | Whenever a customer registers, open their main account.
-- | The command is addressed by deriving the account id from the customer id,
-- | so replaying the policy produces the same command.
mainAccountProvisioning :: Policy CustomerId CustomerEvent AccountId AccountCommand
mainAccountProvisioning =
  { on: \envelope -> match
      { customerRegistered: \event ->
          [ { target: AccountId (unCustomerId envelope.id <> "-main")
            , command: openAccount event.name
            }
          ]
      }
      envelope.event
  }
