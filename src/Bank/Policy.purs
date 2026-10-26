module Bank.Policy
  ( mainAccountProvisioning
  ) where

import Prelude

import Bank.Account (AccountCommand, openAccount)
import Bank.Customer (CustomerEvent)
import Data.Variant (match)
import Domain (Policy)

-- | Whenever a customer registers, open their main account.
-- | The command is addressed by deriving the account id from the customer id,
-- | so replaying the policy produces the same command.
mainAccountProvisioning :: Policy CustomerEvent AccountCommand
mainAccountProvisioning =
  { on: \envelope -> match
      { customerRegistered: \event ->
          [ { target: envelope.id <> "-main"
            , command: openAccount event.name
            }
          ]
      }
      envelope.event
  }
