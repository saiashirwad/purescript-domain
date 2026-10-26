module Bank.Trace
  ( accountCommandName
  , accountEventName
  , accountRejectionName
  , customerCommandName
  , customerEventName
  , customerRejectionName
  ) where

import Bank.Account (AccountCommand, AccountEvent, AccountRejection)
import Bank.Customer (CustomerCommand, CustomerEvent, CustomerRejection)
import Data.Variant (match)

accountCommandName :: AccountCommand -> String
accountCommandName = match
  { openAccount: \_ -> "openAccount"
  , depositMoney: \_ -> "depositMoney"
  , withdrawMoney: \_ -> "withdrawMoney"
  }

accountEventName :: AccountEvent -> String
accountEventName = match
  { accountOpened: \_ -> "accountOpened"
  , moneyDeposited: \_ -> "moneyDeposited"
  , moneyWithdrawn: \_ -> "moneyWithdrawn"
  }

accountRejectionName :: AccountRejection -> String
accountRejectionName = match
  { alreadyOpen: \_ -> "alreadyOpen"
  , notOpen: \_ -> "notOpen"
  , insufficientFunds: \_ -> "insufficientFunds"
  }

customerCommandName :: CustomerCommand -> String
customerCommandName = match
  { registerCustomer: \_ -> "registerCustomer"
  }

customerEventName :: CustomerEvent -> String
customerEventName = match
  { customerRegistered: \_ -> "customerRegistered"
  }

customerRejectionName :: CustomerRejection -> String
customerRejectionName = match
  { alreadyRegistered: \_ -> "alreadyRegistered"
  }
