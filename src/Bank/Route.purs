module Bank.Route
  ( BankRoute(..)
  , route
  , parseRoute
  , printRoute
  , methodOf
  , routeName
  ) where

import Prelude

import Bank.Account (AccountId(..))
import Bank.Customer (CustomerId(..))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Profunctor (dimap)
import Domain.Http (Method(..))
import Routing.Duplex (RouteDuplex', parse, prefix, print, root, segment)
import Routing.Duplex.Generic (sum)

-- | The addressable surface of the bank. The route is the type; the handler
-- | record in `Bank.Api` is keyed by these constructors.
data BankRoute
  = OpenAccount AccountId
  | DepositMoney AccountId
  | WithdrawMoney AccountId
  | AccountSummary AccountId
  | AccountStatement AccountId
  | RegisterCustomer CustomerId

derive instance genericBankRoute :: Generic BankRoute _
derive instance eqBankRoute :: Eq BankRoute

instance showBankRoute :: Show BankRoute where
  show = case _ of
    OpenAccount id -> "OpenAccount " <> show id
    DepositMoney id -> "DepositMoney " <> show id
    WithdrawMoney id -> "WithdrawMoney " <> show id
    AccountSummary id -> "AccountSummary " <> show id
    AccountStatement id -> "AccountStatement " <> show id
    RegisterCustomer id -> "RegisterCustomer " <> show id

accountId :: RouteDuplex' AccountId
accountId = dimap (\(AccountId id) -> id) AccountId segment

customerId :: RouteDuplex' CustomerId
customerId = dimap (\(CustomerId id) -> id) CustomerId segment

route :: RouteDuplex' BankRoute
route =
  root $ sum
    { "OpenAccount": prefix "accounts" $ prefix "open" accountId
    , "DepositMoney": prefix "accounts" $ prefix "deposit" accountId
    , "WithdrawMoney": prefix "accounts" $ prefix "withdraw" accountId
    , "AccountSummary": prefix "accounts" $ prefix "summary" accountId
    , "AccountStatement": prefix "accounts" $ prefix "statement" accountId
    , "RegisterCustomer": prefix "customers" $ prefix "register" customerId
    }

parseRoute :: String -> Either String BankRoute
parseRoute = lmap show <<< parse route

printRoute :: BankRoute -> String
printRoute = print route

methodOf :: BankRoute -> Method
methodOf = case _ of
  OpenAccount _ -> POST
  DepositMoney _ -> POST
  WithdrawMoney _ -> POST
  AccountSummary _ -> GET
  AccountStatement _ -> GET
  RegisterCustomer _ -> POST

routeName :: BankRoute -> String
routeName = case _ of
  OpenAccount _ -> "openAccount"
  DepositMoney _ -> "depositMoney"
  WithdrawMoney _ -> "withdrawMoney"
  AccountSummary _ -> "accountSummary"
  AccountStatement _ -> "accountStatement"
  RegisterCustomer _ -> "registerCustomer"
