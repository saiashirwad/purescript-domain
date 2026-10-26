module Bank.Customer
  ( CustomerId(..)
  , CustomerState(..)
  , CustomerEventRow
  , CustomerEvent
  , CustomerRejectionRow
  , CustomerRejection
  , CustomerCommandRow
  , CustomerCommand
  , customerStream
  , unCustomerId
  , customerDecider
  , registerCustomer
  , customerRegistered
  , alreadyRegistered
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant, inj, match)
import Domain (Decider)
import Domain.Id.Internal (StreamId(..))
import Type.Proxy (Proxy(..))

newtype CustomerId = CustomerId String

derive newtype instance eqCustomerId :: Eq CustomerId
derive newtype instance ordCustomerId :: Ord CustomerId
derive newtype instance showCustomerId :: Show CustomerId

data CustomerState
  = Unregistered
  | Registered { name :: String }

instance eqCustomerState :: Eq CustomerState where
  eq Unregistered Unregistered = true
  eq (Registered a) (Registered b) = a.name == b.name
  eq _ _ = false

instance showCustomerState :: Show CustomerState where
  show Unregistered = "Unregistered"
  show (Registered a) = "Registered { name: " <> show a.name <> " }"

type CustomerEventRow =
  ( customerRegistered :: { name :: String } )

type CustomerEvent = Variant CustomerEventRow

type CustomerRejectionRow =
  ( alreadyRegistered :: {} )

type CustomerRejection = Variant CustomerRejectionRow

type CustomerCommandRow =
  ( registerCustomer :: { name :: String } )

type CustomerCommand = Variant CustomerCommandRow

customerStream :: CustomerId -> StreamId
customerStream (CustomerId id) = StreamId ("Customer/" <> id)

unCustomerId :: CustomerId -> String
unCustomerId (CustomerId id) = id

registerCustomer :: String -> CustomerCommand
registerCustomer name = inj (Proxy :: Proxy "registerCustomer") { name }

customerRegistered :: String -> CustomerEvent
customerRegistered name = inj (Proxy :: Proxy "customerRegistered") { name }

alreadyRegistered :: CustomerRejection
alreadyRegistered = inj (Proxy :: Proxy "alreadyRegistered") {}

customerDecider
  :: Decider CustomerState CustomerCommand CustomerEvent CustomerRejection Unit
customerDecider =
  { initial: Unregistered
  , decide: \state -> match
      { registerCustomer: \input -> case state of
          Unregistered -> Right [ customerRegistered input.name ]
          Registered _ -> Left alreadyRegistered
      }
  , evolve: \_ -> match
      { customerRegistered: \event -> Registered { name: event.name } }
  , reply: \_ _ -> unit
  }
