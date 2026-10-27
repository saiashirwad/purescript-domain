module Bank.Customer
  ( CustomerId(..)
  , CustomerState(..)
  , CustomerCommand(..)
  , CustomerEvent(..)
  , CustomerRejection(..)
  , CustomerChange
  , Customer
  , customer
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Domain (Aggregate(..), Change)

newtype CustomerId = CustomerId String

derive instance Newtype CustomerId _
derive newtype instance Eq CustomerId
derive newtype instance Ord CustomerId
derive newtype instance Show CustomerId
derive newtype instance EncodeJson CustomerId
derive newtype instance DecodeJson CustomerId

data CustomerState = Unregistered | Registered { name :: String }

derive instance Eq CustomerState
derive instance Generic CustomerState _
instance Show CustomerState where
  show = genericShow

data CustomerCommand = RegisterCustomer { name :: String }

derive instance Eq CustomerCommand
derive instance Generic CustomerCommand _
instance Show CustomerCommand where
  show = genericShow

data CustomerEvent = CustomerRegistered { name :: String }

derive instance Eq CustomerEvent
derive instance Generic CustomerEvent _
instance Show CustomerEvent where
  show = genericShow

data CustomerRejection = AlreadyRegistered

derive instance Eq CustomerRejection
derive instance Generic CustomerRejection _
instance Show CustomerRejection where
  show = genericShow

type Customer = Aggregate CustomerId CustomerCommand CustomerEvent CustomerRejection CustomerState Unit

type CustomerChange = Change CustomerId CustomerState CustomerEvent

customer :: Customer
customer = Aggregate
  { initial: Unregistered
  , decide: case _, _ of
      RegisterCustomer { name }, Unregistered -> Right [ CustomerRegistered { name } ]
      RegisterCustomer _, Registered _ -> Left AlreadyRegistered
  , evolve: \_ (CustomerRegistered { name }) -> Registered { name }
  , reply: \_ -> unit
  }
