module Bank.Customer
  ( CustomerId(..)
  , CustomerState(..)
  , Customer
  , customer
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Variant (Variant)
import Domain (Aggregate, Decision, aggregate, emit, reject)

newtype CustomerId = CustomerId String

derive instance Newtype CustomerId _
derive newtype instance Eq CustomerId
derive newtype instance Ord CustomerId
derive newtype instance Show CustomerId

data CustomerState
  = Unregistered
  | Registered { name :: String }

derive instance Eq CustomerState
derive instance Generic CustomerState _
instance Show CustomerState where
  show = genericShow

type Customer =
  { id :: CustomerId
  , state :: CustomerState
  , command :: Variant (registerCustomer :: { name :: String })
  , event :: Variant (customerRegistered :: { name :: String })
  , rejection :: Variant (alreadyRegistered :: {})
  , reply :: Unit
  }

customer :: Aggregate Customer
customer = aggregate
  { initial: Unregistered
  , decide: { registerCustomer }
  , evolve: { customerRegistered: \_ { name } -> Registered { name } }
  , reply: \_ -> unit
  }

registerCustomer
  :: CustomerState
  -> { name :: String }
  -> Decision (alreadyRegistered :: {}) (customerRegistered :: { name :: String })
registerCustomer Unregistered { name } = emit @"customerRegistered" { name }
registerCustomer (Registered _) _ = reject @"alreadyRegistered" {}
