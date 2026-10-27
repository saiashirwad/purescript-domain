module Bank.Account
  ( AccountId(..)
  , AccountState(..)
  , AccountCommand(..)
  , AccountEvent(..)
  , AccountRejection(..)
  , AccountChange
  , Account
  , Balance
  , Movement
  , Shortfall
  , account
  , customerOf
  ) where

import Prelude

import Bank.Customer (CustomerId)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Domain (Aggregate(..), Change)

newtype AccountId = AccountId String

derive instance Newtype AccountId _
derive newtype instance Eq AccountId
derive newtype instance Ord AccountId
derive newtype instance Show AccountId

data AccountState = Unopened | Open { owner :: CustomerId, balance :: Int }

derive instance Eq AccountState
derive instance Generic AccountState _
instance Show AccountState where
  show = genericShow

type Movement = { amount :: Int, balanceAfter :: Int }

type Shortfall = { balance :: Int, requested :: Int }

type Balance = { balance :: Int }

data AccountCommand
  = OpenAccount { owner :: CustomerId }
  | DepositMoney { amount :: Int }
  | WithdrawMoney { amount :: Int }

derive instance Eq AccountCommand
derive instance Generic AccountCommand _
instance Show AccountCommand where
  show = genericShow

data AccountEvent
  = AccountOpened { owner :: CustomerId }
  | MoneyDeposited Movement
  | MoneyWithdrawn Movement

derive instance Eq AccountEvent
derive instance Generic AccountEvent _
instance Show AccountEvent where
  show = genericShow

data AccountRejection
  = AlreadyOpen
  | NotOpen
  | NonPositiveAmount { amount :: Int }
  | InsufficientFunds Shortfall

derive instance Eq AccountRejection
derive instance Generic AccountRejection _
instance Show AccountRejection where
  show = genericShow

type Account = Aggregate AccountId AccountCommand AccountEvent AccountRejection AccountState Balance

-- | What views and policies over accounts read.
type AccountChange = Change AccountId AccountState AccountEvent

account :: Account
account = Aggregate
  { initial: Unopened
  , decide
  , evolve
  , reply: \state -> { balance: balanceOf state }
  }

decide :: AccountCommand -> AccountState -> Either AccountRejection (Array AccountEvent)
decide = case _, _ of
  OpenAccount { owner }, Unopened -> Right [ AccountOpened { owner } ]
  OpenAccount _, Open _ -> Left AlreadyOpen
  _, Unopened -> Left NotOpen
  DepositMoney { amount }, Open { balance }
    | amount <= 0 -> Left (NonPositiveAmount { amount })
    | otherwise -> Right [ MoneyDeposited { amount, balanceAfter: balance + amount } ]
  WithdrawMoney { amount }, Open { balance }
    | amount <= 0 -> Left (NonPositiveAmount { amount })
    | amount > balance -> Left (InsufficientFunds { balance, requested: amount })
    | otherwise -> Right [ MoneyWithdrawn { amount, balanceAfter: balance - amount } ]

evolve :: AccountState -> AccountEvent -> AccountState
evolve state = case _ of
  AccountOpened { owner } -> Open { owner, balance: 0 }
  MoneyDeposited movement -> settle movement
  MoneyWithdrawn movement -> settle movement
  where
  settle { balanceAfter } = case state of
    Open open -> Open open { balance = balanceAfter }
    Unopened -> Unopened

balanceOf :: AccountState -> Int
balanceOf Unopened = 0
balanceOf (Open { balance }) = balance

customerOf :: AccountState -> Maybe CustomerId
customerOf Unopened = Nothing
customerOf (Open { owner }) = Just owner
