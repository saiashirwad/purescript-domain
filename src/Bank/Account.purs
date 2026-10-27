module Bank.Account (AccountId(..), AccountState(..), Account, Movement, account, customerOf) where

import Prelude

import Bank.Customer (CustomerId)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Variant (Variant)
import Domain (Aggregate, Decision, aggregate, emit, reject)

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

-- | Every label the account knows, declared once. The runtime, traces,
-- | codecs, HTTP routes, and smart constructors are all derived from it.
type Account =
  { id :: AccountId
  , state :: AccountState
  , command ::
      Variant
        ( openAccount :: { owner :: CustomerId }
        , depositMoney :: { amount :: Int }
        , withdrawMoney :: { amount :: Int }
        )
  , event ::
      Variant
        ( accountOpened :: { owner :: CustomerId }
        , moneyDeposited :: Movement
        , moneyWithdrawn :: Movement
        )
  , rejection :: Variant (alreadyOpen :: {}, notOpen :: {}, insufficientFunds :: Shortfall)
  , reply :: { balance :: Int }
  }

account :: Aggregate Account
account = aggregate
  { initial: Unopened
  , decide: { openAccount, depositMoney, withdrawMoney }
  , evolve:
      { accountOpened: \_ { owner } -> Open { owner, balance: 0 }
      , moneyDeposited: settle
      , moneyWithdrawn: settle
      }
  , reply: \state -> { balance: balanceOf state }
  }

openAccount ::
  AccountState ->
  { owner :: CustomerId } ->
  Decision (alreadyOpen :: {}) (accountOpened :: { owner :: CustomerId })
openAccount Unopened { owner } = emit @"accountOpened" { owner }
openAccount (Open _) _ = reject @"alreadyOpen" {}

depositMoney ::
  AccountState -> { amount :: Int } -> Decision (notOpen :: {}) (moneyDeposited :: Movement)
depositMoney Unopened _ = reject @"notOpen" {}
depositMoney (Open { balance }) { amount } =
  emit @"moneyDeposited" { amount, balanceAfter: balance + amount }

withdrawMoney ::
  AccountState ->
  { amount :: Int } ->
  Decision (notOpen :: {}, insufficientFunds :: Shortfall) (moneyWithdrawn :: Movement)
withdrawMoney Unopened _ = reject @"notOpen" {}
withdrawMoney (Open { balance }) { amount }
  | amount > balance = reject @"insufficientFunds" { balance, requested: amount }
  | otherwise = emit @"moneyWithdrawn" { amount, balanceAfter: balance - amount }

settle :: AccountState -> Movement -> AccountState
settle (Open open) { balanceAfter } = Open open { balance = balanceAfter }
settle Unopened _ = Unopened

balanceOf :: AccountState -> Int
balanceOf Unopened = 0
balanceOf (Open { balance }) = balance

customerOf :: AccountState -> Maybe CustomerId
customerOf Unopened = Nothing
customerOf (Open { owner }) = Just owner
