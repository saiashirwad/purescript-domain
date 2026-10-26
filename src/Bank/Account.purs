module Bank.Account
  ( AccountId(..)
  , AccountState(..)
  , AccountEventRow
  , AccountEvent
  , AccountRejectionRow
  , AccountRejection
  , AccountCommandRow
  , AccountCommand
  , accountStream
  , accountDecider
  , openAccount
  , depositMoney
  , withdrawMoney
  , accountOpened
  , moneyDeposited
  , moneyWithdrawn
  , alreadyOpen
  , notOpen
  , insufficientFunds
  , balanceOf
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Variant (Variant, expand, inj, match)
import Domain (Decider)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

newtype AccountId = AccountId String

derive newtype instance eqAccountId :: Eq AccountId
derive newtype instance ordAccountId :: Ord AccountId
derive newtype instance showAccountId :: Show AccountId

-- | The lifecycle is explicit. An `Unopened` account has no balance to read.
data AccountState
  = Unopened
  | Open { owner :: String, balance :: Int }

instance eqAccountState :: Eq AccountState where
  eq Unopened Unopened = true
  eq (Open a) (Open b) = a.owner == b.owner && a.balance == b.balance
  eq _ _ = false

instance showAccountState :: Show AccountState where
  show Unopened = "Unopened"
  show (Open a) =
    "Open { owner: " <> show a.owner <> ", balance: " <> show a.balance <> " }"

-- | The event row is the single source of truth for what the account records.
-- | `evolve` must handle every label; adding one without a handler is a type error.
type AccountEventRow =
  ( accountOpened :: { owner :: String }
  , moneyDeposited :: { amount :: Int, balanceAfter :: Int }
  , moneyWithdrawn :: { amount :: Int, balanceAfter :: Int }
  )

type AccountEvent = Variant AccountEventRow

type AccountRejectionRow =
  ( alreadyOpen :: {}
  , notOpen :: {}
  , insufficientFunds :: { balance :: Int, requested :: Int }
  )

type AccountRejection = Variant AccountRejectionRow

type AccountCommandRow =
  ( openAccount :: { owner :: String }
  , depositMoney :: { amount :: Int }
  , withdrawMoney :: { amount :: Int }
  )

type AccountCommand = Variant AccountCommandRow

accountStream :: AccountId -> String
accountStream (AccountId id) = "Account/" <> id

openAccount :: String -> AccountCommand
openAccount owner = inj (Proxy :: Proxy "openAccount") { owner }

depositMoney :: Int -> AccountCommand
depositMoney amount = inj (Proxy :: Proxy "depositMoney") { amount }

withdrawMoney :: Int -> AccountCommand
withdrawMoney amount = inj (Proxy :: Proxy "withdrawMoney") { amount }

accountOpened :: String -> AccountEvent
accountOpened owner = inj (Proxy :: Proxy "accountOpened") { owner }

moneyDeposited :: Int -> Int -> AccountEvent
moneyDeposited amount balanceAfter =
  inj (Proxy :: Proxy "moneyDeposited") { amount, balanceAfter }

moneyWithdrawn :: Int -> Int -> AccountEvent
moneyWithdrawn amount balanceAfter =
  inj (Proxy :: Proxy "moneyWithdrawn") { amount, balanceAfter }

alreadyOpen :: AccountRejection
alreadyOpen = inj (Proxy :: Proxy "alreadyOpen") {}

notOpen :: AccountRejection
notOpen = inj (Proxy :: Proxy "notOpen") {}

insufficientFunds :: Int -> Int -> AccountRejection
insufficientFunds balance requested =
  inj (Proxy :: Proxy "insufficientFunds") { balance, requested }

balanceOf :: AccountState -> Int
balanceOf Unopened = 0
balanceOf (Open account) = account.balance

-- | Each handler names the exact events it may record and the exact
-- | rejections it may return. `expand` embeds that narrow row into the
-- | aggregate's full row, so a deposit cannot accidentally return
-- | `InsufficientFunds` and an `OpenAccount` cannot record `MoneyDeposited`.
decide :: AccountState -> AccountCommand -> Either AccountRejection (Array AccountEvent)
decide state = match
  { openAccount: \input ->
      openAccountHandler state input
        # lmap expand
        # map (map expand)
  , depositMoney: \input ->
      depositMoneyHandler state input
        # lmap expand
        # map (map expand)
  , withdrawMoney: \input ->
      withdrawMoneyHandler state input
        # lmap expand
        # map (map expand)
  }

openAccountHandler
  :: AccountState
  -> { owner :: String }
  -> Either
       (Variant (alreadyOpen :: {}))
       (Array (Variant (accountOpened :: { owner :: String })))
openAccountHandler state input = case state of
  Unopened ->
    Right [ inj (Proxy :: Proxy "accountOpened") { owner: input.owner } ]
  Open _ -> Left (inj (Proxy :: Proxy "alreadyOpen") {})

depositMoneyHandler
  :: AccountState
  -> { amount :: Int }
  -> Either
       (Variant (notOpen :: {}))
       (Array (Variant (moneyDeposited :: { amount :: Int, balanceAfter :: Int })))
depositMoneyHandler state input = case state of
  Unopened -> Left (inj (Proxy :: Proxy "notOpen") {})
  Open account ->
    Right
      [ inj (Proxy :: Proxy "moneyDeposited")
          { amount: input.amount, balanceAfter: account.balance + input.amount }
      ]

withdrawMoneyHandler
  :: AccountState
  -> { amount :: Int }
  -> Either
       (Variant (notOpen :: {}, insufficientFunds :: { balance :: Int, requested :: Int }))
       (Array (Variant (moneyWithdrawn :: { amount :: Int, balanceAfter :: Int })))
withdrawMoneyHandler state input = case state of
  Unopened -> Left (inj (Proxy :: Proxy "notOpen") {})
  Open account
    | input.amount > account.balance ->
        Left
          ( inj (Proxy :: Proxy "insufficientFunds")
              { balance: account.balance, requested: input.amount }
          )
    | otherwise ->
        Right
          [ inj (Proxy :: Proxy "moneyWithdrawn")
              { amount: input.amount, balanceAfter: account.balance - input.amount }
          ]

evolve :: AccountState -> AccountEvent -> AccountState
evolve state = match
  { accountOpened: \event -> Open { owner: event.owner, balance: 0 }
  , moneyDeposited: \event -> case state of
      Open account -> Open (account { balance = event.balanceAfter })
      Unopened ->
        unsafeCrashWith "Account.evolve: MoneyDeposited on an unopened account"
  , moneyWithdrawn: \event -> case state of
      Open account -> Open (account { balance = event.balanceAfter })
      Unopened ->
        unsafeCrashWith "Account.evolve: MoneyWithdrawn on an unopened account"
  }

reply :: AccountState -> AccountCommand -> { balance :: Int }
reply state = match
  { openAccount: \_ -> { balance: balanceOf state }
  , depositMoney: \_ -> { balance: balanceOf state }
  , withdrawMoney: \_ -> { balance: balanceOf state }
  }

accountDecider
  :: Decider AccountState AccountCommand AccountEvent AccountRejection { balance :: Int }
accountDecider =
  { initial: Unopened
  , decide
  , evolve
  , reply
  }
