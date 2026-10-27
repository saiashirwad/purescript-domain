module Test.Trace (testTrace) where

import Prelude

import Bank (bank)
import Bank.Account (AccountCommand(..), AccountId(..))
import Bank.Customer (CustomerCommand(..), CustomerId(..))
import Data.Array (index)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Domain.Runtime (Span, start)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Support (assertEqual, commandId, fail, failingOnce, recordingOptions)

testTrace :: Aff Unit
testTrace = do
  outcomes
  failures

outcomes :: Aff Unit
outcomes = do
  { options, spans } <- recordingOptions
  rt <- start bank options
  let
    sam = AccountId "a-1"
  _ <- rt.account.send (commandId "open") sam (OpenAccount { owner: CustomerId "sam" })
  _ <- rt.account.send (commandId "overdraw") sam (WithdrawMoney { amount: 5 })
  _ <- rt.account.send (commandId "open") sam (OpenAccount { owner: CustomerId "sam" })
  _ <- rt.account.send (commandId "open") sam (DepositMoney { amount: 5 })
  _ <- rt.customer.send (commandId "register") (CustomerId "c-1")
    (RegisterCustomer { name: "ada" })
  rt.stop
  recorded <- liftEffect (Ref.read spans)
  assertEqual "a span per command, the policy's order before the command that caused it"
    [ "command.account"
    , "command.account"
    , "command.account"
    , "command.account"
    , "command.account"
    , "command.customer"
    ]
    (map _.name recorded)
  committed <- spanAt 0 recorded
  assertEqual "a committed command's attributes"
    ( Map.fromFoldable
        [ Tuple "command.id" "open"
        , Tuple "command.name" "openAccount"
        , Tuple "stream.id" "account/a-1"
        , Tuple "outcome" "committed"
        , Tuple "version" "1"
        , Tuple "events" "accountOpened"
        , Tuple "policy.orders" "0"
        ]
    )
    committed.attributes
  rejected <- spanAt 1 recorded
  assertEqual "a rejected command's attributes"
    ( Map.fromFoldable
        [ Tuple "command.id" "overdraw"
        , Tuple "command.name" "withdrawMoney"
        , Tuple "stream.id" "account/a-1"
        , Tuple "outcome" "rejected"
        , Tuple "rejection" "insufficientFunds"
        , Tuple "policy.orders" "0"
        ]
    )
    rejected.attributes
  duplicate <- spanAt 2 recorded
  assertEqual "a retried command is marked a duplicate" (Just "duplicate")
    (Map.lookup "outcome" duplicate.attributes)
  reused <- spanAt 3 recorded
  assertEqual "a reused id's span names the new command and the one the id already names"
    ( Map.fromFoldable
        [ Tuple "command.id" "open"
        , Tuple "command.name" "depositMoney"
        , Tuple "stream.id" "account/a-1"
        , Tuple "outcome" "reused"
        , Tuple "stored.command" "openAccount"
        , Tuple "policy.orders" "0"
        ]
    )
    reused.attributes
  registered <- spanAt 5 recorded
  assertEqual "a policy's orders are counted on the triggering span" (Just "1")
    (Map.lookup "policy.orders" registered.attributes)
  where
  spanAt :: Int -> Array Span -> Aff Span
  spanAt n spans = case index spans n of
    Just span -> pure span
    Nothing -> fail ("no span at index " <> show n)

-- | A command that fails still emits its span, with the failure's message,
-- | whether it fails in its turn or because the runtime is stopped.
failures :: Aff Unit
failures = do
  { options, spans } <- recordingOptions
  failing <- failingOnce "account/a-1" "the stream is down"
  rt <- start bank options { beforeTurn = failing }
  let
    sam = AccountId "a-1"
  _ <- attempt
    (rt.account.send (commandId "open") sam (OpenAccount { owner: CustomerId "sam" }))
  rt.stop
  _ <- attempt (rt.account.send (commandId "late") sam (DepositMoney { amount: 1 }))
  recorded <- liftEffect (Ref.read spans)
  assertEqual "each failed command's span carries its failure"
    [ Map.fromFoldable
        [ Tuple "command.id" "open"
        , Tuple "command.name" "openAccount"
        , Tuple "stream.id" "account/a-1"
        , Tuple "outcome" "failed"
        , Tuple "error" "FAIL the stream is down"
        ]
    , Map.fromFoldable
        [ Tuple "command.id" "late"
        , Tuple "command.name" "depositMoney"
        , Tuple "stream.id" "account/a-1"
        , Tuple "outcome" "failed"
        , Tuple "error" "Domain.Runtime: the runtime is stopped"
        ]
    ]
    (map _.attributes recorded)
