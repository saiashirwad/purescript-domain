module Test.Stream (testStream) where

import Prelude

import Bank.Account (Account, AccountId(..), AccountState(..), account)
import Bank.Customer (CustomerId(..), customer)
import Bank.Policy (mainAccount, provisioning)
import Data.Array (concatMap)
import Data.Array as Array
import Domain
  (Command, cmd, commandIdText, orders, rejection, sequenceNumber, streamIdText, versionNumber)
import Domain.Id (streamOf)
import Domain.Stream (Fact, Stored, Stream, Turn(..), open, turn)
import Effect.Aff (Aff)
import Test.Support (assertEqual, commandId, epoch, fail)

testStream :: Aff Unit
testStream = do
  receipts
  sequences
  orderIds

type AccountStream = Stream AccountId AccountState Account { balance :: Int }

type AccountTurn = Turn AccountId AccountState Account { balance :: Int }

fresh :: String -> AccountStream
fresh raw = open account (streamOf "account" raw) (AccountId raw)

sendTo :: String -> Command Account -> AccountStream -> AccountTurn
sendTo key = turn account epoch (commandId key)

committed ::
  String ->
  AccountTurn ->
  Aff
    { stream :: AccountStream
    , stored :: Stored AccountId Account { balance :: Int }
    , facts :: Array (Fact AccountId AccountState Account)
    }
committed label = case _ of
  Committed result -> pure result
  Duplicate _ -> fail (label <> ": expected a commit, got a duplicate")
  Conflict _ -> fail (label <> ": expected a commit, got a conflict")
  Refused refused -> fail (label <> ": expected a commit, got " <> show refused)

openSam :: Command Account
openSam = cmd @"openAccount" { owner: CustomerId "sam" }

receipts :: Aff Unit
receipts = do
  opened <- committed "open" (sendTo "open" openSam (fresh "a-1"))
  case sendTo "open" openSam opened.stream of
    Duplicate stored ->
      assertEqual "a repeated id returns the stored receipt" (show opened.stored.receipt)
        (show stored.receipt)
    _ -> fail "a repeated id on one stream is a duplicate"

  case sendTo "overdraw" (cmd @"withdrawMoney" { amount: 5 }) opened.stream of
    Refused refused -> assertEqual "an overdraft is refused"
      (rejection @"insufficientFunds" { balance: 0, requested: 5 }) refused
    _ -> fail "an overdraft is refused"
  funded <- committed "fund" (sendTo "fund" (cmd @"depositMoney" { amount: 10 }) opened.stream)
  _ <- committed "a refused id is decided again"
    (sendTo "overdraw" (cmd @"withdrawMoney" { amount: 5 }) funded.stream)

  elsewhere <- committed "the same id on another stream" (sendTo "open" openSam (fresh "a-2"))
  assertEqual "the same id on another stream commits there" [ "account/a-2" ]
    (map (streamIdText <<< _.stream) elsewhere.stored.envelopes)

sequences :: Aff Unit
sequences = do
  opened <- committed "open" (sendTo "open" openSam (fresh "a-1"))
  deposited <-
    committed "deposit" (sendTo "deposit" (cmd @"depositMoney" { amount: 10 }) opened.stream)
  withdrawn <-
    committed "withdraw" (sendTo "withdraw" (cmd @"withdrawMoney" { amount: 4 }) deposited.stream)
  assertEqual "each commit appends after the stream's version" [ [ 1 ], [ 2 ], [ 3 ] ]
    (map (map (sequenceNumber <<< _.seq) <<< _.stored.envelopes) [ opened, deposited, withdrawn ])
  assertEqual "the receipt's version is the last sequence" [ 1, 2, 3 ]
    (map (versionNumber <<< _.stored.receipt.version) [ opened, deposited, withdrawn ])
  assertEqual "history holds every envelope in order" [ 1, 2, 3 ]
    (map (sequenceNumber <<< _.seq) (Array.fromFoldable withdrawn.stream.history))
  assertEqual "a fact carries the state right after its event"
    [ Open { owner: CustomerId "sam", balance: 6 } ] (map _.state withdrawn.facts)

orderIds :: Aff Unit
orderIds = do
  let
    ada = CustomerId "c-1"
    register = cmd @"registerCustomer" { name: "ada" }
  registered <-
    case
      turn customer epoch (commandId "register") register
        (open customer (streamOf "customer" "c-1") ada)
    of
      Committed result -> pure result
      _ -> fail "register commits"
  let
    placed = concatMap (orders "provisioning" provisioning) registered.stored.envelopes
  assertEqual "an order's id names the policy, the source stream, the sequence, and the index"
    [ "provisioning:customer/c-1@1#0" ] (map (commandIdText <<< _.commandId) placed)
  assertEqual "the order targets the customer's main account" [ mainAccount ada ] (map _.to placed)
  case turn customer epoch (commandId "register") register registered.stream of
    Duplicate stored -> assertEqual "a duplicate re-derives the same order ids"
      (map (commandIdText <<< _.commandId) placed)
      ( map (commandIdText <<< _.commandId)
          (concatMap (orders "provisioning" provisioning) stored.envelopes)
      )
    _ -> fail "a repeated register is a duplicate"
  assertEqual "another policy reacting to the same event gets other ids"
    [ "welcome:customer/c-1@1#0" ]
    ( map (commandIdText <<< _.commandId)
        (concatMap (orders "welcome" provisioning) registered.stored.envelopes)
    )
