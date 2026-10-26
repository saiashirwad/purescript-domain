module Test.Projection (testProjection) where

import Prelude

import Bank.Account
  ( AccountEvent
  , AccountId(..)
  , depositMoney
  , openAccount
  , withdrawMoney
  )
import Bank.Customer
  ( CustomerEvent
  , CustomerId(..)
  , customerRegistered
  , customerStream
  )
import Bank.Policy (mainAccountProvisioning)
import Bank.Runtime
  ( accountHistory
  , newWithClock
  , sendAccount
  , statementFor
  , summaryFor
  )
import Bank.Views (statement, summary)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Variant (match)
import Domain
  ( Envelope
  , applyViews
  , commandsFrom
  , firstSequence
  , mkEnvelope
  , viewEmitted
  )
import Domain.Runtime.Projection (dedupe, replay)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Support (assertEqual, commandId, firstOf, fixedClock, fixedInstant)

accountId :: AccountId
accountId = AccountId "a-projection"

customerId :: CustomerId
customerId = CustomerId "c-projection"

testProjection :: Aff Unit
testProjection = do
  testRebuildSummary
  testRebuildStatement
  testDedupeProjection
  testPolicyAddressing

testRebuildSummary :: Aff Unit
testRebuildSummary = do
  runtime <- newWithClock fixedClock
  _ <- sendAccount runtime (commandId "open") accountId (openAccount "sam")
  _ <- sendAccount runtime (commandId "deposit") accountId (depositMoney 100)
  _ <- sendAccount runtime (commandId "withdraw") accountId (withdrawMoney 30)
  history <- accountHistory runtime accountId
  let rebuilt = applyViews summary Map.empty history
  live <- summaryFor runtime accountId
  assertEqual "rebuilt summary matches the live summary"
    (map _.state (Map.lookup accountId rebuilt))
    live

testRebuildStatement :: Aff Unit
testRebuildStatement = do
  runtime <- newWithClock fixedClock
  _ <- sendAccount runtime (commandId "open") accountId (openAccount "sam")
  _ <- sendAccount runtime (commandId "deposit") accountId (depositMoney 100)
  _ <- sendAccount runtime (commandId "withdraw") accountId (withdrawMoney 30)
  history <- accountHistory runtime accountId
  let rebuilt = applyViews statement Map.empty history
  live <- statementFor runtime accountId
  assertEqual "rebuilt statement matches the live statement"
    (map viewEmitted (Map.lookup accountId rebuilt))
    (Just live)

testDedupeProjection :: Aff Unit
testDedupeProjection = do
  runtime <- newWithClock fixedClock
  _ <- sendAccount runtime (commandId "open") accountId (openAccount "sam")
  _ <- sendAccount runtime (commandId "deposit") accountId (depositMoney 100)
  _ <- sendAccount runtime (commandId "withdraw") accountId (withdrawMoney 30)
  history <- accountHistory runtime accountId
  labels <- liftEffect (Ref.new [])
  let
    projection =
      { on: \envelope ->
          liftEffect
            ( Ref.modify_
                (\seen -> Array.snoc seen (eventLabel envelope.event))
                labels
            )
      }
  deduped <- dedupe projection
  replay deduped [ firstOf history, firstOf history ]
  afterDuplicate <- liftEffect (Ref.read labels)
  assertEqual "dedupe drops a repeated event"
    [ "accountOpened" ]
    afterDuplicate
  replay deduped history
  afterReplay <- liftEffect (Ref.read labels)
  assertEqual "dedupe still applies later events"
    [ "accountOpened", "moneyDeposited", "moneyWithdrawn" ]
    afterReplay

testPolicyAddressing :: Aff Unit
testPolicyAddressing = do
  let
    envelope = customerEnvelope (customerRegistered "sam")
    first = commandsFrom mainAccountProvisioning envelope
    second = commandsFrom mainAccountProvisioning envelope
  assertEqual "policy addressing is deterministic" first second
  assertEqual "policy targets the customer's main account"
    [ AccountId "c-projection-main" ]
    (map _.target first)

customerEnvelope :: CustomerEvent -> Envelope CustomerId CustomerEvent
customerEnvelope event =
  mkEnvelope customerId
    (customerStream customerId)
    firstSequence
    (commandId "register")
    fixedInstant
    event

eventLabel :: AccountEvent -> String
eventLabel = match
  { accountOpened: \_ -> "accountOpened"
  , moneyDeposited: \_ -> "moneyDeposited"
  , moneyWithdrawn: \_ -> "moneyWithdrawn"
  }
