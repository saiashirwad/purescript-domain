module Test.Trace (testTrace) where

import Prelude

import Bank.Account (AccountId(..), alreadyOpen, openAccount)
import Bank.Customer (CustomerId(..), registerCustomer)
import Bank.Runtime
  ( accountHistory
  , newWithCapabilities
  , sendAccount
  , sendCustomer
  )
import Data.Array (length)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Domain.Runtime.Trace (collect, recorded)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Test.Support (assertEqual, commandId, fixedClock)

accountId :: AccountId
accountId = AccountId "a-trace"

customerId :: CustomerId
customerId = CustomerId "c-trace"

testTrace :: Aff Unit
testTrace = do
  testCommittedCommandSpan
  testRejectedCommandSpan
  testPolicySpan

testCommittedCommandSpan :: Aff Unit
testCommittedCommandSpan = do
  { tracer, spans } <- collect
  runtime <-
    newWithCapabilities
      { clock: fixedClock
      , tracer
      }
  _ <- sendAccount runtime (commandId "cmd-open") accountId (openAccount "sam")
  recordedSpans <- recorded spans
  assertEqual "one command span" 1 (length recordedSpans)
  case recordedSpans of
    [ span ] -> do
      assertEqual "span name" "command.account" span.name
      assertEqual "command id" (Just "cmd-open")
        (Map.lookup "command.id" span.attributes)
      assertEqual "stream id" (Just "Account/a-trace")
        (Map.lookup "stream.id" span.attributes)
      assertEqual "command name" (Just "openAccount")
        (Map.lookup "command.name" span.attributes)
      assertEqual "outcome" (Just "committed")
        (Map.lookup "outcome" span.attributes)
      assertEqual "version" (Just "1")
        (Map.lookup "version" span.attributes)
      assertEqual "events" (Just "accountOpened")
        (Map.lookup "events" span.attributes)
      assertEqual "fixed clock makes duration zero" (Milliseconds 0.0)
        span.duration
      history <- accountHistory runtime accountId
      assertEqual "the traced command really committed" 1 (length history)
    _ ->
      throwError (error "expected exactly one span")

testRejectedCommandSpan :: Aff Unit
testRejectedCommandSpan = do
  { tracer, spans } <- collect
  runtime <-
    newWithCapabilities
      { clock: fixedClock
      , tracer
      }
  _ <- sendAccount runtime (commandId "cmd-open") accountId (openAccount "sam")
  rejection <-
    sendAccount runtime (commandId "cmd-open-again") accountId
      (openAccount "sam")
  case rejection of
    Left observed -> assertEqual "rejection is the domain rejection" alreadyOpen observed
    Right _ -> throwError (error "expected the second open to be rejected")
  recordedSpans <- recorded spans
  assertEqual "two spans" 2 (length recordedSpans)
  case recordedSpans of
    [ _, span ] -> do
      assertEqual "rejected outcome" (Just "rejected")
        (Map.lookup "outcome" span.attributes)
      assertEqual "rejection is recorded" (Just "alreadyOpen")
        (Map.lookup "rejection" span.attributes)
    _ ->
      throwError (error "expected two spans")

testPolicySpan :: Aff Unit
testPolicySpan = do
  { tracer, spans } <- collect
  runtime <-
    newWithCapabilities
      { clock: fixedClock
      , tracer
      }
  _ <- sendCustomer runtime (commandId "cmd-register") customerId
    (registerCustomer "sam")
  recordedSpans <- recorded spans
  assertEqual "policy emits a customer span and an account span"
    [ "command.account", "command.customer" ]
    (map _.name recordedSpans)
  case recordedSpans of
    [ _, customerSpan ] ->
      assertEqual "policy command count" (Just "1")
        (Map.lookup "policy.commands" customerSpan.attributes)
    _ ->
      throwError (error "expected two policy spans")
