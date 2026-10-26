module Test.Support
  ( accountId
  , customerId
  , fixedInstant
  , fixedClock
  , commandId
  , versionOne
  , assertEqual
  , expectRight
  , expectRightAff
  , expectLeft
  , expectLeftAff
  , firstOf
  , newRuntime
  ) where

import Prelude

import Bank.Account (AccountId(..))
import Bank.Customer (CustomerId(..))
import Bank.Runtime (BankRuntime, newWithClock)
import Data.Array as Array
import Data.DateTime.Instant (Instant, instant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Domain (CommandId, Version, parseCommandId, parseVersion)
import Domain.Runtime.InMemory (Clock)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Partial.Unsafe (unsafeCrashWith)

accountId :: AccountId
accountId = AccountId "a-1"

customerId :: CustomerId
customerId = CustomerId "c-1"

fixedInstant :: Instant
fixedInstant =
  case instant (Milliseconds 0.0) of
    Just value -> value
    Nothing -> unsafeCrashWith "test instant out of range"

fixedClock :: Clock
fixedClock = { now: pure fixedInstant }

newRuntime :: Aff BankRuntime
newRuntime = newWithClock fixedClock

commandId :: String -> CommandId
commandId value =
  case parseCommandId value of
    Right identifier -> identifier
    Left failure -> unsafeCrashWith ("invalid test command id: " <> show failure)

versionOne :: Version
versionOne =
  case parseVersion 1 of
    Right value -> value
    Left failure -> unsafeCrashWith ("invalid test version: " <> show failure)

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Aff Unit
assertEqual label expected actual =
  if expected == actual then pure unit
  else
    throwError
      ( error
          ( "FAIL " <> label
              <> "\n  expected: " <> show expected
              <> "\n  actual:   " <> show actual
          )
      )

expectRight :: forall a b. Show a => Either a b -> Aff b
expectRight = case _ of
  Right value -> pure value
  Left failure ->
    throwError (error ("expected Right, got Left: " <> show failure))

expectRightAff :: forall a b. Show a => Aff (Either a b) -> Aff b
expectRightAff action = action >>= expectRight

expectLeft :: forall a b. Show b => Either a b -> Aff a
expectLeft = case _ of
  Left failure -> pure failure
  Right value ->
    throwError (error ("expected Left, got Right: " <> show value))

expectLeftAff :: forall a b. Show b => Aff (Either a b) -> Aff a
expectLeftAff action = action >>= expectLeft

firstOf :: forall a. Array a -> a
firstOf values =
  case Array.head values of
    Just value -> value
    Nothing -> unsafeCrashWith "expected a non-empty array"
