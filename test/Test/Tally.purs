-- | Runtime guarantees the bank cannot show: its commands happen to be
-- | idempotent and its labels are ordinary words. A tally counts every
-- | bump, so a command that runs twice shows up as a count of two.
module Test.Tally (testTally) where

import Prelude

import Bank.Customer (Customer, CustomerId(..), customer)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Domain
  ( Aggregate
  , Decision
  , Policy
  , View
  , aggregate
  , cmd
  , emit
  , order
  , parseCommandId
  , policy
  , streamIdText
  , view
  )
import Domain.Runtime (start)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Test.Support (assertEqual, assertThat, commandId, expectRight, fail, isLeft, testOptions)

newtype TallyId = TallyId String

derive instance Newtype TallyId _
derive newtype instance Eq TallyId
derive newtype instance Ord TallyId
derive newtype instance Show TallyId

-- | `toString`, `valueOf`, and `__proto__` are also names on every JavaScript
-- | object.
type Tally =
  { id :: TallyId
  , state :: Int
  , command :: Variant (bump :: {}, toString :: {}, "__proto__" :: {})
  , event :: Variant (bumped :: {}, valueOf :: {})
  , rejection :: Variant ()
  , reply :: Int
  }

tally :: Aggregate Tally
tally = aggregate
  { initial: 0
  , decide: { bump, toString, "__proto__": proto }
  , evolve: { bumped: \n _ -> n + 1, valueOf: \n _ -> n }
  , reply: identity
  }
  where
  bump :: Int -> {} -> Decision () (bumped :: {})
  bump _ _ = emit @"bumped" {}

  proto :: Int -> {} -> Decision () (bumped :: {})
  proto _ _ = emit @"bumped" {}

  toString :: Int -> {} -> Decision () (valueOf :: {})
  toString _ _ = emit @"valueOf" {}

-- | Ignores `valueOf`.
bumps :: View Tally TallyId Int
bumps = view { initial: 0, on: { bumped: \n _ -> n + 1 } }

-- | Throws on the second bump of a tally.
fragile :: View Tally TallyId Int
fragile = view
  { initial: 0, on: { bumped: \n _ -> if n >= 1 then unsafeCrashWith "fragile view" else n + 1 } }

counted :: Policy Customer Tally
counted = policy { customerRegistered: \_ _ -> [ order @"bump" target {} ] }

target :: TallyId
target = TallyId "t"

testTally :: Aff Unit
testTally = do
  derivedOrderIds
  retryHealsALostOrder
  clientsCannotClaimOrderIds
  prototypeLabels
  failingViewCommitsNothing

-- | A retried trigger re-runs its policy. Only the derived order id keeps
-- | the tally from counting the order twice.
derivedOrderIds :: Aff Unit
derivedOrderIds = do
  rt <- start { customer, tally, counted } testOptions
  let
    ada = CustomerId "c-1"
    registerAda =
      rt.customer.send (commandId "register") ada (cmd @"registerCustomer" { name: "ada" })
  _ <- expectRight "register" =<< registerAda
  _ <- expectRight "retry register" =<< registerAda
  counts <- rt.tally.history target
  assertEqual "a retried trigger's order commits once" 1 (length counts)
  rt.stop

-- | The order's target fails once, after the trigger committed. The caller
-- | sees the failure; retrying the trigger re-dispatches the lost order.
retryHealsALostOrder :: Aff Unit
retryHealsALostOrder = do
  failNext <- liftEffect (Ref.new true)
  let
    options =
      testOptions
        { beforeTurn = \stream -> when (streamIdText stream == "tally/t") do
            failing <-
              liftEffect (Ref.modify' (\pending -> { state: false, value: pending }) failNext)
            when failing (fail "the tally is down")
        }
  rt <- start { customer, tally, counted } options
  let
    ada = CustomerId "c-1"
    registerAda =
      rt.customer.send (commandId "register") ada (cmd @"registerCustomer" { name: "ada" })
  lost <- attempt registerAda
  assertThat "the caller sees the failed order" (isLeft lost)
  registered <- rt.customer.history ada
  assertEqual "the trigger itself committed" 1 (length registered)
  before <- rt.tally.history target
  assertEqual "the order was lost" 0 (length before)
  _ <- expectRight "retry register" =<< registerAda
  after <- rt.tally.history target
  assertEqual "the retry re-dispatched the lost order" 1 (length after)
  rt.stop

-- | A client key spelled like an order id is still the client's own id, so
-- | the policy's order cannot be suppressed in advance.
clientsCannotClaimOrderIds :: Aff Unit
clientsCannotClaimOrderIds = do
  rt <- start { customer, tally, counted } testOptions
  let
    spoofed = unsafePartial (fromJust (parseCommandId "counted:customer/c-1@1#0"))
  _ <- expectRight "client bump" =<< rt.tally.send spoofed target (cmd @"bump" {})
  _ <- expectRight "register" =<< rt.customer.send (commandId "register") (CustomerId "c-1")
    (cmd @"registerCustomer" { name: "ada" })
  counts <- rt.tally.history target
  assertEqual "the client's bump and the policy's bump both count" 2 (length counts)
  rt.stop

-- | Labels that name `Object.prototype` members dispatch like any other.
prototypeLabels :: Aff Unit
prototypeLabels = do
  rt <- start { tally, bumps } testOptions
  _ <- expectRight "toString" =<< rt.tally.send (commandId "poke") target (cmd @"toString" {})
  _ <- expectRight "bump" =<< rt.tally.send (commandId "bump") target (cmd @"bump" {})
  _ <- expectRight "__proto__" =<< rt.tally.send (commandId "proto") target (cmd @"__proto__" {})
  history <- rt.tally.history target
  assertEqual "every command committed" 3 (length history)
  row <- rt.bumps.read target
  assertEqual "a view skips an event it does not name" (Just 2) row
  rt.stop

-- | A view that throws fails the command, and the commit does not land
-- | without its views.
failingViewCommitsNothing :: Aff Unit
failingViewCommitsNothing = do
  rt <- start { tally, bumps, fragile } testOptions
  _ <- expectRight "first bump" =<< rt.tally.send (commandId "one") target (cmd @"bump" {})
  broken <- attempt (rt.tally.send (commandId "two") target (cmd @"bump" {}))
  case broken of
    Left _ -> pure unit
    Right _ -> fail "a throwing view did not fail the command"
  history <- rt.tally.history target
  assertEqual "the failed command appended nothing" 1 (length history)
  row <- rt.bumps.read target
  assertEqual "no view saw the failed command" (Just 1) row
  rt.stop
