-- | Runtime guarantees the bank cannot show: its commands happen to be
-- | idempotent and its labels are ordinary words. A tally counts every
-- | bump, so a command that runs twice shows up as a count of two.
module Test.Tally (testTally) where

import Prelude

import Bank.Customer (CustomerChange, CustomerCommand(..), CustomerId(..), customer)
import Data.Array (length)
import Data.Either (Either(..), isLeft)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Domain (Aggregate(..), Change, Policy(..), View, byId, parseCommandId)
import Domain.Codec (decode, encode, labelled)
import Domain.Fold as Fold
import Domain.Internal.Stream (Refusal(..))
import Domain.Runtime (start)
import Effect.Aff (Aff, attempt)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Test.Support
  (assertEqual, assertJson, assertThat, commandId, expectRight, fail, failingOnce, testOptions)

newtype TallyId = TallyId String

derive instance Newtype TallyId _
derive newtype instance Eq TallyId
derive newtype instance Ord TallyId
derive newtype instance Show TallyId

-- | `ValueOf` and `Constructor` label as `valueOf` and `constructor`, which
-- | are also names on every JavaScript object.
data TallyCommand = Bump | ValueOf | Constructor

derive instance Eq TallyCommand
derive instance Generic TallyCommand _
instance Show TallyCommand where
  show = genericShow

data TallyEvent = Bumped | ToString

derive instance Eq TallyEvent
derive instance Generic TallyEvent _
instance Show TallyEvent where
  show = genericShow

-- | A tally never rejects.
data TallyRejection

derive instance Eq TallyRejection
derive instance Generic TallyRejection _
instance Show TallyRejection where
  show = genericShow

type Tally = Aggregate TallyId TallyCommand TallyEvent TallyRejection Int Int

type TallyChange = Change TallyId Int TallyEvent

tally :: Tally
tally = Aggregate
  { initial: 0
  , decide: \sent _ -> Right (eventsOf sent)
  , evolve: \n -> case _ of
      Bumped -> n + 1
      ToString -> n
  , reply: identity
  }
  where
  eventsOf = case _ of
    Bump -> [ Bumped ]
    ValueOf -> [ ToString ]
    Constructor -> [ Bumped ]

-- | Ignores `ToString`.
bumps :: View TallyChange TallyId Int
bumps = byId $ Fold.fold 0 \n { event } -> case event of
  Bumped -> n + 1
  ToString -> n

-- | Throws on the second bump of a tally.
fragile :: View TallyChange TallyId Int
fragile = byId $ Fold.fold 0 \n _ ->
  if n >= 1 then unsafeCrashWith "fragile view" else n + 1

counted :: Policy CustomerChange TallyId TallyCommand
counted = Policy \_ -> [ { to: target, command: Bump } ]

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
      rt.customer.send (commandId "register") ada (RegisterCustomer { name: "ada" })
  _ <- expectRight "register" =<< registerAda
  _ <- expectRight "retry register" =<< registerAda
  counts <- rt.tally.history target
  assertEqual "a retried trigger's order commits once" 1 (length counts)
  rt.stop

-- | The order's target fails once, after the trigger committed. The caller
-- | sees the failure; retrying the trigger re-dispatches the lost order.
retryHealsALostOrder :: Aff Unit
retryHealsALostOrder = do
  failing <- failingOnce "tally/t" "the tally is down"
  rt <- start { customer, tally, counted } testOptions { beforeTurn = failing }
  let
    ada = CustomerId "c-1"
    registerAda =
      rt.customer.send (commandId "register") ada (RegisterCustomer { name: "ada" })
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
  _ <- expectRight "client bump" =<< rt.tally.send spoofed target Bump
  _ <- expectRight "register" =<< rt.customer.send (commandId "register") (CustomerId "c-1")
    (RegisterCustomer { name: "ada" })
  counts <- rt.tally.history target
  assertEqual "the client's bump and the policy's bump both count" 2 (length counts)
  rt.stop

-- | Constructors labelled like `Object.prototype` members behave like any
-- | other: the runtime stores the label as the command's name, and the codec
-- | reads it back.
prototypeLabels :: Aff Unit
prototypeLabels = do
  rt <- start { tally, bumps } testOptions
  _ <- expectRight "valueOf" =<< rt.tally.send (commandId "poke") target ValueOf
  _ <- expectRight "bump" =<< rt.tally.send (commandId "bump") target Bump
  _ <- expectRight "constructor" =<< rt.tally.send (commandId "ctor") target Constructor
  retried <- rt.tally.send (commandId "poke") target ValueOf
  _ <- expectRight "a retried valueOf is a duplicate, not a new command" retried
  reused <- rt.tally.send (commandId "poke") target Constructor
  case reused of
    Left (Reused { command }) -> assertEqual "a reused id names the stored label" "valueOf" command
    _ -> fail "an id stored for valueOf was reusable by constructor"
  history <- rt.tally.history target
  assertEqual "every command committed once" 3 (length history)
  row <- rt.bumps.read target
  assertEqual "a view skips an event it does not name" (Just 2) row
  rt.stop

  assertJson "a valueOf command encodes under its label"
    """{"type":"valueOf","value":{}}""" (encode labelled ValueOf)
  assertEqual "valueOf and constructor decode to their constructors"
    (Right [ ValueOf, Constructor ])
    ( traverse (decode labelled <<< encode labelled) [ ValueOf, Constructor ]
    )
  assertEqual "toString decodes as an event" (Right ToString)
    (decode labelled (encode labelled ToString))

-- | A view that throws fails the command, and the commit does not land
-- | without its views.
failingViewCommitsNothing :: Aff Unit
failingViewCommitsNothing = do
  rt <- start { tally, bumps, fragile } testOptions
  _ <- expectRight "first bump" =<< rt.tally.send (commandId "one") target Bump
  broken <- attempt (rt.tally.send (commandId "two") target Bump)
  case broken of
    Left _ -> pure unit
    Right _ -> fail "a throwing view did not fail the command"
  history <- rt.tally.history target
  assertEqual "the failed command appended nothing" 1 (length history)
  row <- rt.bumps.read target
  assertEqual "no view saw the failed command" (Just 1) row
  rt.stop
