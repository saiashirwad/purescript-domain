module Test.Main where

import Prelude

import Bank.Account
  ( AccountCommand
  , AccountEvent
  , AccountId(..)
  , AccountRejection
  , AccountState(..)
  , accountDecider
  , accountOpened
  , accountStream
  , alreadyOpen
  , depositMoney
  , insufficientFunds
  , moneyDeposited
  , moneyWithdrawn
  , notOpen
  , openAccount
  , withdrawMoney
  )
import Bank.Customer (customerRegistered, registerCustomer)
import Bank.Policy (mainAccountProvisioning)
import Bank.Runtime
  ( accountHistory
  , customerHistory
  , newWithActorCapabilities
  , sendAccount
  , sendCustomer
  , shutdown
  , statementFor
  , summaryFor
  )
import Bank.Views (StatementLine, Summary, summary)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Domain
  ( Changes(..)
  , Envelope
  , ViewState
  , applyView
  , changesValues
  , changesVersion
  , commandsFrom
  , firstSequence
  , mkEnvelope
  , replay
  , sequenceNumber
  , versionNumber
  , viewEmitted
  )
import Domain.Story (Step(..), runStory)
import Domain.Runtime.Actor as Actor
import Domain.Trace (noTrace)
import Effect (Effect)
import Effect.Aff (Aff, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, message, throw)
import Test.Projection (testProjection)
import Test.QuickCheck (quickCheck')
import Test.Actor (testActor)
import Test.Api (testApi)
import Test.Codec (testCodec)
import Test.Route (testRoute)
import Test.Support
  ( accountId
  , assertEqual
  , commandId
  , customerId
  , expectLeftAff
  , expectRight
  , expectRightAff
  , firstOf
  , fixedClock
  , fixedInstant
  , newRuntime
  , versionOne
  )
import Test.Trace (testTrace)

main :: Effect Unit
main =
  runAff_
    ( case _ of
        Left failure -> throw (message failure)
        Right _ -> log "All stories passed."
    )
    testMain

testMain :: Aff Unit
testMain = do
  testActor
  testOpenAccount
  testOpenTwice
  testRetryReturnsReceipt
  testRejectedCommandStoresNoReceipt
  testDeposit
  testDepositUnopened
  testOverdraft
  testSummary
  testStatement
  testPolicy
  testPolicyRetry
  testBankOnActors
  testRoute
  testApi
  testCodec
  testReplayLaw
  testReplayLawProperty
  testViewDuplicateDelivery
  testViewSemilattice
  testSemilatticeProperty
  testChanges
  testProjection
  testTrace
  testStoryDeposit
  testStoryOverdraft
  testStoryUnopened

testOpenAccount :: Aff Unit
testOpenAccount = do
  runtime <- newRuntime
  result <- sendAccount runtime (commandId "cmd-open") accountId (openAccount "sam")
  receipt <- expectRight result
  assertEqual "opens.events" [ accountOpened "sam" ] receipt.events
  assertEqual "opens.version" 1 (versionNumber receipt.version)
  summaryState <- summaryFor runtime accountId
  assertSummary "opens.summary" { owner: Just "sam", balance: 0 } summaryState
  history <- accountHistory runtime accountId
  assertEqual "opens.history.length" 1 (length history)
  assertEqual "opens.envelope.seq" 1 (sequenceNumber (firstOf history).seq)

testOpenTwice :: Aff Unit
testOpenTwice = do
  runtime <- newRuntime
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-open") accountId (openAccount "sam"))
  rejection <- expectLeftAff (sendAccount runtime (commandId "cmd-open-2") accountId (openAccount "sam"))
  assertEqual "refuses to open twice" alreadyOpen rejection
  history <- accountHistory runtime accountId
  assertEqual "rejected command appends no events" 1 (length history)

testRetryReturnsReceipt :: Aff Unit
testRetryReturnsReceipt = do
  runtime <- newRuntime
  first <- expectRightAff (sendAccount runtime (commandId "cmd-open") accountId (openAccount "sam"))
  second <- expectRightAff (sendAccount runtime (commandId "cmd-open") accountId (openAccount "sam"))
  assertEqual "retry returns the same receipt" first second
  history <- accountHistory runtime accountId
  assertEqual "retry appends once" 1 (length history)

testRejectedCommandStoresNoReceipt :: Aff Unit
testRejectedCommandStoresNoReceipt = do
  runtime <- newRuntime
  rejection <- expectLeftAff (sendAccount runtime (commandId "cmd-shared") accountId (depositMoney 100))
  assertEqual "deposit on unopened is rejected" notOpen rejection
  opened <- expectRightAff (sendAccount runtime (commandId "cmd-shared") accountId (openAccount "sam"))
  assertEqual "a rejected command id can be reused" 1 (versionNumber opened.version)

testDeposit :: Aff Unit
testDeposit = do
  runtime <- newRuntime
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-open") accountId (openAccount "sam"))
  deposited <- expectRightAff (sendAccount runtime (commandId "cmd-deposit") accountId (depositMoney 100))
  assertEqual "deposit.events" [ moneyDeposited 100 100 ] deposited.events
  assertEqual "deposit.reply" 100 deposited.reply.balance
  summaryState <- summaryFor runtime accountId
  assertSummary "deposit.summary" { owner: Just "sam", balance: 100 } summaryState

testDepositUnopened :: Aff Unit
testDepositUnopened = do
  runtime <- newRuntime
  rejection <- expectLeftAff (sendAccount runtime (commandId "cmd-deposit") accountId (depositMoney 100))
  assertEqual "refuses a deposit into an unopened account" notOpen rejection
  history <- accountHistory runtime accountId
  assertEqual "unopened stream has no history" 0 (length history)

testOverdraft :: Aff Unit
testOverdraft = do
  runtime <- newRuntime
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-open") accountId (openAccount "sam"))
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-deposit") accountId (depositMoney 100))
  rejection <- expectLeftAff (sendAccount runtime (commandId "cmd-withdraw") accountId (withdrawMoney 150))
  assertEqual "refuses an overdraft" (insufficientFunds 100 150) rejection

testSummary :: Aff Unit
testSummary = do
  runtime <- newRuntime
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-open") accountId (openAccount "sam"))
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-deposit") accountId (depositMoney 100))
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-withdraw") accountId (withdrawMoney 30))
  summaryState <- summaryFor runtime accountId
  assertSummary "summary follows the balance"
    { owner: Just "sam", balance: 70 }
    summaryState

testStatement :: Aff Unit
testStatement = do
  runtime <- newRuntime
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-open") accountId (openAccount "sam"))
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-deposit") accountId (depositMoney 100))
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-withdraw") accountId (withdrawMoney 30))
  lines <- statementFor runtime accountId
  assertEqual "statement has one line per movement"
    [ "deposit:100:100", "withdrawal:30:70" ]
    (map showLine lines)

testPolicy :: Aff Unit
testPolicy = do
  runtime <- newRuntime
  registered <- expectRightAff (sendCustomer runtime (commandId "cmd-register") customerId (registerCustomer "sam"))
  assertEqual "customer.events" [ customerRegistered "sam" ] registered.events
  summaryState <- summaryFor runtime (AccountId "c-1-main")
  assertSummary "policy opens the main account"
    { owner: Just "sam", balance: 0 }
    summaryState

testPolicyRetry :: Aff Unit
testPolicyRetry = do
  runtime <- newRuntime
  first <- expectRightAff (sendCustomer runtime (commandId "cmd-register") customerId (registerCustomer "sam"))
  second <- expectRightAff (sendCustomer runtime (commandId "cmd-register") customerId (registerCustomer "sam"))
  assertEqual "customer retry returns the same receipt" first second
  customerEvents <- customerHistory runtime customerId
  accountEvents <- accountHistory runtime (AccountId "c-1-main")
  assertEqual "customer event appended once" 1 (length customerEvents)
  assertEqual "policy account event appended once" 1 (length accountEvents)
  let derived = commandsFrom mainAccountProvisioning (firstOf customerEvents)
  assertEqual "policy derives exactly one command" 1 (length derived)

testBankOnActors :: Aff Unit
testBankOnActors = do
  runtime <-
    newWithActorCapabilities 8 Actor.defaultHooks
      { clock: fixedClock, tracer: noTrace }
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-open") accountId (openAccount "sam"))
  _ <- expectRightAff (sendAccount runtime (commandId "cmd-deposit") accountId (depositMoney 100))
  summaryState <- summaryFor runtime accountId
  assertSummary "the bank runs on actors"
    { owner: Just "sam", balance: 100 }
    summaryState
  registered <- expectRightAff (sendCustomer runtime (commandId "cmd-register") customerId (registerCustomer "sam"))
  assertEqual "the actor host runs policies" [ customerRegistered "sam" ] registered.events
  policySummary <- summaryFor runtime (AccountId "c-1-main")
  assertSummary "the actor host applies policy views"
    { owner: Just "sam", balance: 0 }
    policySummary
  shutdown runtime

testReplayLaw :: Aff Unit
testReplayLaw = do
  let
    before = [ accountOpened "sam" ]
    after = [ moneyDeposited 100 100, moneyWithdrawn 30 70 ]
    replayAll = replay accountDecider.evolve Unopened (before <> after)
    replayInSteps =
      replay accountDecider.evolve
        (replay accountDecider.evolve Unopened before)
        after
  assertEqual "replay is a monoid homomorphism" (show replayAll) (show replayInSteps)

testReplayLawProperty :: Aff Unit
testReplayLawProperty =
  liftEffect
    ( quickCheck' 100
        \(xs :: Array Int) (ys :: Array Int) ->
          replay (\state event -> state + event) 0 (xs <> ys)
            == replay
                (\state event -> state + event)
                (replay (\state event -> state + event) 0 xs)
                ys
    )

testViewDuplicateDelivery :: Aff Unit
testViewDuplicateDelivery = do
  let
    envelope = testEnvelope (accountOpened "sam")
    once = applyView summary Map.empty envelope
    twice = applyView summary once envelope
  assertEqual "view state is stable under duplicate delivery"
    (viewEntry once)
    (viewEntry twice)

testViewSemilattice :: Aff Unit
testViewSemilattice = do
  let
    semilattice
      :: { initial :: Int
         , keyOf :: Envelope AccountId AccountEvent -> Maybe AccountId
         , step :: Int -> Envelope AccountId AccountEvent -> Tuple Int (Array Unit)
         }
    semilattice =
      { initial: 0
      , keyOf: \eventEnvelope -> Just eventEnvelope.id
      , step: \state eventEnvelope -> Tuple (max state (eventAmount eventEnvelope.event)) []
      }
    envelope = testEnvelope (moneyDeposited 100 100)
    once = applyView semilattice Map.empty envelope
    twice = applyView semilattice once envelope
    Tuple firstState _ = semilattice.step semilattice.initial envelope
    Tuple secondState _ = semilattice.step firstState envelope
  assertEqual "join-semilattice view is idempotent"
    (map _.state once)
    (map _.state twice)
  assertEqual "the semilattice step itself is idempotent"
    firstState
    secondState

testSemilatticeProperty :: Aff Unit
testSemilatticeProperty =
  liftEffect do
    quickCheck' 100 \(value :: Int) ->
      max value value == value
    quickCheck' 100 \(x :: Int) (y :: Int) (z :: Int) ->
      max x (max y z) == max (max x y) z

testChanges :: Aff Unit
testChanges = do
  let
    changes = Changes { version: versionOne, values: [ 1, 2, 3 ] }
    traversed = traverse (\value -> if value > 0 then Right (value * 2) else Left "negative") changes
    mapped = map (_ * 2) changes
  case traversed of
    Left failure -> throwError (error ("Changes.traverse failed: " <> failure))
    Right result -> do
      assertEqual "traverse keeps the version" (changesVersion changes) (changesVersion result)
      assertEqual "traverse maps every value" [ 2, 4, 6 ] (changesValues result)
  assertEqual "functor maps every value while keeping the version"
    (Changes { version: versionOne, values: [ 2, 4, 6 ] })
    mapped

testStoryDeposit :: Aff Unit
testStoryDeposit =
  expectAccountStory "deposits into an open account and replies with the balance"
    [ Given (accountOpened "sam")
    , When (depositMoney 100)
    , Then (moneyDeposited 100 100)
    , Replied { balance: 100 }
    ]

testStoryOverdraft :: Aff Unit
testStoryOverdraft =
  expectAccountStory "refuses to withdraw more than the balance"
    [ Given (accountOpened "sam")
    , Given (moneyDeposited 100 100)
    , When (withdrawMoney 150)
    , Rejected (insufficientFunds 100 150)
    ]

testStoryUnopened :: Aff Unit
testStoryUnopened =
  expectAccountStory "refuses a deposit into an account that isn't open"
    [ When (depositMoney 100)
    , Rejected notOpen
    ]

expectAccountStory
  :: String
  -> Array (Step AccountEvent AccountCommand AccountRejection { balance :: Int })
  -> Aff Unit
expectAccountStory label steps =
  case runStory accountDecider steps of
    Left failure -> throwError (error ("FAIL " <> label <> ": " <> failure))
    Right _ -> pure unit

showLine :: StatementLine -> String
showLine line = line.kind <> ":" <> show line.amount <> ":" <> show line.balance

assertSummary
  :: String
  -> { owner :: Maybe String, balance :: Int }
  -> Maybe Summary
  -> Aff Unit
assertSummary label expected actual =
  case actual of
    Nothing -> throwError (error ("FAIL " <> label <> ": no summary"))
    Just summaryState -> do
      assertEqual (label <> ".owner") expected.owner summaryState.owner
      assertEqual (label <> ".balance") expected.balance summaryState.balance

testEnvelope :: AccountEvent -> Envelope AccountId AccountEvent
testEnvelope event =
  mkEnvelope accountId
    (accountStream accountId)
    firstSequence
    (commandId "cmd-test")
    fixedInstant
    event

eventAmount :: AccountEvent -> Int
eventAmount = match
  { accountOpened: \_ -> 0
  , moneyDeposited: \event -> event.amount
  , moneyWithdrawn: \event -> event.amount
  }

viewEntry
  :: Map AccountId (ViewState Summary Unit)
  -> Maybe { state :: Summary, emitted :: Array Unit }
viewEntry states =
  map
    (\viewState -> { state: viewState.state, emitted: viewEmitted viewState })
    (Map.lookup accountId states)
