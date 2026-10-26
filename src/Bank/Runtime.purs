module Bank.Runtime
  ( Bank
  , BankRuntime
  , Capabilities
  , bank
  , new
  , newWithClock
  , newWithCapabilities
  , newWithActorCapabilities
  , sendAccount
  , sendCustomer
  , summaryFor
  , summaryAtLeast
  , statementFor
  , accountHistory
  , customerHistory
  , shutdown
  ) where

import Prelude

import Bank.Account
  ( AccountCommand
  , AccountEvent
  , AccountId
  , AccountRejection
  , AccountState
  , accountDecider
  , accountStream
  )
import Bank.Customer
  ( CustomerCommand
  , CustomerEvent
  , CustomerId
  , CustomerRejection
  , CustomerState
  , customerDecider
  , customerStream
  )
import Bank.Policy (mainAccountProvisioning)
import Bank.Trace
  ( accountCommandName
  , accountEventName
  , accountRejectionName
  , customerCommandName
  , customerEventName
  , customerRejectionName
  )
import Bank.Views (StatementLine, Summary, statement, summary)
import Data.Array (concatMap, intercalate, length)
import Data.DateTime.Instant (Instant, diff)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Domain
  ( Aggregate
  , CommandId
  , Envelope
  , Policy
  , Receipt
  , View
  , ViewState
  , Version
  , applyViews
  , commandIdText
  , commandsFrom
  , sequenceNumber
  , streamIdText
  , versionNumber
  , viewEmitted
  )
import Domain.Runtime (Runtime)
import Domain.Runtime as Runtime
import Domain.Runtime.Actor as Actor
import Domain.Runtime.InMemory (Clock)
import Domain.Runtime.InMemory as InMemory
import Domain.Trace (Span, Tracer, noTrace)
import Effect.Aff (Aff, delay, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref

type Bank =
  { account
      :: Aggregate AccountId AccountState AccountCommand AccountEvent
           AccountRejection
           { balance :: Int }
  , customer
      :: Aggregate CustomerId CustomerState CustomerCommand CustomerEvent
           CustomerRejection
           Unit
  , summary :: View AccountId Summary AccountId AccountEvent Unit
  , statement :: View AccountId Unit AccountId AccountEvent StatementLine
  , provisioning :: Policy CustomerId CustomerEvent AccountId AccountCommand
  }

bank :: Bank
bank =
  { account: { stream: accountStream, decider: accountDecider }
  , customer: { stream: customerStream, decider: customerDecider }
  , summary
  , statement
  , provisioning: mainAccountProvisioning
  }

type BankViews =
  { summaries :: Map AccountId (ViewState Summary Unit)
  , statements :: Map AccountId (ViewState Unit StatementLine)
  }

type Capabilities =
  { clock :: Clock
  , tracer :: Tracer Aff
  }

newtype BankRuntime = BankRuntime
  { accounts
      :: Runtime Aff AccountId AccountState AccountCommand AccountEvent
           AccountRejection
           { balance :: Int }
  , customers
      :: Runtime Aff CustomerId CustomerState CustomerCommand CustomerEvent
           CustomerRejection
           Unit
  , views :: Ref BankViews
  , capabilities :: Capabilities
  }

new :: Aff BankRuntime
new =
  newWithCapabilities
    { clock: InMemory.systemClock
    , tracer: noTrace
    }

newWithClock :: Clock -> Aff BankRuntime
newWithClock clock =
  newWithCapabilities { clock, tracer: noTrace }

newWithCapabilities :: Capabilities -> Aff BankRuntime
newWithCapabilities capabilities = do
  accounts <- InMemory.newWithClock capabilities.clock
  customers <- InMemory.newWithClock capabilities.clock
  views <- liftEffect do
    Ref.new
      { summaries: Map.empty
      , statements: Map.empty
      }
  pure
    ( BankRuntime
        { accounts: Runtime.fromInMemory accounts
        , customers: Runtime.fromInMemory customers
        , views
        , capabilities
        }
    )

-- | The same bank on the per-key actor host. Account commands and customer
-- | commands for one key are serialized; different keys run in parallel.
newWithActorCapabilities
  :: Int
  -> Actor.Hooks
  -> Capabilities
  -> Aff BankRuntime
newWithActorCapabilities capacity hooks capabilities = do
  accountStore <- InMemory.newWithClock capabilities.clock
  customerStore <- InMemory.newWithClock capabilities.clock
  accounts <- Actor.newWithHooks capacity hooks accountStore
  customers <- Actor.newWithHooks capacity hooks customerStore
  views <- liftEffect do
    Ref.new
      { summaries: Map.empty
      , statements: Map.empty
      }
  pure
    ( BankRuntime
        { accounts: Runtime.fromActor accounts
        , customers: Runtime.fromActor customers
        , views
        , capabilities
        }
    )

sendAccount
  :: BankRuntime
  -> CommandId
  -> AccountId
  -> AccountCommand
  -> Aff
       ( Either AccountRejection
           (Receipt AccountId AccountEvent { balance :: Int })
       )
sendAccount (BankRuntime runtime) commandId accountId command = do
  startedAt <- runtime.capabilities.clock.now
  result <-
    runtime.accounts.send bank.account commandId accountId command
  finishedAt <- runtime.capabilities.clock.now
  runtime.capabilities.tracer.emit
    ( spanOf "command.account" startedAt finishedAt
        (accountAttributes commandId accountId command result)
    )
  case result of
    Left rejection -> pure (Left rejection)
    Right receipt -> do
      liftEffect do
        Ref.modify_
          (applyAccountEnvelopes receipt.envelopes)
          runtime.views
      pure (Right receipt)

sendCustomer
  :: BankRuntime
  -> CommandId
  -> CustomerId
  -> CustomerCommand
  -> Aff (Either CustomerRejection (Receipt CustomerId CustomerEvent Unit))
sendCustomer runtime@(BankRuntime state) commandId customerId command = do
  startedAt <- state.capabilities.clock.now
  result <-
    state.customers.send bank.customer commandId customerId command
  case result of
    Left rejection -> do
      finishedAt <- state.capabilities.clock.now
      state.capabilities.tracer.emit
        ( spanOf "command.customer" startedAt finishedAt
            (customerAttributes commandId customerId command result 0)
        )
      pure (Left rejection)
    Right receipt -> do
      let
        commands =
          concatMap (commandsFrom bank.provisioning) receipt.envelopes
      for_ commands \addressed -> do
        accountResult <-
          sendAccount runtime addressed.commandId addressed.target
            addressed.command
        case accountResult of
          Left rejection -> handlePolicyRejection rejection
          Right _ -> pure unit
      finishedAt <- state.capabilities.clock.now
      state.capabilities.tracer.emit
        ( spanOf "command.customer" startedAt finishedAt
            (customerAttributes commandId customerId command result (length commands))
        )
      pure (Right receipt)

handlePolicyRejection :: AccountRejection -> Aff Unit
handlePolicyRejection rejection =
  throwError
    ( error
        ( "Bank.Runtime: the provisioning policy produced an unexpected "
            <> "rejection: "
            <> show rejection
        )
    )

applyAccountEnvelopes
  :: Array (Envelope AccountId AccountEvent)
  -> BankViews
  -> BankViews
applyAccountEnvelopes envelopes views =
  { summaries: applyViews bank.summary views.summaries envelopes
  , statements: applyViews bank.statement views.statements envelopes
  }

summaryFor :: BankRuntime -> AccountId -> Aff (Maybe Summary)
summaryFor (BankRuntime runtime) accountId = do
  views <- liftEffect (Ref.read runtime.views)
  pure (map _.state (Map.lookup accountId views.summaries))

-- | Read-your-writes: wait until the summary view has applied every event up
-- | to `required`. The in-memory host is synchronous, so this returns on the
-- | first check for any version that has already committed. A future version
-- | times out rather than returning stale data.
summaryAtLeast
  :: BankRuntime
  -> AccountId
  -> Version
  -> Aff (Maybe Summary)
summaryAtLeast runtime accountId required =
  waitForView 50 runtime accountId required
  where
  waitForView attempts runtime' accountId' required'
    | attempts <= 0 = pure Nothing
    | otherwise = do
        caughtUp <- viewCaughtUp runtime' accountId' required'
        if caughtUp then summaryFor runtime' accountId'
        else do
          delay (Milliseconds 1.0)
          waitForView (attempts - 1) runtime' accountId' required'

viewCaughtUp :: BankRuntime -> AccountId -> Version -> Aff Boolean
viewCaughtUp (BankRuntime runtime) accountId required = do
  views <- liftEffect (Ref.read runtime.views)
  pure case Map.lookup accountId views.summaries of
    Nothing -> versionNumber required == 0
    Just viewState ->
      case Map.lookup (accountStream accountId) viewState.applied of
        Nothing -> versionNumber required == 0
        Just seen -> sequenceNumber seen >= versionNumber required

statementFor :: BankRuntime -> AccountId -> Aff (Array StatementLine)
statementFor (BankRuntime runtime) accountId = do
  views <- liftEffect (Ref.read runtime.views)
  pure (maybe [] viewEmitted (Map.lookup accountId views.statements))

accountHistory
  :: BankRuntime
  -> AccountId
  -> Aff (Array (Envelope AccountId AccountEvent))
accountHistory (BankRuntime runtime) accountId =
  runtime.accounts.history bank.account accountId

customerHistory
  :: BankRuntime
  -> CustomerId
  -> Aff (Array (Envelope CustomerId CustomerEvent))
customerHistory (BankRuntime runtime) customerId =
  runtime.customers.history bank.customer customerId

shutdown :: BankRuntime -> Aff Unit
shutdown (BankRuntime runtime) = do
  -- Stop customer work first: a customer policy may still need to send an
  -- account command while the customer actor drains.
  runtime.customers.shutdown
  runtime.accounts.shutdown

spanOf
  :: String
  -> Instant
  -> Instant
  -> Map String String
  -> Span
spanOf name startedAt finishedAt attributes =
  { name
  , startedAt
  , duration: diff finishedAt startedAt :: Milliseconds
  , attributes
  }

accountAttributes
  :: CommandId
  -> AccountId
  -> AccountCommand
  -> Either AccountRejection (Receipt AccountId AccountEvent { balance :: Int })
  -> Map String String
accountAttributes commandId accountId command result =
  Map.union base
    ( case result of
        Left rejection ->
          Map.fromFoldable
            [ Tuple "outcome" "rejected"
            , Tuple "rejection" (accountRejectionName rejection)
            ]
        Right receipt ->
          Map.fromFoldable
            [ Tuple "outcome" "committed"
            , Tuple "version" (show (versionNumber receipt.version))
            , Tuple "events"
                (intercalate "," (map accountEventName receipt.events))
            , Tuple "reply.balance" (show receipt.reply.balance)
            ]
    )
  where
  base =
    Map.fromFoldable
      [ Tuple "command.id" (commandIdText commandId)
      , Tuple "stream.id" (streamIdText (accountStream accountId))
      , Tuple "command.name" (accountCommandName command)
      ]

customerAttributes
  :: CommandId
  -> CustomerId
  -> CustomerCommand
  -> Either CustomerRejection (Receipt CustomerId CustomerEvent Unit)
  -> Int
  -> Map String String
customerAttributes commandId customerId command result policyCommands =
  Map.union base
    ( Map.union
        (Map.fromFoldable [ Tuple "policy.commands" (show policyCommands) ])
        ( case result of
            Left rejection ->
              Map.fromFoldable
                [ Tuple "outcome" "rejected"
                , Tuple "rejection" (customerRejectionName rejection)
                ]
            Right receipt ->
              Map.fromFoldable
                [ Tuple "outcome" "committed"
                , Tuple "version" (show (versionNumber receipt.version))
                , Tuple "events"
                    (intercalate "," (map customerEventName receipt.events))
                ]
        )
    )
  where
  base =
    Map.fromFoldable
      [ Tuple "command.id" (commandIdText commandId)
      , Tuple "stream.id" (streamIdText (customerStream customerId))
      , Tuple "command.name" (customerCommandName command)
      ]
