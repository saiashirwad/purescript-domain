module Bank.Model
  ( Bank
  , World
  , bank
  , emptyWorld
  , sendAccount
  , sendCustomer
  , summaryFor
  , statementFor
  ) where

import Prelude

import Bank.Account
  ( AccountCommand
  , AccountEvent
  , AccountId(..)
  , AccountRejection
  , AccountState
  , accountDecider
  , accountStream
  )
import Bank.Customer
  ( CustomerCommand
  , CustomerEvent
  , CustomerId(..)
  , CustomerRejection
  , CustomerState
  , customerDecider
  , customerStream
  )
import Bank.Policy (mainAccountProvisioning)
import Bank.Views (StatementLine, Summary, statement, summary)
import Data.Array (concatMap, mapWithIndex)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Variant (prj)
import Domain
  ( Aggregate
  , CommandEnvelope
  , Envelope
  , Policy
  , Receipt
  , Stream
  , View
  , ViewState
  , applyView
  , commit
  , emptyStream
  , mkEnvelope
  )
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

type Bank =
  { account :: Aggregate AccountId AccountState AccountCommand AccountEvent AccountRejection { balance :: Int }
  , customer :: Aggregate CustomerId CustomerState CustomerCommand CustomerEvent CustomerRejection Unit
  , summary :: View String Summary AccountEvent Unit
  , statement :: View String Unit AccountEvent StatementLine
  , provisioning :: Policy CustomerEvent AccountCommand
  }

bank :: Bank
bank =
  { account: { stream: accountStream, decider: accountDecider }
  , customer: { stream: customerStream, decider: customerDecider }
  , summary
  , statement
  , provisioning: mainAccountProvisioning
  }

type World =
  { accounts :: Map String (Stream AccountState AccountEvent)
  , customers :: Map String (Stream CustomerState CustomerEvent)
  , summaries :: Map String (ViewState Summary Unit)
  , statements :: Map String (ViewState Unit StatementLine)
  }

emptyWorld :: World
emptyWorld =
  { accounts: Map.empty
  , customers: Map.empty
  , summaries: Map.empty
  , statements: Map.empty
  }

sendAccount
  :: AccountId
  -> AccountCommand
  -> World
  -> Either AccountRejection { world :: World, receipt :: Receipt AccountEvent { balance :: Int } }
sendAccount accountId command world =
  let
    key = accountStream accountId
    stream =
      fromMaybe
        (emptyStream accountDecider.initial)
        (Map.lookup key world.accounts)
  in
    do
      result <- commit accountDecider
        { state: stream.state
        , version: stream.version
        , stream: key
        }
        command
      let
        envelopes = mapWithIndex
          (\index event -> mkEnvelope (unAccountId accountId) key (stream.version + 1 + index) event)
          result.receipt.events
        nextStream =
          { state: result.state
          , version: result.receipt.version
          , history: stream.history <> envelopes
          }
        nextWorld =
          applyAccountEvents envelopes
            (world { accounts = Map.insert key nextStream world.accounts })
      pure { world: nextWorld, receipt: result.receipt }

sendCustomer
  :: CustomerId
  -> CustomerCommand
  -> World
  -> Either CustomerRejection { world :: World, receipt :: Receipt CustomerEvent Unit }
sendCustomer customerId command world =
  let
    key = customerStream customerId
    stream =
      fromMaybe
        (emptyStream customerDecider.initial)
        (Map.lookup key world.customers)
  in
    do
      result <- commit customerDecider
        { state: stream.state
        , version: stream.version
        , stream: key
        }
        command
      let
        envelopes = mapWithIndex
          (\index event -> mkEnvelope (unCustomerId customerId) key (stream.version + 1 + index) event)
          result.receipt.events
        nextStream =
          { state: result.state
          , version: result.receipt.version
          , history: stream.history <> envelopes
          }
        customerWorld =
          world { customers = Map.insert key nextStream world.customers }
        commands = concatMap mainAccountProvisioning.on envelopes
        finalWorld = foldl applyCommand customerWorld commands
      pure { world: finalWorld, receipt: result.receipt }

applyCommand :: World -> CommandEnvelope AccountCommand -> World
applyCommand world { target, command } =
  case sendAccount (AccountId target) command world of
    Left rejection -> handlePolicyRejection rejection world
    Right result -> result.world

-- | A repeated policy run opens an account that is already open.
-- | Anything else is a defect in the policy.
handlePolicyRejection :: AccountRejection -> World -> World
handlePolicyRejection rejection world =
  case prj (Proxy :: Proxy "alreadyOpen") rejection of
    Just _ -> world
    Nothing ->
      unsafeCrashWith "MainAccountProvisioning: unexpected account rejection"

applyAccountEvents :: Array (Envelope AccountEvent) -> World -> World
applyAccountEvents envelopes world =
  world
    { summaries = foldl (applyView summary) world.summaries envelopes
    , statements = foldl (applyView statement) world.statements envelopes
    }

summaryFor :: AccountId -> World -> Maybe Summary
summaryFor accountId world =
  map _.state (Map.lookup (accountStream accountId) world.summaries)

statementFor :: AccountId -> World -> Array StatementLine
statementFor accountId world =
  maybe [] _.emitted (Map.lookup (accountStream accountId) world.statements)

unAccountId :: AccountId -> String
unAccountId (AccountId id) = id

unCustomerId :: CustomerId -> String
unCustomerId (CustomerId id) = id
