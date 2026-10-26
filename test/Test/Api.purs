module Test.Api (testApi) where

import Prelude

import Bank.Account (AccountId(..))
import Bank.Api
  ( BankPrincipal
  , ClientError(..)
  , CommandResult
  , client
  , defaultBankJson
  , handle
  )
import Bank.Customer (CustomerId(..))
import Bank.Route (BankRoute(..), printRoute)
import Bank.Runtime
  ( BankRuntime
  , accountHistory
  , newWithClock
  , summaryFor
  )
import Bank.Views (Summary)
import Data.Argonaut.Core (Json, jsonNull, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Domain (Version, parseVersion)
import Domain.Http (Method(..), Request, Response)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Partial.Unsafe (unsafeCrashWith)
import Test.Support (assertEqual, commandId, fixedClock)

accountId :: AccountId
accountId = AccountId "a-api"

otherAccountId :: AccountId
otherAccountId = AccountId "a-api-other"

principal :: BankPrincipal
principal =
  { principal:
      { subject: "sam"
      , roles: Set.fromFoldable [ "customer" ]
      }
  , accounts: Set.singleton accountId
  }

stranger :: BankPrincipal
stranger =
  { principal:
      { subject: "stranger"
      , roles: Set.empty
      }
  , accounts: Set.empty
  }

adminPrincipal :: BankPrincipal
adminPrincipal =
  { principal:
      { subject: "root"
      , roles: Set.fromFoldable [ "admin" ]
      }
  , accounts: Set.empty
  }

testApi :: Aff Unit
testApi = do
  testCommandAndReadYourWrites
  testIdempotentRetry
  testAuthorization
  testDomainRejection
  testRoutingErrors
  testTypedClient
  testPolicyThroughApi

testCommandAndReadYourWrites :: Aff Unit
testCommandAndReadYourWrites = do
  runtime <- newWithClock fixedClock
  opened <- post runtime principal "api-open" (OpenAccount accountId)
    (encodeJson { owner: "sam" })
  assertEqual "open status" 200 opened.status
  openBody <- decodeCommand opened
  assertEqual "open version" 1 openBody.version
  summaryBefore <- summary runtime principal accountId Nothing
  assertSummary "summary after open" { owner: Just "sam", balance: 0 } summaryBefore
  deposited <- post runtime principal "api-deposit" (DepositMoney accountId)
    (encodeJson { amount: 100 })
  assertEqual "deposit status" 200 deposited.status
  summaryAfter <- summary runtime principal accountId (Just "2")
  assertSummary "read-your-writes with atLeast"
    { owner: Just "sam", balance: 100 }
    summaryAfter

testIdempotentRetry :: Aff Unit
testIdempotentRetry = do
  runtime <- newWithClock fixedClock
  first <- post runtime principal "api-open" (OpenAccount accountId)
    (encodeJson { owner: "sam" })
  second <- post runtime principal "api-open" (OpenAccount accountId)
    (encodeJson { owner: "sam" })
  assertEqual "retry status" first.status second.status
  assertEqual "retry body" (stringify first.body) (stringify second.body)
  history <- accountHistory runtime accountId
  assertEqual "retry appends once" 1 (length history)

testAuthorization :: Aff Unit
testAuthorization = do
  runtime <- newWithClock fixedClock
  _ <- post runtime principal "api-open" (OpenAccount accountId)
    (encodeJson { owner: "sam" })
  forbidden <- post runtime stranger "api-deposit" (DepositMoney accountId)
    (encodeJson { amount: 100 })
  assertEqual "a stranger is forbidden" 403 forbidden.status
  history <- accountHistory runtime accountId
  assertEqual "a forbidden command commits nothing" 1 (length history)
  visible <- summaryResponse runtime stranger accountId Nothing
  assertEqual "a stranger cannot read the summary" 403 visible.status
  adminRead <- summary runtime adminPrincipal accountId Nothing
  assertSummary "an admin can read the summary"
    { owner: Just "sam", balance: 0 }
    adminRead

testDomainRejection :: Aff Unit
testDomainRejection = do
  runtime <- newWithClock fixedClock
  _ <- post runtime principal "api-open" (OpenAccount accountId)
    (encodeJson { owner: "sam" })
  rejected <- post runtime principal "api-withdraw" (WithdrawMoney accountId)
    (encodeJson { amount: 50 })
  assertEqual "an overdraft is a conflict" 409 rejected.status
  assertEqual "the rejection is typed JSON" true
    (contains "insufficientFunds" (stringify rejected.body))

testRoutingErrors :: Aff Unit
testRoutingErrors = do
  runtime <- newWithClock fixedClock
  wrongMethod <- handle defaultBankJson runtime principal
    { method: GET
    , path: printRoute (OpenAccount accountId)
    , query: Map.empty
    , headers: Map.empty
    , body: jsonNull
    }
  assertEqual "wrong method" 405 wrongMethod.status
  missing <- summaryResponse runtime adminPrincipal (AccountId "missing") Nothing
  assertEqual "missing summary" 404 missing.status
  badKey <- handle defaultBankJson runtime principal
    { method: POST
    , path: printRoute (OpenAccount accountId)
    , query: Map.empty
    , headers: Map.empty
    , body: encodeJson { owner: "sam" }
    }
  assertEqual "missing idempotency key" 400 badKey.status
  badVersion <- summaryResponse runtime principal accountId (Just "not-a-version")
  assertEqual "invalid atLeast" 400 badVersion.status

testTypedClient :: Aff Unit
testTypedClient = do
  runtime <- newWithClock fixedClock
  let
    http :: Request -> Aff Response
    http = handle defaultBankJson runtime adminPrincipal

    bank = client http
  opened <- bank.openAccount (commandId "client-open") otherAccountId
    { owner: "ada" }
  case opened of
    Left failure -> throwError (error ("client open failed: " <> show failure))
    Right receipt -> do
      assertEqual "client open version" 1 receipt.version
      deposited <- bank.depositMoney (commandId "client-deposit") otherAccountId
        { amount: 40 }
      case deposited of
        Left failure -> throwError (error ("client deposit failed: " <> show failure))
        Right result ->
          assertEqual "client deposit version" 2 result.version
  read <- bank.summary otherAccountId (Just (versionFromInt 2))
  case read of
    Left failure -> throwError (error ("client summary failed: " <> show failure))
    Right summaryValue ->
      assertSummary "client summary"
        { owner: Just "ada", balance: 40 }
        (Right summaryValue)
  forbidden <- (client (handle defaultBankJson runtime stranger)).summary accountId Nothing
  case forbidden of
    Left (ApiFailure 403 _) -> pure unit
    other -> throwError (error ("expected a 403, got " <> show other))

testPolicyThroughApi :: Aff Unit
testPolicyThroughApi = do
  runtime <- newWithClock fixedClock
  registered <- post runtime adminPrincipal "api-register"
    (RegisterCustomer (CustomerId "c-api"))
    (encodeJson { name: "sam" })
  assertEqual "register customer" 200 registered.status
  policySummary <- summaryFor runtime (AccountId "c-api-main")
  case policySummary of
    Nothing -> throwError (error "policy did not open the main account")
    Just value ->
      assertSummary "policy through the API"
        { owner: Just "sam", balance: 0 }
        (Right value)

post
  :: BankRuntime
  -> BankPrincipal
  -> String
  -> BankRoute
  -> Json
  -> Aff Response
post runtime bankPrincipal idempotencyKey bankRoute body =
  handle defaultBankJson runtime bankPrincipal
    { method: POST
    , path: printRoute bankRoute
    , query: Map.empty
    , headers:
        Map.fromFoldable [ Tuple "idempotency-key" idempotencyKey ]
    , body
    }

summaryResponse
  :: BankRuntime
  -> BankPrincipal
  -> AccountId
  -> Maybe String
  -> Aff Response
summaryResponse runtime bankPrincipal target atLeast =
  handle defaultBankJson runtime bankPrincipal
    { method: GET
    , path: printRoute (AccountSummary target)
    , query: case atLeast of
        Nothing -> Map.empty
        Just raw -> Map.singleton "atLeast" raw
    , headers: Map.empty
    , body: jsonNull
    }

summary
  :: BankRuntime
  -> BankPrincipal
  -> AccountId
  -> Maybe String
  -> Aff (Either ClientError Summary)
summary runtime bankPrincipal target atLeast = do
  response <- summaryResponse runtime bankPrincipal target atLeast
  pure (decodeSummary response)

decodeSummary :: Response -> Either ClientError Summary
decodeSummary response =
  if response.status == 200 then
    case decodeJson response.body of
      Left failure -> Left (DecodeFailure (show failure))
      Right value -> Right value
  else
    Left (ApiFailure response.status (stringify response.body))

decodeCommand :: Response -> Aff CommandResult
decodeCommand response =
  case decodeJson response.body of
    Left failure ->
      throwError
        (error ("could not decode command response: " <> show failure))
    Right value -> pure value

versionFromInt :: Int -> Version
versionFromInt value =
  case parseVersion value of
    Right version -> version
    Left failure -> unsafeCrashWith ("bad test version: " <> show failure)

assertSummary
  :: String
  -> { owner :: Maybe String, balance :: Int }
  -> Either ClientError Summary
  -> Aff Unit
assertSummary label expected = case _ of
  Left failure -> throwError (error ("FAIL " <> label <> ": " <> show failure))
  Right actual -> do
    assertEqual (label <> ".owner") expected.owner actual.owner
    assertEqual (label <> ".balance") expected.balance actual.balance

contains :: String -> String -> Boolean
contains needle haystack =
  case String.indexOf (String.Pattern needle) haystack of
    Just _ -> true
    Nothing -> false
