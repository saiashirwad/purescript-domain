module Bank.Api
  ( BankPrincipal
  , BankJson
  , defaultBankJson
  , AccountEndpoint
  , CustomerEndpoint
  , BankApi
  , bankApi
  , handle
  , HttpClient
  , BankClient
  , ClientError(..)
  , CommandResult
  , client
  ) where

import Prelude

import Bank.Account
  ( AccountCommand
  , AccountEvent
  , AccountId
  , AccountRejection
  , depositMoney
  , openAccount
  , withdrawMoney
  )
import Bank.Customer
  ( CustomerEvent
  , CustomerId
  , CustomerRejection
  , registerCustomer
  )
import Bank.Codec
  ( accountEventCodec
  , accountRejectionCodec
  , customerEventCodec
  , customerRejectionCodec
  , statementLineCodec
  , summaryCodec
  )
import Bank.Route
  ( BankRoute(..)
  , parseRoute
  , printRoute
  , routeName
  )
import Bank.Runtime
  ( BankRuntime
  , sendAccount
  , sendCustomer
  , statementFor
  , summaryAtLeast
  , summaryFor
  )
import Bank.Views (StatementLine, Summary)
import Data.Argonaut.Core (Json, jsonNull, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Domain
  ( CommandId
  , Receipt
  , Version
  , commandIdText
  , parseCommandId
  , parseVersion
  , versionNumber
  )
import Domain.Codec (Codec, decode, encode, recordCodec)
import Domain.Auth (Predicate(..), Principal, allows, hasRole)
import Domain.Http
  ( ApiError(..)
  , Method(..)
  , Request
  , Response
  , errorResponse
  , header
  , jsonResponse
  , queryParam
  )
import Effect.Aff (Aff)

type BankPrincipal =
  { principal :: Principal
  , accounts :: Set AccountId
  }

admin :: Predicate BankPrincipal
admin = Predicate \bankPrincipal ->
  allows (hasRole "admin") bankPrincipal.principal

ownsAccount :: AccountId -> Predicate BankPrincipal
ownsAccount accountId = Predicate \bankPrincipal ->
  Set.member accountId bankPrincipal.accounts
    || allows admin bankPrincipal

-- | The wire encoders the API needs. The default implementation is small
-- | enough to read; `Bank.Codec` can supply the same record from the
-- | profunctor codecs when a caller wants the full schema layer.
type BankJson =
  { accountEvent :: AccountEvent -> Json
  , accountRejection :: AccountRejection -> Json
  , customerEvent :: CustomerEvent -> Json
  , customerRejection :: CustomerRejection -> Json
  , summary :: Summary -> Json
  , statementLine :: StatementLine -> Json
  , owner :: Json -> Either String { owner :: String }
  , amount :: Json -> Either String { amount :: Int }
  , name :: Json -> Either String { name :: String }
  }

defaultBankJson :: BankJson
defaultBankJson =
  { accountEvent: encode accountEventCodec
  , accountRejection: encode accountRejectionCodec
  , customerEvent: encode customerEventCodec
  , customerRejection: encode customerRejectionCodec
  , summary: encode summaryCodec
  , statementLine: encode statementLineCodec
  , owner:
      decode
        (recordCodec :: Codec { owner :: String } { owner :: String })
  , amount:
      decode
        (recordCodec :: Codec { amount :: Int } { amount :: Int })
  , name:
      decode
        (recordCodec :: Codec { name :: String } { name :: String })
  }

type AccountEndpoint =
  { method :: Method
  , authorize :: AccountId -> Predicate BankPrincipal
  , handle :: BankPrincipal -> AccountId -> Request -> Aff Response
  }

type CustomerEndpoint =
  { method :: Method
  , authorize :: Predicate BankPrincipal
  , handle :: BankPrincipal -> CustomerId -> Request -> Aff Response
  }

type BankApi =
  { openAccount :: AccountEndpoint
  , depositMoney :: AccountEndpoint
  , withdrawMoney :: AccountEndpoint
  , accountSummary :: AccountEndpoint
  , accountStatement :: AccountEndpoint
  , registerCustomer :: CustomerEndpoint
  }

bankApi :: BankJson -> BankRuntime -> BankApi
bankApi json runtime =
  { openAccount:
      { method: POST
      , authorize: ownsAccount
      , handle: \_ accountId request ->
          case json.owner request.body of
            Left message -> pure (errorResponse (BadRequest message))
            Right body ->
              runAccountCommand runtime json request accountId
                (openAccount body.owner)
      }
  , depositMoney:
      { method: POST
      , authorize: ownsAccount
      , handle: \_ accountId request ->
          case json.amount request.body of
            Left message -> pure (errorResponse (BadRequest message))
            Right body ->
              runAccountCommand runtime json request accountId
                (depositMoney body.amount)
      }
  , withdrawMoney:
      { method: POST
      , authorize: ownsAccount
      , handle: \_ accountId request ->
          case json.amount request.body of
            Left message -> pure (errorResponse (BadRequest message))
            Right body ->
              runAccountCommand runtime json request accountId
                (withdrawMoney body.amount)
      }
  , accountSummary:
      { method: GET
      , authorize: ownsAccount
      , handle: \_ accountId request ->
          case queryParam "atLeast" request of
            Nothing -> do
              result <- summaryFor runtime accountId
              case result of
                Nothing ->
                  pure (errorResponse (NotFound "no summary for this account"))
                Just summaryValue ->
                  pure (jsonResponse 200 (json.summary summaryValue))
            Just raw ->
              case Int.fromString raw of
                Nothing ->
                  pure
                    ( errorResponse
                        (BadRequest ("invalid atLeast version: " <> raw))
                    )
                Just number ->
                  case parseVersion number of
                    Left _ ->
                      pure
                        ( errorResponse
                            (BadRequest ("invalid atLeast version: " <> raw))
                        )
                    Right atLeast -> do
                      result <- summaryAtLeast runtime accountId atLeast
                      case result of
                        Nothing ->
                          pure
                            ( errorResponse
                                (Timeout "the summary view did not catch up")
                            )
                        Just summaryValue ->
                          pure (jsonResponse 200 (json.summary summaryValue))
      }
  , accountStatement:
      { method: GET
      , authorize: ownsAccount
      , handle: \_ accountId _ -> do
          lines <- statementFor runtime accountId
          pure (jsonResponse 200 (encodeJson (map json.statementLine lines)))
      }
  , registerCustomer:
      { method: POST
      , authorize: admin
      , handle: \_ customerId request ->
          case json.name request.body of
            Left message -> pure (errorResponse (BadRequest message))
            Right body ->
              withCommandId request \commandId -> do
                result <- sendCustomer runtime commandId customerId
                  (registerCustomer body.name)
                pure (customerCommandResult json result)
      }
  }

runAccountCommand
  :: BankRuntime
  -> BankJson
  -> Request
  -> AccountId
  -> AccountCommand
  -> Aff Response
runAccountCommand runtime json request accountId command =
  withCommandId request \commandId -> do
    result <- sendAccount runtime commandId accountId command
    pure (accountCommandResult json result)

accountCommandResult
  :: BankJson
  -> Either AccountRejection
       (Receipt AccountId AccountEvent { balance :: Int })
  -> Response
accountCommandResult json = case _ of
  Left rejection ->
    jsonResponse 409 (json.accountRejection rejection)
  Right receipt ->
    jsonResponse 200
      ( encodeJson
          { version: versionNumber receipt.version
          , events: map json.accountEvent receipt.events
          , reply: encodeJson receipt.reply
          }
      )

customerCommandResult
  :: BankJson
  -> Either CustomerRejection
       (Receipt CustomerId CustomerEvent Unit)
  -> Response
customerCommandResult json = case _ of
  Left rejection ->
    jsonResponse 409 (json.customerRejection rejection)
  Right receipt ->
    jsonResponse 200
      ( encodeJson
          { version: versionNumber receipt.version
          , events: map json.customerEvent receipt.events
          , reply: encodeJson receipt.reply
          }
      )

withCommandId :: Request -> (CommandId -> Aff Response) -> Aff Response
withCommandId request action =
  case commandIdFrom request of
    Left apiError -> pure (errorResponse apiError)
    Right commandId -> action commandId

commandIdFrom :: Request -> Either ApiError CommandId
commandIdFrom request = do
  raw <- maybe
    (Left (BadRequest "missing Idempotency-Key header"))
    Right
    (header "Idempotency-Key" request)
  lmap
    (const (BadRequest ("invalid Idempotency-Key: " <> raw)))
    (parseCommandId raw)

type ResolvedEndpoint =
  { method :: Method
  , authorize :: Predicate BankPrincipal
  , handle :: BankPrincipal -> Request -> Aff Response
  }

endpointFor :: BankApi -> BankRoute -> ResolvedEndpoint
endpointFor api = case _ of
  OpenAccount accountId -> resolveAccount api.openAccount accountId
  DepositMoney accountId -> resolveAccount api.depositMoney accountId
  WithdrawMoney accountId -> resolveAccount api.withdrawMoney accountId
  AccountSummary accountId -> resolveAccount api.accountSummary accountId
  AccountStatement accountId -> resolveAccount api.accountStatement accountId
  RegisterCustomer customerId ->
    { method: api.registerCustomer.method
    , authorize: api.registerCustomer.authorize
    , handle: \principal request ->
        api.registerCustomer.handle principal customerId request
    }

resolveAccount :: AccountEndpoint -> AccountId -> ResolvedEndpoint
resolveAccount endpoint accountId =
  { method: endpoint.method
  , authorize: endpoint.authorize accountId
  , handle: \principal request ->
      endpoint.handle principal accountId request
  }

handle
  :: BankJson
  -> BankRuntime
  -> BankPrincipal
  -> Request
  -> Aff Response
handle json runtime principal request =
  case parseRoute request.path of
    Left _ ->
      pure (errorResponse (NotFound ("no route for " <> request.path)))
    Right route -> do
      let endpoint = endpointFor (bankApi json runtime) route
      if request.method /= endpoint.method then
        pure
          ( errorResponse
              ( MethodNotAllowed
                  (routeName route <> " is " <> show endpoint.method)
              )
          )
      else if not (allows endpoint.authorize principal) then
        pure
          ( errorResponse
              (Forbidden "the principal may not perform this operation")
          )
      else endpoint.handle principal request

type HttpClient m = Request -> m Response

type CommandResult =
  { version :: Int
  , events :: Array Json
  , reply :: Json
  }

data ClientError
  = DomainRejection Json
  | ApiFailure Int String
  | DecodeFailure String

derive instance eqClientError :: Eq ClientError

instance showClientError :: Show ClientError where
  show = case _ of
    DomainRejection body -> "DomainRejection " <> stringify body
    ApiFailure status body ->
      "ApiFailure " <> show status <> " " <> body
    DecodeFailure message -> "DecodeFailure " <> show message

type BankClient m =
  { openAccount
      :: CommandId
      -> AccountId
      -> { owner :: String }
      -> m (Either ClientError CommandResult)
  , depositMoney
      :: CommandId
      -> AccountId
      -> { amount :: Int }
      -> m (Either ClientError CommandResult)
  , withdrawMoney
      :: CommandId
      -> AccountId
      -> { amount :: Int }
      -> m (Either ClientError CommandResult)
  , registerCustomer
      :: CommandId
      -> CustomerId
      -> { name :: String }
      -> m (Either ClientError CommandResult)
  , summary
      :: AccountId
      -> Maybe Version
      -> m (Either ClientError Summary)
  , statement
      :: AccountId
      -> m (Either ClientError (Array StatementLine))
  }

client :: forall m. Monad m => HttpClient m -> BankClient m
client http =
  { openAccount: \commandId accountId body ->
      command (OpenAccount accountId) commandId (encodeJson body)
  , depositMoney: \commandId accountId body ->
      command (DepositMoney accountId) commandId (encodeJson body)
  , withdrawMoney: \commandId accountId body ->
      command (WithdrawMoney accountId) commandId (encodeJson body)
  , registerCustomer: \commandId customerId body ->
      command (RegisterCustomer customerId) commandId (encodeJson body)
  , summary: \accountId atLeast -> do
      response <- http
        { method: GET
        , path: printRoute (AccountSummary accountId)
        , query: maybe Map.empty
            (\version ->
              Map.singleton "atLeast" (show (versionNumber version))
            )
            atLeast
        , headers: Map.empty
        , body: jsonNull
        }
      pure (decodeResponseBody response)
  , statement: \accountId -> do
      response <- http
        { method: GET
        , path: printRoute (AccountStatement accountId)
        , query: Map.empty
        , headers: Map.empty
        , body: jsonNull
        }
      pure (decodeResponseBody response)
  }
  where
  command route commandId body = do
    response <- http
      { method: POST
      , path: printRoute route
      , query: Map.empty
      , headers:
          Map.fromFoldable
            [ Tuple "idempotency-key" (commandIdText commandId) ]
      , body
      }
    pure (decodeCommand response)

  decodeCommand :: Response -> Either ClientError CommandResult
  decodeCommand response =
    case response.status of
      200 -> lmap (DecodeFailure <<< show) (decodeJson response.body)
      409 -> Left (DomainRejection response.body)
      status -> Left (ApiFailure status (stringify response.body))

decodeResponseBody
  :: forall a
   . DecodeJson a
  => Response
  -> Either ClientError a
decodeResponseBody response =
  case response.status of
    200 -> lmap (DecodeFailure <<< show) (decodeJson response.body)
    status -> Left (ApiFailure status (stringify response.body))
