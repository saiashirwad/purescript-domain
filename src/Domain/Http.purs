module Domain.Http
  ( Method(..)
  , Request
  , Response
  , ApiError(..)
  , methodFromString
  , methodToString
  , header
  , queryParam
  , jsonResponse
  , errorResponse
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.String as String

data Method
  = GET
  | POST

derive instance eqMethod :: Eq Method
derive instance ordMethod :: Ord Method

instance showMethod :: Show Method where
  show GET = "GET"
  show POST = "POST"

-- | A transport-neutral request. The Node adapter fills this record from an
-- | `IncomingMessage`; tests construct it directly. Header keys are lowercase.
type Request =
  { method :: Method
  , path :: String
  , query :: Map String String
  , headers :: Map String String
  , body :: Json
  }

type Response =
  { status :: Int
  , body :: Json
  }

data ApiError
  = BadRequest String
  | Unauthorized String
  | Forbidden String
  | NotFound String
  | MethodNotAllowed String
  | Conflict String
  | Unprocessable String
  | Timeout String

derive instance eqApiError :: Eq ApiError

instance showApiError :: Show ApiError where
  show = case _ of
    BadRequest message -> "BadRequest " <> show message
    Unauthorized message -> "Unauthorized " <> show message
    Forbidden message -> "Forbidden " <> show message
    NotFound message -> "NotFound " <> show message
    MethodNotAllowed message -> "MethodNotAllowed " <> show message
    Conflict message -> "Conflict " <> show message
    Unprocessable message -> "Unprocessable " <> show message
    Timeout message -> "Timeout " <> show message

methodFromString :: String -> Either String Method
methodFromString = case _ of
  "GET" -> Right GET
  "POST" -> Right POST
  other -> Left ("unsupported HTTP method: " <> other)

methodToString :: Method -> String
methodToString = show

header :: String -> Request -> Maybe String
header name request =
  Map.lookup (String.toLower name) request.headers

queryParam :: String -> Request -> Maybe String
queryParam name request =
  Map.lookup name request.query

jsonResponse :: Int -> Json -> Response
jsonResponse status body = { status, body }

errorResponse :: ApiError -> Response
errorResponse apiError =
  jsonResponse (statusOf apiError)
    (encodeJson { error: messageOf apiError })
  where
  statusOf = case _ of
    BadRequest _ -> 400
    Unauthorized _ -> 401
    Forbidden _ -> 403
    NotFound _ -> 404
    MethodNotAllowed _ -> 405
    Conflict _ -> 409
    Unprocessable _ -> 422
    Timeout _ -> 408

  messageOf = case _ of
    BadRequest message -> message
    Unauthorized message -> message
    Forbidden message -> message
    NotFound message -> message
    MethodNotAllowed message -> message
    Conflict message -> message
    Unprocessable message -> message
    Timeout message -> message
