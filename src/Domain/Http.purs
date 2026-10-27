-- | An HTTP boundary derived from the context. Routes, body codecs, and
-- | status codes all come from the context's labels and specs:
-- |
-- | - `POST /<aggregate>/<id>/<command>` with an `Idempotency-Key` header
-- |   sends the command. 200 returns the receipt, 409 the rejection.
-- | - `GET /<view>/<key>` reads the view's row.
-- |
-- | The only hand-written input is `guards`: one `principal -> id ->
-- | Boolean` per aggregate label, and one `principal -> key -> Boolean` per
-- | view label.
module Domain.Http (Method(..), Request, Response, serve, class Routes, routes) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (uncons)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap)
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Domain
  (class Spec, Aggregate, Command, Event, Policy, Rejection, View, parseCommandId, versionNumber)
import Domain.Codec (class DecodeCases, class EncodeCases, decode, encode, labelled, tagged)
import Domain.Labels (class Field)
import Domain.Runtime (AggregateHandle, ViewHandle)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as Record
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))

data Method = GET | POST

derive instance Eq Method

-- | Transport-neutral. A server adapter fills it; header keys are lowercase.
type Request =
  { method :: Method
  , path :: String
  , query :: Map String String
  , headers :: Map String String
  , body :: Json
  }

type Response = { status :: Int, body :: Json }

type Route principal =
  { method :: Method, handle :: principal -> String -> Array String -> Request -> Aff Response }

serve ::
  forall ctx rl guards rt principal.
  RowToList ctx rl =>
  Routes rl guards rt principal =>
  Record ctx ->
  Record guards ->
  Record rt ->
  principal ->
  Request ->
  Aff Response
serve _ guards rt = \principal request ->
  case Array.filter (_ /= "") (split (Pattern "/") request.path) # uncons of
    Just { head: label, tail }
      | Just { head: id, tail: rest } <- uncons tail -> case Object.lookup label table of
        Nothing -> pure (failure 404 ("no route for " <> request.path))
        Just route
          | route.method /= request.method ->
            pure (failure 405 (label <> " does not accept this method"))
          | otherwise -> route.handle principal id rest request
    _ -> pure (failure 404 ("no route for " <> request.path))
  where
  table = routes (Proxy :: Proxy rl) guards rt

failure :: Int -> String -> Response
failure status message = { status, body: encodeJson { error: message } }

forbidden :: Response
forbidden = failure 403 "the principal may not perform this operation"

commandRoute ::
  forall spec id s c e r b crl erl rrl principal.
  Spec spec id s c e r b =>
  Newtype id String =>
  RowToList c crl =>
  RowToList e erl =>
  RowToList r rrl =>
  EncodeCases crl c =>
  DecodeCases crl c =>
  EncodeCases erl e =>
  DecodeCases erl e =>
  EncodeCases rrl r =>
  DecodeCases rrl r =>
  EncodeJson b =>
  (principal -> id -> Boolean) ->
  AggregateHandle spec id b ->
  Route principal
commandRoute guard handle =
  { method: POST
  , handle: \principal rawId rest request -> do
      let
        id = wrap rawId
      case rest, Map.lookup "idempotency-key" request.headers >>= parseCommandId of
        _, _
          | not (guard principal id) -> pure forbidden
        [ name ], Just commandId -> case decode labelled (tagged name request.body) of
          Left message -> pure (failure 400 message)
          Right (sent :: Command spec) -> do
            result <- handle.send commandId id sent
            pure case result of
              Left (refused :: Rejection spec) -> { status: 409, body: encode labelled refused }
              Right receipt ->
                { status: 200
                , body: encodeJson
                    { version: versionNumber receipt.version
                    , events: map (encode labelled :: Event spec -> Json) receipt.events
                    , reply: encodeJson receipt.reply
                    }
                }
        [ _ ], Nothing -> pure (failure 400 "missing or empty Idempotency-Key header")
        _, _ -> pure (failure 404 ("no route for " <> request.path))
  }

viewRoute ::
  forall k v principal.
  Newtype k String =>
  EncodeJson v =>
  (principal -> k -> Boolean) ->
  ViewHandle k v ->
  Route principal
viewRoute guard handle =
  { method: GET
  , handle: \principal rawKey rest request -> do
      let
        key = wrap rawKey
      if not (guard principal key)
      then pure forbidden
      else if rest /= []
      then pure (failure 404 ("no route for " <> request.path))
      else maybe (failure 404 "nothing recorded for this key") ok <$> handle.read key
  }
  where
  ok row = { status: 200, body: encodeJson row }

-- | One route per aggregate and view label. A missing guard is a compile
-- | error naming the label.
class Routes :: RowList Type -> Row Type -> Row Type -> Type -> Constraint
class Routes rl guards rt principal where
  routes :: Proxy rl -> Record guards -> Record rt -> Object (Route principal)

instance Routes Nil guards rt principal where
  routes _ _ _ = Object.empty

instance ( IsSymbol l
         , Spec spec id s c e r b
         , Newtype id String
         , RowToList guards grl
         , Field "serve needs a guard for " l grl (principal -> id -> Boolean)
         , Row.Cons l (AggregateHandle spec id b) rtTail rt
         , RowToList c crl
         , RowToList e erl
         , RowToList r rrl
         , EncodeCases crl c
         , DecodeCases crl c
         , EncodeCases erl e
         , DecodeCases erl e
         , EncodeCases rrl r
         , DecodeCases rrl r
         , EncodeJson b
         , Routes t guards rt principal
         ) =>
  Routes (Cons l (Aggregate spec) t) guards rt principal where
  routes _ guards rt = Object.insert (reflectSymbol (Proxy :: Proxy l))
    ( commandRoute (unsafeGet (reflectSymbol (Proxy :: Proxy l)) guards)
        (Record.get (Proxy :: Proxy l) rt)
    )
    (routes (Proxy :: Proxy t) guards rt)
else instance ( IsSymbol l
              , Newtype k String
              , EncodeJson v
              , RowToList guards grl
              , Field "serve needs a guard for " l grl (principal -> k -> Boolean)
              , Row.Cons l (ViewHandle k v) rtTail rt
              , Routes t guards rt principal
              ) =>
  Routes (Cons l (View spec k v) t) guards rt principal where
  routes _ guards rt = Object.insert (reflectSymbol (Proxy :: Proxy l))
    ( viewRoute (unsafeGet (reflectSymbol (Proxy :: Proxy l)) guards)
        (Record.get (Proxy :: Proxy l) rt)
    )
    (routes (Proxy :: Proxy t) guards rt)
else instance Routes t guards rt principal =>
  Routes (Cons l (Policy src tgt) t) guards rt principal where
  routes _ = routes (Proxy :: Proxy t)
