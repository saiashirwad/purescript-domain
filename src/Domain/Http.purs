-- | An HTTP boundary derived from the context. Routes, body codecs, and
-- | status codes all come from the context's labels and specs:
-- |
-- | - `POST /<aggregate>/<id>/<command>` with an `Idempotency-Key` header
-- |   sends the command. 200 returns the receipt, 409 the rejection, and
-- |   422 says the key already names another command.
-- | - `GET /<view>/<key>` reads the view's row.
-- |
-- | The only hand-written input is `guards`. An aggregate label takes a
-- | record with one `principal -> id -> Boolean` per command label, so a new
-- | command needs its own decision about who may send it. A view label takes
-- | one `principal -> key -> Boolean`. A failure inside the runtime is a 500
-- | whose body says nothing about the failure.
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
import Domain.Codec (class DecodeCases, class EncodeCases, decodeLabelled, encodeLabelled)
import Domain.Labels (class Covers, class Field)
import Domain.Runtime (AggregateHandle, Refusal(..), ViewHandle)
import Effect.Aff (Aff, catchError)
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
  catchError (dispatch principal request) \_ -> pure (failure 500 "internal error")
  where
  table = routes (Proxy :: Proxy rl) guards rt

  dispatch principal request =
    case Array.filter (_ /= "") (split (Pattern "/") request.path) # uncons of
      Just { head: label, tail }
        | Just { head: id, tail: rest } <- uncons tail -> case Map.lookup label table of
          Nothing -> pure (failure 404 ("no route for " <> request.path))
          Just route
            | route.method /= request.method ->
              pure (failure 405 (label <> " does not accept this method"))
            | otherwise -> route.handle principal id rest request
      _ -> pure (failure 404 ("no route for " <> request.path))

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
  DecodeCases crl c =>
  EncodeCases erl e =>
  EncodeCases rrl r =>
  EncodeJson b =>
  Map String (principal -> id -> Boolean) ->
  AggregateHandle spec id b ->
  Route principal
commandRoute guards handle =
  { method: POST
  , handle: \principal rawId rest request -> do
      let
        id = wrap rawId
      case rest of
        [ name ] -> case Map.lookup name guards of
          Nothing -> pure (failure 400 ("unknown command " <> name))
          Just guard
            | not (guard principal id) -> pure forbidden
            | otherwise -> case Map.lookup "idempotency-key" request.headers >>= parseCommandId of
              Nothing -> pure (failure 400 "missing or empty Idempotency-Key header")
              Just commandId -> case decodeLabelled name request.body of
                Left message -> pure (failure 400 message)
                Right (sent :: Command spec) -> respond <$> handle.send commandId id sent
        _ -> pure (failure 404 ("no route for " <> request.path))
  }
  where
  respond = case _ of
    Left (Rejected (refused :: Rejection spec)) -> { status: 409, body: encodeLabelled refused }
    Left (Reused reused) ->
      failure 422 ("the Idempotency-Key already names the command " <> reused.command)
    Right receipt ->
      { status: 200
      , body: encodeJson
          { version: versionNumber receipt.version
          , events: map (encodeLabelled :: Event spec -> Json) receipt.events
          , reply: encodeJson receipt.reply
          }
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

-- | One route per aggregate and view label. A missing guard, or an
-- | aggregate's guard record that misses or misspells a command, is a
-- | compile error naming the label.
class Routes :: RowList Type -> Row Type -> Row Type -> Type -> Constraint
class Routes rl guards rt principal where
  routes :: Proxy rl -> Record guards -> Record rt -> Map String (Route principal)

instance Routes Nil guards rt principal where
  routes _ _ _ = Map.empty

instance ( IsSymbol l
         , Spec spec id s c e r b
         , Newtype id String
         , RowToList guards grl
         , Field "serve needs a guard for " l grl (Record commandGuards)
         , RowToList commandGuards cgl
         , RowToList c crl
         , Covers "a guard names an undeclared command " cgl crl
         , Covers "serve needs a guard for the command " crl cgl
         , GuardTable cgl commandGuards principal id
         , Row.Cons l (AggregateHandle spec id b) rtTail rt
         , RowToList e erl
         , RowToList r rrl
         , DecodeCases crl c
         , EncodeCases erl e
         , EncodeCases rrl r
         , EncodeJson b
         , Routes t guards rt principal
         ) =>
  Routes (Cons l (Aggregate spec) t) guards rt principal where
  routes _ guards rt = Map.insert (reflectSymbol (Proxy :: Proxy l))
    ( commandRoute (guardTable (Proxy :: Proxy cgl) commandGuards)
        (Record.get (Proxy :: Proxy l) rt)
    )
    (routes (Proxy :: Proxy t) guards rt)
    where
    commandGuards :: Record commandGuards
    commandGuards = unsafeGet (reflectSymbol (Proxy :: Proxy l)) guards
else instance ( IsSymbol l
              , Newtype k String
              , EncodeJson v
              , RowToList guards grl
              , Field "serve needs a guard for " l grl (principal -> k -> Boolean)
              , Row.Cons l (ViewHandle k v) rtTail rt
              , Routes t guards rt principal
              ) =>
  Routes (Cons l (View spec k v) t) guards rt principal where
  routes _ guards rt = Map.insert (reflectSymbol (Proxy :: Proxy l))
    ( viewRoute (unsafeGet (reflectSymbol (Proxy :: Proxy l)) guards)
        (Record.get (Proxy :: Proxy l) rt)
    )
    (routes (Proxy :: Proxy t) guards rt)
else instance Routes t guards rt principal =>
  Routes (Cons l (Policy src tgt) t) guards rt principal where
  routes _ = routes (Proxy :: Proxy t)

-- | An aggregate's guards by command label. Each label comes from the
-- | record's own row list, so `Row.Cons` only fixes the guard's type.
class GuardTable :: RowList Type -> Row Type -> Type -> Type -> Constraint
class GuardTable rl guards principal id where
  guardTable :: Proxy rl -> Record guards -> Map String (principal -> id -> Boolean)

instance GuardTable Nil guards principal id where
  guardTable _ _ = Map.empty

instance
  ( IsSymbol l
  , Row.Cons l (principal -> id -> Boolean) tail guards
  , GuardTable t guards principal id
  ) =>
  GuardTable (Cons l g t) guards principal id where
  guardTable _ guards = Map.insert (reflectSymbol (Proxy :: Proxy l))
    (Record.get (Proxy :: Proxy l) guards)
    (guardTable (Proxy :: Proxy t) guards)
