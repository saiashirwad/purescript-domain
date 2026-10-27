-- | An HTTP boundary derived from the context. Routes, body codecs, and
-- | status codes all come from the context's labels and types:
-- |
-- | - `POST /<aggregate>/<id>/<command>` with an `Idempotency-Key` header
-- |   sends the command. 200 returns the receipt, 409 the rejection, and
-- |   422 says the key already names another command.
-- | - `GET /<view>/<key>` reads the view's row.
-- |
-- | The only hand-written input is `guards`. An aggregate label takes one
-- | `principal -> id -> command -> Boolean`; match on the command without a
-- | wildcard and a new command needs its own decision about who may send
-- | it. A view label takes one `principal -> key -> Boolean`. A failure
-- | inside the runtime is a 500 whose body says nothing about the failure.
module Domain.Http (Method(..), Request, Response, serve) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (uncons)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap)
import Data.String (Pattern(..), split)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Domain.Internal.Id (parseCommandId, versionNumber)
import Domain.Internal.Kernel (Aggregate, Policy, View)
import Domain.Internal.Labels (class DecodeCases, class EncodeCases, decodeLabelled, encodeLabelled)
import Domain.Runtime (AggregateHandle, Refusal(..), ViewHandle)
import Effect.Aff (Aff, catchError)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.TypeError (class Fail, Beside, QuoteLabel, Text)
import Record as Record
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))

data Method = GET | POST

derive instance Eq Method

-- | Transport-neutral. A server adapter fills it; header keys are lowercase.
type Request = { method :: Method, path :: String, headers :: Map String String, body :: Json }

type Response = { status :: Int, body :: Json }

type Route principal =
  { method :: Method, handle :: principal -> String -> Array String -> Request -> Aff Response }

serve ::
  forall ctx rl guards grl rt principal.
  RowToList ctx rl =>
  RowToList guards grl =>
  KnownGuards grl rl =>
  Routes rl guards rt principal =>
  Record ctx ->
  Record guards ->
  Record rt ->
  principal ->
  Request ->
  Aff Response
serve _ guards rt = \principal request -> catchError (dispatch principal request) \_ ->
  pure (failure 500 "internal error")
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
  forall id c crep e erep r rrep b principal.
  Newtype id String =>
  Generic c crep =>
  DecodeCases crep =>
  Generic e erep =>
  EncodeCases erep =>
  Generic r rrep =>
  EncodeCases rrep =>
  EncodeJson b =>
  (principal -> id -> c -> Boolean) ->
  AggregateHandle id c e r b ->
  Route principal
commandRoute guard handle =
  { method: POST
  , handle: \principal rawId rest request -> do
      let
        id = wrap rawId
      case rest of
        [ name ] -> case decodeLabelled name request.body of
          Left message -> pure (failure 400 message)
          Right sent
            | not (guard principal id sent) -> pure forbidden
            | otherwise -> case Map.lookup "idempotency-key" request.headers >>= parseCommandId of
              Nothing -> pure (failure 400 "missing or empty Idempotency-Key header")
              Just commandId -> respond <$> handle.send commandId id sent
        _ -> pure (failure 404 ("no route for " <> request.path))
  }
  where
  respond = case _ of
    Left (Rejected refused) -> { status: 409, body: encodeLabelled refused }
    Left (Reused reused) ->
      failure 422 ("the Idempotency-Key already names the command " <> reused.command)
    Right receipt ->
      { status: 200
      , body: encodeJson
          { version: versionNumber receipt.version
          , events: map encodeLabelled receipt.events
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

-- | One route per aggregate and view label. A missing guard is a compile
-- | error naming the label.
class Routes :: RowList Type -> Row Type -> Row Type -> Type -> Constraint
class Routes rl guards rt principal where
  routes :: Proxy rl -> Record guards -> Record rt -> Map String (Route principal)

instance Routes Nil guards rt principal where
  routes _ _ _ = Map.empty

instance ( IsSymbol l
         , Newtype id String
         , RowToList guards grl
         , HasGuard l grl (principal -> id -> c -> Boolean)
         , Row.Cons l (AggregateHandle id c e r b) rtTail rt
         , Generic c crep
         , DecodeCases crep
         , Generic e erep
         , EncodeCases erep
         , Generic r rrep
         , EncodeCases rrep
         , EncodeJson b
         , Routes t guards rt principal
         ) =>
  Routes (Cons l (Aggregate id c e r s b) t) guards rt principal where
  routes _ guards rt = Map.insert (reflectSymbol (Proxy :: Proxy l))
    (commandRoute (guardOf (Proxy :: Proxy l) guards) (Record.get (Proxy :: Proxy l) rt))
    (routes (Proxy :: Proxy t) guards rt)
else instance ( IsSymbol l
              , Newtype k String
              , EncodeJson v
              , RowToList guards grl
              , HasGuard l grl (principal -> k -> Boolean)
              , Row.Cons l (ViewHandle k v) rtTail rt
              , Routes t guards rt principal
              ) =>
  Routes (Cons l (View i k v) t) guards rt principal where
  routes _ guards rt = Map.insert (reflectSymbol (Proxy :: Proxy l))
    (viewRoute (guardOf (Proxy :: Proxy l) guards) (Record.get (Proxy :: Proxy l) rt))
    (routes (Proxy :: Proxy t) guards rt)
else instance Routes t guards rt principal =>
  Routes (Cons l (Policy i id c) t) guards rt principal where
  routes _ = routes (Proxy :: Proxy t)

-- | Proves the guards name `l` with type `g`. A `Row.Cons` constraint would
-- | report a missing guard as a unification of the whole guards row, so
-- | the guard is read with `unsafeGet` under this proof instead.
class HasGuard :: Symbol -> RowList Type -> Type -> Constraint
class HasGuard l rl g | l rl -> g

instance HasGuard l (Cons l g t) g
else instance HasGuard l t g => HasGuard l (Cons m x t) g
else instance Fail (Beside (Text "serve needs a guard for ") (QuoteLabel l)) => HasGuard l Nil g

guardOf :: forall l guards g. IsSymbol l => Proxy l -> Record guards -> g
guardOf label = unsafeGet (reflectSymbol label)

-- | Every guard names an aggregate or view of the context, so a misspelled
-- | or stale guard is a compile error rather than a rule that never runs.
class KnownGuards :: RowList Type -> RowList Type -> Constraint
class KnownGuards grl rl

instance KnownGuards Nil rl
instance (Routable l rl, KnownGuards t rl) => KnownGuards (Cons l g t) rl

class Routable :: Symbol -> RowList Type -> Constraint
class Routable l rl

instance Routable l (Cons l (Aggregate id c e r s b) t)
else instance Routable l (Cons l (View i k v) t)
else instance Routable l t => Routable l (Cons m x t)
else instance Fail (Beside (Text "a guard names no aggregate or view of the context: ") (QuoteLabel l)) =>
  Routable l Nil
