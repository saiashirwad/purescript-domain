-- | The two halves of the labelled codec. Each needs only its own
-- | direction's argonaut instances, so the HTTP boundary asks an event
-- | payload for `EncodeJson` alone.
module Domain.Internal.Codec
  ( encodeLabelled
  , decodeLabelled
  , class EncodeCases
  , class DecodeCases
  )
  where

import Prelude

import Data.Argonaut.Core (Json, fromObject, fromString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import Domain.Internal.Kernel (class Labelled, fromVariant, toVariant)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))

-- | `{ "type": <label>, "value": <payload> }`.
encodeLabelled ::
  forall a row rl. Labelled a row => RowToList row rl => EncodeCases rl row => a -> Json
encodeLabelled = encodeCases (Proxy :: Proxy rl) <<< toVariant

-- | Reads a label and a payload that arrive apart, as the command name and
-- | body of an HTTP request do.
decodeLabelled ::
  forall a row rl.
  Labelled a row => RowToList row rl => DecodeCases rl row => String -> Json -> Either String a
decodeLabelled label = map fromVariant <<< decodeCase (Proxy :: Proxy rl) label

class EncodeCases :: RowList Type -> Row Type -> Constraint
class EncodeCases rl row | rl -> row where
  encodeCases :: Proxy rl -> Variant row -> Json

instance EncodeCases Nil () where
  encodeCases _ = case_

instance (IsSymbol l, EncodeJson a, EncodeCases t tail, Row.Cons l a tail row) =>
  EncodeCases (Cons l a t) row where
  encodeCases _ = on (Proxy :: Proxy l) (tagged <<< encodeJson) (encodeCases (Proxy :: Proxy t))
    where
    tagged payload = fromObject
      ( Object.fromFoldable
          [ Tuple "type" (fromString (reflectSymbol (Proxy :: Proxy l))), Tuple "value" payload ]
      )

-- | Walks the row list with the full row in view, so each label injects
-- | straight into it.
class DecodeCases :: RowList Type -> Row Type -> Constraint
class DecodeCases rl row where
  decodeCase :: Proxy rl -> String -> Json -> Either String (Variant row)

instance DecodeCases Nil row where
  decodeCase _ label _ = Left ("unknown label " <> label)

instance (IsSymbol l, DecodeJson a, DecodeCases t row, Row.Cons l a tail row) =>
  DecodeCases (Cons l a t) row where
  decodeCase _ label payload
    | label == reflectSymbol (Proxy :: Proxy l) = inj (Proxy :: Proxy l)
      <$> lmap (\e -> label <> ": " <> printJsonDecodeError e) (decodeJson payload)
    | otherwise = decodeCase (Proxy :: Proxy t) label payload
