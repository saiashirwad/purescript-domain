-- | JSON codecs derived from the spec. A command, event, or rejection is
-- | encoded as `{ "type": <label>, "value": <payload> }`; payloads use
-- | argonaut's record instances. Versioned payloads read older versions
-- | through upcasters.
module Domain.Codec
  ( Codec
  , codec
  , encode
  , decode
  , json
  , labelled
  , tagged
  , Upcaster
  , upcaster
  , versioned
  , class EncodeCases
  , encodeCases
  , class DecodeCases
  , decodeCases
  )
  where

import Prelude

import Data.Argonaut.Core (Json, fromObject, fromString, stringify, toObject, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (range)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (foldM)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import Domain (class Labelled, fromVariant, toVariant)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))

newtype Codec a = Codec { encode :: a -> Json, decode :: Json -> Either String a }

codec :: forall a. (a -> Json) -> (Json -> Either String a) -> Codec a
codec encode' decode' = Codec { encode: encode', decode: decode' }

encode :: forall a. Codec a -> a -> Json
encode (Codec c) = c.encode

decode :: forall a. Codec a -> Json -> Either String a
decode (Codec c) = c.decode

json :: forall a. EncodeJson a => DecodeJson a => Codec a
json = Codec { encode: encodeJson, decode: lmap printJsonDecodeError <<< decodeJson }

-- | The codec for `Command spec`, `Event spec`, or `Rejection spec`.
labelled ::
  forall a row rl.
  Labelled a row => RowToList row rl => EncodeCases rl row => DecodeCases rl row => Codec a
labelled = Codec
  { encode: encodeCases (Proxy :: Proxy rl) <<< toVariant
  , decode: map fromVariant <<< decodeCases (Proxy :: Proxy rl)
  }

-- | The wire shape of one labelled value.
tagged :: String -> Json -> Json
tagged label payload =
  fromObject (Object.fromFoldable [ Tuple "type" (fromString label), Tuple "value" payload ])

class EncodeCases :: RowList Type -> Row Type -> Constraint
class EncodeCases rl row | rl -> row where
  encodeCases :: Proxy rl -> Variant row -> Json

instance EncodeCases Nil () where
  encodeCases _ = case_

instance (IsSymbol l, EncodeJson a, EncodeCases t tail, Row.Cons l a tail row) =>
  EncodeCases (Cons l a t) row where
  encodeCases _ = on (Proxy :: Proxy l) (tagged (reflectSymbol (Proxy :: Proxy l)) <<< encodeJson)
    (encodeCases (Proxy :: Proxy t))

-- | Walks the row list with the full row in view, so each label injects
-- | straight into it.
class DecodeCases :: RowList Type -> Row Type -> Constraint
class DecodeCases rl row where
  decodeCases :: Proxy rl -> Json -> Either String (Variant row)

instance DecodeCases Nil row where
  decodeCases _ value = Left ("unknown label in " <> stringify value)

instance (IsSymbol l, DecodeJson a, DecodeCases t row, Row.Cons l a tail row) =>
  DecodeCases (Cons l a t) row where
  decodeCases _ value = do
    object <- note ("expected a labelled object, got " <> stringify value) (toObject value)
    label <- note "missing string field: type" (Object.lookup "type" object >>= toString)
    if label /= reflectSymbol (Proxy :: Proxy l)
    then decodeCases (Proxy :: Proxy t) value
    else do
      payload <- note "missing field: value" (Object.lookup "value" object)
      inj (Proxy :: Proxy l)
        <$> lmap (\e -> label <> ": " <> printJsonDecodeError e) (decodeJson payload)

-- | Rewrites a payload from version `from` to `from + 1`.
newtype Upcaster = Upcaster { from :: Int, upcast :: Json -> Either String Json }

upcaster :: Int -> (Json -> Either String Json) -> Upcaster
upcaster from upcast = Upcaster { from, upcast }

-- | Writes `{ "version": n, "payload": ... }` and reads any older version
-- | by applying upcasters in order.
versioned :: forall a. Int -> Array Upcaster -> Codec a -> Codec a
versioned current steps inner = Codec { encode: write, decode: read }
  where
  upcasters :: Map Int (Json -> Either String Json)
  upcasters = Map.fromFoldable (map (\(Upcaster step) -> Tuple step.from step.upcast) steps)

  write value = fromObject
    ( Object.fromFoldable
        [ Tuple "version" (encodeJson current), Tuple "payload" (encode inner value) ]
    )

  read value = do
    object <- note ("expected a versioned payload, got " <> stringify value) (toObject value)
    version <- note "missing field: version" (Object.lookup "version" object)
      >>= lmap printJsonDecodeError
      <<< decodeJson
    payload <- note "missing field: payload" (Object.lookup "payload" object)
    when (version > current)
      (Left ("payload version " <> show version <> " is newer than " <> show current))
    upgraded <-
      if version < current then foldM step payload (range version (current - 1)) else pure payload
    decode inner upgraded

  step payload version = case Map.lookup version upcasters of
    Nothing -> Left ("no upcaster from payload version " <> show version)
    Just upcast -> upcast payload
