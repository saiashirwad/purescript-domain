-- | JSON codecs derived from the spec. A command, event, or rejection is
-- | encoded as `{ "type": <label>, "value": <payload> }`; payloads use
-- | argonaut's record instances. Versioned payloads read older versions
-- | through upcasters.
module Domain.Codec (Codec, encode, decode, labelled, Upcaster, upcaster, versioned) where

import Prelude

import Data.Argonaut.Core (Json, fromObject, stringify, toObject, toString)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Domain.Internal.Codec (class DecodeCases, class EncodeCases, decodeLabelled, encodeLabelled)
import Domain.Internal.Kernel (class Labelled)
import Foreign.Object as Object
import Prim.RowList (class RowToList)

newtype Codec a = Codec { encode :: a -> Json, decode :: Json -> Either String a }

encode :: forall a. Codec a -> a -> Json
encode (Codec c) = c.encode

decode :: forall a. Codec a -> Json -> Either String a
decode (Codec c) = c.decode

-- | The codec for `Command spec`, `Event spec`, or `Rejection spec`.
labelled ::
  forall a row rl.
  Labelled a row => RowToList row rl => EncodeCases rl row => DecodeCases rl row => Codec a
labelled = Codec { encode: encodeLabelled, decode: read }
  where
  read value = do
    object <- note ("expected a labelled object, got " <> stringify value) (toObject value)
    label <- note "missing string field: type" (Object.lookup "type" object >>= toString)
    decodeLabelled label =<< note "missing field: value" (Object.lookup "value" object)

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
    decode inner =<< upgrade version payload

  -- One step per upcaster, so a version below the oldest one fails on its
  -- first lookup instead of counting up to `current`.
  upgrade version payload
    | version == current = pure payload
    | otherwise = case Map.lookup version upcasters of
      Nothing -> Left ("no upcaster from payload version " <> show version)
      Just upcast -> upgrade (version + 1) =<< upcast payload
