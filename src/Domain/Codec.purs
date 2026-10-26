-- | Codecs between domain values and JSON.
-- |
-- | `Codec a b` encodes values of `a` to `Json` and decodes `Json` into values
-- | of `b`. The wire type is fixed to `Json`, which makes `Codec` a
-- | `Profunctor`: `encode` consumes `a` contravariantly and `decode` produces
-- | `b` covariantly, so `dimap` needs no inverses.
module Domain.Codec
  ( Codec(..)
  , encode
  , decode
  , lmap
  , rmap
  , dimap
  , jsonCodec
  , recordCodec
  , variantCodec
  , class EncodeVariant
  , encodeVariant
  , class DecodeVariant
  , decodeVariant
  , envelopeCodec
  , Upcaster
  , upcaster
  , Schema
  , schema
  , schemaVersion
  , versionedCodec
  , encodeVersioned
  , decodeVersioned
  ) where

import Prelude

import Data.Argonaut.Core
  ( Json
  , fromObject
  , fromString
  , stringify
  , toObject
  , toString
  )
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Bifunctor as Bifunctor
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Profunctor (class Profunctor)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import Domain
  ( Envelope
  , commandIdText
  , parseCommandId
  , parseSequence
  , parseStreamId
  , sequenceNumber
  , streamIdText
  )
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList as RowList
import Type.Proxy (Proxy(..))

newtype Codec a b = Codec
  { encode :: a -> Json
  , decode :: Json -> Either String b
  }

encode :: forall a b. Codec a b -> a -> Json
encode (Codec codec) = codec.encode

decode :: forall a b. Codec a b -> Json -> Either String b
decode (Codec codec) = codec.decode

dimap :: forall a b c d. (a -> b) -> (c -> d) -> Codec b c -> Codec a d
dimap = dimapCodec

dimapCodec :: forall a b c d. (a -> b) -> (c -> d) -> Codec b c -> Codec a d
dimapCodec f g (Codec codec) = Codec
  { encode: codec.encode <<< f
  , decode: map g <<< codec.decode
  }

instance profunctorCodec :: Profunctor Codec where
  dimap = dimapCodec

lmap :: forall a b c. (a -> b) -> Codec b c -> Codec a c
lmap f = dimap f identity

rmap :: forall a b c. (b -> c) -> Codec a b -> Codec a c
rmap g = dimap identity g

-- | Any type argonaut-codecs knows how to write can be encoded and any type it
-- | knows how to read can be decoded. Instantiate both sides with the same type
-- | for a round-tripping codec.
jsonCodec :: forall a b. EncodeJson a => DecodeJson b => Codec a b
jsonCodec = Codec
  { encode: encodeJson
  , decode: Bifunctor.lmap printJsonDecodeError <<< decodeJson
  }

-- | Records are encoded by their field names using argonaut-codecs' row-generic
-- | instances.
recordCodec
  :: forall row
   . EncodeJson (Record row)
  => DecodeJson (Record row)
  => Codec (Record row) (Record row)
recordCodec = jsonCodec

-- | A variant is encoded as `{ "type": <label>, "value": <payload> }`.
-- | Decoding tries the labels in row order.
variantCodec
  :: forall row rl
   . RowList.RowToList row rl
  => EncodeVariant rl row
  => DecodeVariant rl row
  => Codec (Variant row) (Variant row)
variantCodec = Codec
  { encode: encodeVariant (Proxy :: Proxy rl)
  , decode: decodeVariant (Proxy :: Proxy rl)
  }

class EncodeVariant (rl :: RowList.RowList Type) (row :: Row Type) | rl -> row where
  encodeVariant :: Proxy rl -> Variant row -> Json

instance encodeVariantNil :: EncodeVariant RowList.Nil () where
  encodeVariant _ = case_

instance
  ( IsSymbol sym
  , EncodeJson a
  , EncodeVariant tail tailRow
  , Row.Cons sym a tailRow row
  ) =>
  EncodeVariant (RowList.Cons sym a tail) row where
  encodeVariant _ =
    on (Proxy :: Proxy sym)
      (taggedVariant (reflectSymbol (Proxy :: Proxy sym)) <<< encodeJson)
      (encodeVariant (Proxy :: Proxy tail))

-- | The decoder walks the row list while keeping the full row in view, so the
-- | last label can inject straight into it. Running out of labels is an error,
-- | which is why the `Nil` instance is polymorphic in the row.
class DecodeVariant (rl :: RowList.RowList Type) (row :: Row Type) where
  decodeVariant :: Proxy rl -> Json -> Either String (Variant row)

instance decodeVariantNil :: DecodeVariant RowList.Nil row where
  decodeVariant _ json =
    Left ("unexpected variant value: " <> stringify json)

instance
  ( IsSymbol sym
  , DecodeJson a
  , DecodeVariant tail row
  , Row.Cons sym a tailRow row
  ) =>
  DecodeVariant (RowList.Cons sym a tail) row where
  decodeVariant _ json = do
    { tag, payload } <- decodeTagged json
    if tag == reflectSymbol (Proxy :: Proxy sym) then
      inj (Proxy :: Proxy sym) <$> withLabel "value" decodeJson payload
    else
      decodeVariant (Proxy :: Proxy tail) json

taggedVariant :: String -> Json -> Json
taggedVariant tag payload = fromObject
  ( Object.fromFoldable
      [ Tuple "type" (fromString tag), Tuple "value" payload ]
  )

decodeTagged :: Json -> Either String { tag :: String, payload :: Json }
decodeTagged json = do
  object <- note ("expected an object, got " <> stringify json) (toObject json)
  tagJson <- note "missing field: type" (Object.lookup "type" object)
  tag <- note "field type: expected a string" (toString tagJson)
  payload <- note "missing field: value" (Object.lookup "value" object)
  pure { tag, payload }

-- | An event envelope: the aggregate id and address plus the event itself.
-- | `at` is encoded as epoch milliseconds.
envelopeCodec
  :: forall id e
   . Codec id id
  -> Codec e e
  -> Codec (Envelope id e) (Envelope id e)
envelopeCodec idCodec eventCodec = Codec
  { encode: \envelope ->
      fromObject
        ( Object.fromFoldable
            [ Tuple "id" (encode idCodec envelope.id)
            , Tuple "stream" (fromString (streamIdText envelope.stream))
            , Tuple "seq" (encodeJson (sequenceNumber envelope.seq))
            , Tuple "commandId"
                (fromString (commandIdText envelope.commandId))
            , Tuple "at" (encode instantCodec envelope.at)
            , Tuple "event" (encode eventCodec envelope.event)
            ]
        )
  , decode: \json -> do
      object <- note ("expected an envelope object, got " <> stringify json) (toObject json)
      id <- withField "id" object (decode idCodec)
      stream <-
        withField "stream" object
          (Bifunctor.lmap show <<< parseStreamId <=< expectString)
      seq <- withField "seq" object \seqJson -> do
        value <- Bifunctor.lmap printJsonDecodeError (decodeJson seqJson)
        Bifunctor.lmap show (parseSequence value)
      commandId <-
        withField "commandId" object
          (Bifunctor.lmap show <<< parseCommandId <=< expectString)
      at <- withField "at" object (decode instantCodec)
      event <- withField "event" object (decode eventCodec)
      pure { id, stream, seq, commandId, at, event }
  }

instantCodec :: Codec Instant Instant
instantCodec = Codec
  { encode: \value -> encodeJson (milliseconds value)
  , decode: \json -> do
      value <-
        Bifunctor.lmap printJsonDecodeError (decodeJson json)
          :: Either String Number
      note ("instant out of range: " <> show value) (instant (Milliseconds value))
  }

milliseconds :: Instant -> Number
milliseconds instantValue = case unInstant instantValue of
  Milliseconds value -> value

withField
  :: forall a
   . String
  -> Object.Object Json
  -> (Json -> Either String a)
  -> Either String a
withField key object decodeValue = do
  value <- note ("missing field: " <> key) (Object.lookup key object)
  Bifunctor.lmap (\message -> key <> ": " <> message) (decodeValue value)

withLabel
  :: forall a
   . String
  -> (Json -> Either JsonDecodeError a)
  -> Json
  -> Either String a
withLabel label decodeValue =
  Bifunctor.lmap (\error -> label <> ": " <> printJsonDecodeError error) <<< decodeValue

expectString :: Json -> Either String String
expectString json = note ("expected a string, got " <> stringify json) (toString json)

-- | One step in a payload's version history. `upcast` rewrites a payload from
-- | version `from` to version `from + 1`.
newtype Upcaster = Upcaster
  { from :: Int
  , upcast :: Json -> Either String Json
  }

upcaster :: Int -> (Json -> Either String Json) -> Upcaster
upcaster from upcast = Upcaster { from, upcast }

-- | The current version of a payload together with the upcasters needed to
-- | read older versions. A versioned payload is encoded as
-- | `{ "version": <n>, "payload": <json> }`.
newtype Schema a = Schema
  { version :: Int
  , codec :: Codec a a
  , upcasters :: Map Int Upcaster
  }

schema :: forall a. Int -> Codec a a -> Array Upcaster -> Schema a
schema version codec steps =
  Schema
    { version
    , codec
    , upcasters:
        Map.fromFoldable
          (map (\step@(Upcaster definition) -> Tuple definition.from step) steps)
    }

schemaVersion :: forall a. Schema a -> Int
schemaVersion (Schema definition) = definition.version

versionedCodec :: forall a. Schema a -> Codec a a
versionedCodec schemaValue = Codec
  { encode: encodeVersioned schemaValue
  , decode: decodeVersioned schemaValue
  }

encodeVersioned :: forall a. Schema a -> a -> Json
encodeVersioned (Schema definition) value =
  fromObject
    ( Object.fromFoldable
        [ Tuple "version" (encodeJson definition.version)
        , Tuple "payload" (encode definition.codec value)
        ]
    )

decodeVersioned :: forall a. Schema a -> Json -> Either String a
decodeVersioned (Schema definition) json = do
  object <- note ("expected a versioned payload, got " <> stringify json) (toObject json)
  version <-
    withField "version" object
      (Bifunctor.lmap printJsonDecodeError <<< decodeJson)
  payload <- note "missing field: payload" (Object.lookup "payload" object)
  upgraded <- upcastPayload definition.version definition.upcasters version payload
  decode definition.codec upgraded

upcastPayload
  :: Int
  -> Map Int Upcaster
  -> Int
  -> Json
  -> Either String Json
upcastPayload target upcasters = go
  where
  go version payload
    | version == target = Right payload
    | version > target =
        Left
          ( "payload version " <> show version
              <> " is newer than the schema version "
              <> show target
          )
    | otherwise = do
        Upcaster step <-
          note
            ("no upcaster from payload version " <> show version)
            (Map.lookup version upcasters)
        next <- step.upcast payload
        go (version + 1) next
