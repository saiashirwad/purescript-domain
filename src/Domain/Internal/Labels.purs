-- | Labels and the labelled JSON format, derived from `Generic`. A
-- | constructor's label is its name with the first letter lowered, so
-- | `MoneyDeposited { amount: 5 }` is labelled `moneyDeposited` and encodes
-- | as `{ "type": "moneyDeposited", "value": { "amount": 5 } }`. A
-- | constructor takes no argument or one; a nullary one encodes its value
-- | as `{}`.
module Domain.Internal.Labels
  ( class LabelOf
  , class EncodeCases
  , class DecodeCases
  , class EncodePayload
  , class DecodePayload
  , labelOf
  , encodeLabelled
  , decodeLabelled
  )
  where

import Prelude

import Data.Argonaut.Core (Json, fromObject, fromString, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep
  (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product, Sum(..), from, to)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))

labelOf :: forall a rep. Generic a rep => LabelOf rep => a -> String
labelOf = repLabel <<< from

-- | `{ "type": <label>, "value": <payload> }`.
encodeLabelled :: forall a rep. Generic a rep => EncodeCases rep => a -> Json
encodeLabelled = encodeCases <<< from

-- | Reads a label and a payload that arrive apart, as the command name and
-- | body of an HTTP request do.
decodeLabelled :: forall a rep. Generic a rep => DecodeCases rep => String -> Json -> Either String a
decodeLabelled label payload =
  maybe (Left ("unknown label " <> label)) (map to) (decodeCase label payload)

label :: forall name. IsSymbol name => Proxy name -> String
label name = String.toLower (CodeUnits.take 1 raw) <> CodeUnits.drop 1 raw
  where
  raw = reflectSymbol name

class LabelOf rep where
  repLabel :: rep -> String

instance (LabelOf a, LabelOf b) => LabelOf (Sum a b) where
  repLabel (Inl a) = repLabel a
  repLabel (Inr b) = repLabel b

-- | A type with no constructors, such as the rejections of an aggregate
-- | that never rejects, has no values to label.
instance LabelOf NoConstructors where
  repLabel none = repLabel none

instance IsSymbol name => LabelOf (Constructor name args) where
  repLabel _ = label (Proxy :: Proxy name)

class EncodeCases rep where
  encodeCases :: rep -> Json

instance (EncodeCases a, EncodeCases b) => EncodeCases (Sum a b) where
  encodeCases (Inl a) = encodeCases a
  encodeCases (Inr b) = encodeCases b

instance EncodeCases NoConstructors where
  encodeCases none = encodeCases none

instance (IsSymbol name, EncodePayload args) => EncodeCases (Constructor name args) where
  encodeCases (Constructor args) = fromObject
    ( Object.fromFoldable
        [ Tuple "type" (fromString (label (Proxy :: Proxy name)))
        , Tuple "value" (encodePayload args)
        ]
    )

class DecodeCases rep where
  decodeCase :: String -> Json -> Maybe (Either String rep)

instance (DecodeCases a, DecodeCases b) => DecodeCases (Sum a b) where
  decodeCase name payload = case decodeCase name payload of
    Just decoded -> Just (Inl <$> decoded)
    Nothing -> map Inr <$> decodeCase name payload

instance DecodeCases NoConstructors where
  decodeCase _ _ = Nothing

instance (IsSymbol name, DecodePayload args) => DecodeCases (Constructor name args) where
  decodeCase name payload
    | name == label (Proxy :: Proxy name) =
      Just (Constructor <$> lmap (\e -> name <> ": " <> e) (decodePayload payload))
    | otherwise = Nothing

class EncodePayload args where
  encodePayload :: args -> Json

instance EncodePayload NoArguments where
  encodePayload _ = jsonEmptyObject

instance EncodeJson a => EncodePayload (Argument a) where
  encodePayload (Argument a) = encodeJson a

instance Fail OneArgument => EncodePayload (Product a b) where
  encodePayload _ = jsonEmptyObject

class DecodePayload args where
  decodePayload :: Json -> Either String args

instance DecodePayload NoArguments where
  decodePayload _ = Right NoArguments

instance DecodeJson a => DecodePayload (Argument a) where
  decodePayload = map Argument <<< lmap printJsonDecodeError <<< decodeJson

instance Fail OneArgument => DecodePayload (Product a b) where
  decodePayload _ = Left "unreachable: rejected at compile time"

type OneArgument = Text "a labelled constructor takes at most one argument; put its fields in a record"
