module Test.Iris where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Traversable (traverse)
import Domain (Changes(..), changesValues, changesVersion, zeroVersion)
import Domain.Codec (Codec, decode, encode, jsonCodec)
import Domain.Iris.Deriving (IrisCodec(..), WrappedChanges(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)

newtype Count = Count Int

derive newtype instance eqCount :: Eq Count
derive newtype instance showCount :: Show Count

main :: Effect Unit
main = do
  testTraverse
  testDimap
  log "Iris deriving checks passed."

testTraverse :: Effect Unit
testTraverse = do
  let
    wrapped = WrappedChanges (Changes { version: zeroVersion, values: [ 1, 2, 3 ] })
    traversed = traverse (\value -> if value > 0 then Just (value * 2) else Nothing) wrapped
  case traversed of
    Just (WrappedChanges result) -> do
      assertEqual "traverse keeps the version" zeroVersion (changesVersion result)
      assertEqual "traverse maps every value" [ 2, 4, 6 ]
        (changesValues result)
    Nothing -> throw "FAIL traverse returned Nothing"

testDimap :: Effect Unit
testDimap = do
  let
    codec :: IrisCodec Count Count
    codec = dimap (\(Count value) -> value) Count
      (IrisCodec (jsonCodec :: Codec Int Int))
  case codec of
    IrisCodec unwrapped ->
      case decode unwrapped (encode unwrapped (Count 42)) of
        Right value -> assertEqual "dimap round-trips" (Count 42) value
        Left failure -> throw ("FAIL dimap decode: " <> failure)

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEqual label expected actual =
  if expected == actual then pure unit
  else
    throw
      ( "FAIL " <> label
          <> "\n  expected: " <> show expected
          <> "\n  actual:   " <> show actual
      )
