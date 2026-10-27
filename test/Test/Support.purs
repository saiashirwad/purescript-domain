module Test.Support
  ( assertEqual
  , assertJson
  , assertThat
  , expectRight
  , commandId
  , epoch
  , testOptions
  , recordingOptions
  , parked
  , failingOnce
  , fail
  )
  where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.DateTime.Instant (Instant, instant)
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Time.Duration (Milliseconds(..))
import Domain (CommandId, StreamId, parseCommandId, streamIdText)
import Domain.Runtime (Options, Span, defaults)
import Effect.AVar (AVar)
import Effect.Aff (Aff, throwError)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)

fail :: forall a. String -> Aff a
fail message = throwError (error ("FAIL " <> message))

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Aff Unit
assertEqual label expected actual = unless (expected == actual) do
  fail (label <> "\n  expected: " <> show expected <> "\n  actual:   " <> show actual)

assertThat :: String -> Boolean -> Aff Unit
assertThat label ok = unless ok (fail label)

assertJson :: String -> String -> Json -> Aff Unit
assertJson label expected actual = case jsonParser expected of
  Left failure -> fail (label <> ": bad expected JSON: " <> failure)
  Right parsed -> unless (parsed == actual) do
    fail (label <> "\n  expected: " <> expected <> "\n  actual:   " <> stringify actual)

expectRight :: forall a b. Show a => String -> Either a b -> Aff b
expectRight label = case _ of
  Right value -> pure value
  Left failure -> fail (label <> ": expected Right, got " <> show failure)

epoch :: Instant
epoch = unsafePartial (fromJust (instant (Milliseconds 0.0)))

commandId :: String -> CommandId
commandId raw = unsafePartial (fromJust (parseCommandId raw))

testOptions :: Options
testOptions = defaults { now = pure epoch }

recordingOptions :: Aff { options :: Options, spans :: Ref (Array Span) }
recordingOptions = do
  spans <- liftEffect (Ref.new [])
  pure
    { options: testOptions { trace = \span -> liftEffect (Ref.modify_ (_ <> [ span ]) spans) }
    , spans
    }

-- | A `beforeTurn` that parks every command on `stream` until `release`.
-- | `arrived` fills when the first one reaches its turn, and `turns` counts
-- | the ones that have.
parked ::
  String ->
  Aff
    { beforeTurn :: StreamId -> Aff Unit
    , arrived :: AVar Unit
    , turns :: Aff Int
    , release :: Aff Unit
    }
parked stream = do
  arrived <- AVar.empty
  gate <- AVar.empty
  count <- liftEffect (Ref.new 0)
  pure
    { beforeTurn: \at -> when (streamIdText at == stream) do
        liftEffect (Ref.modify_ (_ + 1) count)
        void (AVar.tryPut unit arrived)
        AVar.read gate
    , arrived
    , turns: liftEffect (Ref.read count)
    , release: AVar.put unit gate
    }

-- | A `beforeTurn` that fails the first command on `stream` with `message`.
failingOnce :: String -> String -> Aff (StreamId -> Aff Unit)
failingOnce stream message = do
  pending <- liftEffect (Ref.new true)
  pure \at -> when (streamIdText at == stream) do
    first <- liftEffect (Ref.modify' (\state -> { state: false, value: state }) pending)
    when first (fail message)
