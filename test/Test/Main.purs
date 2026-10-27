module Test.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Test.Codec (testCodec)
import Test.Concurrency (testConcurrency)
import Test.Http (testHttp)
import Test.Replay (testReplay)
import Test.Runtime (testRuntime)
import Test.Stories (testStories)
import Test.Stream (testStream)
import Test.Trace (testTrace)

main :: Effect Unit
main = launchAff_ do
  testStories
  testReplay
  testCodec
  testStream
  testRuntime
  testConcurrency
  testTrace
  testHttp
  liftEffect (log "All tests passed.")
