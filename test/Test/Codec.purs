module Test.Codec (testCodec) where

import Prelude

import Bank.Codec (accountEvents)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Domain (event)
import Domain.Codec (decode, encode)
import Effect.Aff (Aff)
import Test.Support (assertEqual, assertJson, assertThat, isLeft)

testCodec :: Aff Unit
testCodec = do
  let
    deposited = event @"moneyDeposited" { amount: 40, balanceAfter: 40 }
    events =
      [ event @"accountOpened" { owner: "sam" }
      , deposited
      , event @"moneyWithdrawn" { amount: 15, balanceAfter: 25 }
      ]
  assertEqual "every stored event round-trips"
    (Right events)
    (traverse (decode accountEvents <<< encode accountEvents) events)
  assertJson "events are written at the current version"
    """{"version":2,"payload":{"type":"moneyDeposited","value":{"amount":40,"balanceAfter":40}}}"""
    (encode accountEvents deposited)
  assertEqual "version 1 deposits are upcast" (Right deposited)
    (decode accountEvents =<< jsonParser """{"version":1,"payload":{"type":"moneyDeposited","value":{"amount":40}}}""")
  assertEqual "version 1 events the upcaster does not touch decode unchanged"
    (Right (event @"accountOpened" { owner: "sam" }))
    (decode accountEvents =<< jsonParser """{"version":1,"payload":{"type":"accountOpened","value":{"owner":"sam"}}}""")
  assertThat "a payload newer than the codec is refused"
    (isLeft (decode accountEvents =<< jsonParser """{"version":3,"payload":{"type":"accountOpened","value":{"owner":"sam"}}}"""))
  assertThat "an unknown label is refused"
    (isLeft (decode accountEvents =<< jsonParser """{"version":2,"payload":{"type":"accountClosed","value":{}}}"""))
  assertThat "an unversioned payload is refused"
    (isLeft (decode accountEvents =<< jsonParser """{"type":"accountOpened","value":{"owner":"sam"}}"""))
