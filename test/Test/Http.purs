module Test.Http (testHttp) where

import Prelude

import Bank (bank)
import Bank.Account (AccountId(..))
import Bank.Api (Principal, guards)
import Data.Argonaut.Core (fromString, jsonEmptyObject, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Domain.Http (Method(..), serve)
import Domain.Runtime (start)
import Effect.Aff (Aff)
import Test.Support (assertEqual, assertJson, testOptions)

testHttp :: Aff Unit
testHttp = do
  rt <- start bank testOptions
  let
    sam :: Principal
    sam = { subject: "sam", roles: Set.empty, accounts: Set.singleton (AccountId "a-1") }
    admin = { subject: "root", roles: Set.singleton "admin", accounts: Set.empty }
    call who method path key query body = serve bank guards rt who
      { method
      , path
      , query: Map.fromFoldable query
      , headers: maybe' Map.empty (Map.singleton "idempotency-key") key
      , body
      }
    post who path key body = call who POST path (Just key) [] body
    get who path query = call who GET path Nothing query jsonEmptyObject

  opened <- post sam "/account/a-1/openAccount" "k-open" (encodeJson { owner: "sam" })
  assertEqual "a command route commits" 200 opened.status
  assertJson "the receipt is derived JSON"
    """{"events":[{"type":"accountOpened","value":{"owner":"sam"}}],"reply":{"balance":0},"version":1}"""
    opened.body
  retried <- post sam "/account/a-1/openAccount" "k-open" (encodeJson { owner: "sam" })
  assertEqual "a retried key returns the same status" 200 retried.status
  assertEqual "a retried key returns the same body" (stringify opened.body) (stringify retried.body)

  overdraw <- post sam "/account/a-1/withdrawMoney" "k-withdraw" (encodeJson { amount: 5 })
  assertEqual "a rejection is a 409" 409 overdraw.status
  assertJson "the rejection body carries the label and payload"
    """{"type":"insufficientFunds","value":{"balance":0,"requested":5}}""" overdraw.body

  deposited <- post sam "/account/a-1/depositMoney" "k-deposit" (encodeJson { amount: 70 })
  assertEqual "a deposit commits" 200 deposited.status
  summary <- get sam "/summary/a-1" []
  assertEqual "a view route is 200" 200 summary.status
  assertJson "a view route reads the view" """{"balance":70,"owner":"sam"}""" summary.body
  atLeast <- get sam "/summary/a-1" [ Tuple "atLeast" "2" ]
  assertEqual "atLeast waits for a version already reached" 200 atLeast.status
  statement <- get sam "/statement/a-1" []
  assertJson "another view route" """[{"kind":"deposit","amount":70,"balance":70}]""" statement.body
  missing <- get sam "/summary/a-1" [ Tuple "atLeast" "x" ]
  assertEqual "a malformed atLeast is 400" 400 missing.status

  badPayload <- post sam "/account/a-1/depositMoney" "k-bad" (encodeJson { amount: "lots" })
  assertEqual "a payload of the wrong shape is 400" 400 badPayload.status
  notAnObject <- post sam "/account/a-1/depositMoney" "k-bad-2" (fromString "nope")
  assertEqual "a body that is not an object is 400" 400 notAnObject.status
  noKey <- call sam POST "/account/a-1/depositMoney" Nothing [] (encodeJson { amount: 1 })
  assertEqual "a command without an idempotency key is 400" 400 noKey.status
  unknownCommand <- post sam "/account/a-1/closeAccount" "k-close" jsonEmptyObject
  assertEqual "an unknown command is 400" 400 unknownCommand.status
  unknownRoute <- post sam "/nothing/a-1/openAccount" "k-x" jsonEmptyObject
  assertEqual "an unknown aggregate is 404" 404 unknownRoute.status
  shortPath <- get sam "/account" []
  assertEqual "a path without an id is 404" 404 shortPath.status
  wrongMethod <- get sam "/account/a-1/openAccount" []
  assertEqual "a command route rejects GET with 405" 405 wrongMethod.status
  wrongMethodView <- post sam "/summary/a-1" "k-y" jsonEmptyObject
  assertEqual "a view route rejects POST with 405" 405 wrongMethodView.status
  unseen <- get admin "/summary/a-9" []
  assertEqual "a view of nothing recorded is 404" 404 unseen.status

  strangerView <- get sam "/summary/a-2" []
  assertEqual "a view guard denies another's account" 403 strangerView.status
  strangerCommand <- post sam "/account/a-2/openAccount" "k-steal" (encodeJson { owner: "sam" })
  assertEqual "a command guard denies another's account" 403 strangerCommand.status
  register <- post sam "/customer/c-9/registerCustomer" "k-reg" (encodeJson { name: "sam" })
  assertEqual "only admins register customers" 403 register.status
  adminRegister <- post admin "/customer/c-9/registerCustomer" "k-reg" (encodeJson { name: "sam" })
  assertEqual "an admin may" 200 adminRegister.status
  adminView <- get admin "/summary/c-9-main" []
  assertEqual "an admin reads the policy's account" 200 adminView.status
  rt.stop
  where
  maybe' default f = case _ of
    Nothing -> default
    Just value -> f value
