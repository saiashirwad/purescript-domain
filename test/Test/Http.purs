module Test.Http (testHttp) where

import Prelude

import Bank (bank)
import Bank.Account (AccountId(..))
import Bank.Api (Principal, guards)
import Bank.Customer (CustomerId(..))
import Data.Argonaut.Core (fromString, jsonEmptyObject, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Domain.Http (Method(..), serve)
import Domain.Runtime (start)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Support (assertEqual, assertJson, recordingOptions, testOptions)

testHttp :: Aff Unit
testHttp = do
  rt <- start bank testOptions
  let
    sam :: Principal
    sam =
      { subject: "sam"
      , roles: Set.empty
      , customer: Just (CustomerId "sam")
      , accounts: Set.singleton (AccountId "a-1")
      }
    admin =
      { subject: "root", roles: Set.singleton "admin", customer: Nothing, accounts: Set.empty }
    newcomer =
      { subject: "c-1"
      , roles: Set.empty
      , customer: Just (CustomerId "c-1")
      , accounts: Set.singleton (AccountId "c-1-main")
      }
    call who method path key body = serve bank guards rt who
      { method, path, headers: maybe Map.empty (Map.singleton "idempotency-key") key, body }
    post who path key body = call who POST path (Just key) body
    get who path = call who GET path Nothing jsonEmptyObject

  inAnotherName <- post sam "/account/a-1/openAccount" "k-mine" (encodeJson { owner: "kim" })
  assertEqual "an owner may not open their account in another customer's name" 403
    inAnotherName.status
  preOpened <- post newcomer "/account/c-1-main/openAccount" "k-early" (encodeJson { owner: "sam" })
  assertEqual "a customer may not pre-open the main account the policy opens" 403 preOpened.status

  opened <- post admin "/account/a-1/openAccount" "k-open" (encodeJson { owner: "sam" })
  assertEqual "an admin opens an account" 200 opened.status
  assertJson "the receipt is derived JSON"
    """{"events":[{"type":"accountOpened","value":{"owner":"sam"}}],"reply":{"balance":0},"version":1}"""
    opened.body
  retried <- post admin "/account/a-1/openAccount" "k-open" (encodeJson { owner: "sam" })
  assertEqual "a retried key returns the same status" 200 retried.status
  assertEqual "a retried key returns the same body" (stringify opened.body) (stringify retried.body)
  reused <- post sam "/account/a-1/depositMoney" "k-open" (encodeJson { amount: 5 })
  assertEqual "a key reused for another command is 422" 422 reused.status
  negative <- post sam "/account/a-1/depositMoney" "k-negative" (encodeJson { amount: -500 })
  assertEqual "a negative deposit is a rejection" 409 negative.status
  assertJson "the rejection names the amount"
    """{"type":"nonPositiveAmount","value":{"amount":-500}}""" negative.body
  victim <- get admin "/dashboard/kim"
  assertEqual "no account or balance landed on another customer's dashboard" 404 victim.status

  overdraw <- post sam "/account/a-1/withdrawMoney" "k-withdraw" (encodeJson { amount: 5 })
  assertEqual "a rejection is a 409" 409 overdraw.status
  assertJson "the rejection body carries the label and payload"
    """{"type":"insufficientFunds","value":{"balance":0,"requested":5}}""" overdraw.body

  deposited <- post sam "/account/a-1/depositMoney" "k-deposit" (encodeJson { amount: 70 })
  assertEqual "a deposit commits" 200 deposited.status
  summary <- get sam "/summary/a-1"
  assertEqual "a view route is 200" 200 summary.status
  assertJson "a view route reads the view" """{"balance":70,"owner":"sam"}""" summary.body
  statement <- get sam "/statement/a-1"
  assertJson "another view route" """[{"kind":"deposit","amount":70,"balance":70}]""" statement.body
  dashboard <- get sam "/dashboard/sam"
  assertEqual "a keyed view route is 200" 200 dashboard.status
  assertJson "a keyed view route reads the customer's row" """{"accounts":1,"balance":70}"""
    dashboard.body

  badPayload <- post sam "/account/a-1/depositMoney" "k-bad" (encodeJson { amount: "lots" })
  assertEqual "a payload of the wrong shape is 400" 400 badPayload.status
  notAnObject <- post sam "/account/a-1/depositMoney" "k-bad-2" (fromString "nope")
  assertEqual "a body that is not an object is 400" 400 notAnObject.status
  noKey <- call sam POST "/account/a-1/depositMoney" Nothing (encodeJson { amount: 1 })
  assertEqual "a command without an idempotency key is 400" 400 noKey.status
  unknownCommand <- post sam "/account/a-1/closeAccount" "k-close" jsonEmptyObject
  assertEqual "an unknown command is 400" 400 unknownCommand.status
  unknownRoute <- post sam "/nothing/a-1/openAccount" "k-x" jsonEmptyObject
  assertEqual "an unknown aggregate is 404" 404 unknownRoute.status
  prototypeRoute <- post sam "/constructor/a-1/openAccount" "k-p" jsonEmptyObject
  assertEqual "a label inherited by every JavaScript object is no route" 404 prototypeRoute.status
  shortPath <- get sam "/account"
  assertEqual "a path without an id is 404" 404 shortPath.status
  wrongMethod <- get sam "/account/a-1/openAccount"
  assertEqual "a command route rejects GET with 405" 405 wrongMethod.status
  wrongMethodView <- post sam "/summary/a-1" "k-y" jsonEmptyObject
  assertEqual "a view route rejects POST with 405" 405 wrongMethodView.status
  unseen <- get admin "/summary/a-9"
  assertEqual "a view of nothing recorded is 404" 404 unseen.status

  strangerView <- get sam "/summary/a-2"
  assertEqual "a view guard denies another's account" 403 strangerView.status
  strangerDashboard <- get sam "/dashboard/kim"
  assertEqual "a keyed view guard denies another customer's row" 403 strangerDashboard.status
  wrongMethodDashboard <- post sam "/dashboard/sam" "k-z" jsonEmptyObject
  assertEqual "a keyed view route rejects POST with 405" 405 wrongMethodDashboard.status
  strangerCommand <- post sam "/account/a-2/depositMoney" "k-steal" (encodeJson { amount: 1 })
  assertEqual "a command guard denies another's account" 403 strangerCommand.status
  register <- post sam "/customer/c-9/registerCustomer" "k-reg" (encodeJson { name: "sam" })
  assertEqual "only admins register customers" 403 register.status
  adminRegister <- post admin "/customer/c-9/registerCustomer" "k-reg" (encodeJson { name: "sam" })
  assertEqual "an admin may" 200 adminRegister.status
  adminView <- get admin "/summary/c-9-main"
  assertEqual "an admin reads the policy's account" 200 adminView.status
  adminDashboard <- get admin "/dashboard/c-9"
  assertJson "an admin reads the customer's row the policy opened" """{"accounts":1,"balance":0}"""
    adminDashboard.body
  noRow <- get admin "/dashboard/c-404"
  assertEqual "a keyed view of nothing recorded is 404" 404 noRow.status
  _ <- post admin "/customer/c-1/registerCustomer" "k-c-1" (encodeJson { name: "c-1" })
  newcomerDashboard <- get admin "/dashboard/c-1"
  assertJson "the policy opened the newcomer's main account in their name"
    """{"accounts":1,"balance":0}""" newcomerDashboard.body
  rt.stop

  { options, spans } <- recordingOptions
  stopped <- start bank options
  stopped.stop
  failed <- serve bank guards stopped admin
    { method: POST
    , path: "/account/a-1/depositMoney"
    , headers: Map.singleton "idempotency-key" "k-late"
    , body: encodeJson { amount: 1 }
    }
  assertEqual "a runtime failure is a 500" 500 failed.status
  assertJson "a 500 does not echo the failure" """{"error":"internal error"}""" failed.body
  recorded <- liftEffect (Ref.read spans)
  assertEqual "the trace carries the failure that the 500 hides"
    [ Just "Domain.Runtime: the runtime is stopped" ]
    (map (Map.lookup "error" <<< _.attributes) recorded)
