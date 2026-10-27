module Test.Stream (testStream) where

import Prelude

import Bank.Customer (CustomerCommand(..), CustomerId(..), customer)
import Bank.Policy (mainAccount, provisioning)
import Data.Array (concatMap)
import Domain (commandIdText)
import Domain.Internal.Id (streamOf)
import Domain.Internal.Stream (Turn(..), open, orders, turn)
import Effect.Aff (Aff)
import Test.Support (assertEqual, commandId, epoch, fail)

-- | A stream keys its receipts by command id, so an order's derived id is
-- | part of what a stream stores, and its format is pinned here.
testStream :: Aff Unit
testStream = do
  let
    ada = CustomerId "c-1"
    register = RegisterCustomer { name: "ada" }
  registered <-
    case
      turn customer epoch (commandId "register") register
        (open customer (streamOf "customer" "c-1") ada)
    of
      Committed result -> pure result
      _ -> fail "register commits"
  let
    placed = concatMap (orders "provisioning" provisioning) registered.stored.facts
  assertEqual "an order's id names the policy, the source stream, the sequence, and the index"
    [ "provisioning:customer/c-1@1#0" ] (map (commandIdText <<< _.commandId) placed)
  assertEqual "the order targets the customer's main account" [ mainAccount ada ] (map _.to placed)
  case turn customer epoch (commandId "register") register registered.stream of
    Duplicate stored -> assertEqual "a duplicate re-derives the same order ids"
      (map (commandIdText <<< _.commandId) placed)
      (map (commandIdText <<< _.commandId) (concatMap (orders "provisioning" provisioning) stored.facts))
    _ -> fail "a repeated register is a duplicate"
  assertEqual "another policy reacting to the same event gets other ids"
    [ "welcome:customer/c-1@1#0" ]
    ( map (commandIdText <<< _.commandId)
        (concatMap (orders "welcome" provisioning) registered.stored.facts)
    )
