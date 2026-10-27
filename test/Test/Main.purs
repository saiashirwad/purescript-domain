module Test.Main (main) where

import Prelude

import Bank (bank)
import Bank.Account (AccountId(..), AccountState(..), account)
import Bank.Api (guards)
import Bank.Codec (accountEvents)
import Bank.Customer (CustomerId(..))
import Bank.Policy (mainAccount)
import Data.Argonaut.Core (Json, jsonEmptyObject, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (index, length)
import Data.DateTime.Instant (Instant, instant)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Domain (CommandId, cmd, event, parseCommandId, rejection, replay, sequenceNumber, versionNumber)
import Domain.Codec (decode, encode)
import Domain.Http (Method(..), serve)
import Domain.Runtime (Options, Span, defaults, start)
import Domain.Story (command, emits, given, rejects, replies, story)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, forkAff, joinFiber, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Ref as Ref
import Data.Set as Set
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = launchAff_ do
  stories
  runtime
  http
  codec
  stopDrainsCascades
  liftEffect (log "All stories passed.")

stories :: Aff Unit
stories = do
  expectStory "deposits into an open account and replies with the balance" $ story account
    [ given @"accountOpened" { owner: "sam" }
    , command @"depositMoney" { amount: 100 }
    , emits @"moneyDeposited" { amount: 100, balanceAfter: 100 }
    , replies { balance: 100 }
    ]
  expectStory "refuses to withdraw more than the balance" $ story account
    [ given @"accountOpened" { owner: "sam" }
    , given @"moneyDeposited" { amount: 100, balanceAfter: 100 }
    , command @"withdrawMoney" { amount: 150 }
    , rejects @"insufficientFunds" { balance: 100, requested: 150 }
    ]
  expectStory "refuses a deposit into an account that isn't open" $ story account
    [ command @"depositMoney" { amount: 100 }
    , rejects @"notOpen" {}
    ]
  expectStory "refuses to open an account twice" $ story account
    [ given @"accountOpened" { owner: "sam" }
    , command @"openAccount" { owner: "kim" }
    , rejects @"alreadyOpen" {}
    ]
  let
    before = [ event @"accountOpened" { owner: "sam" } ]
    after = [ event @"moneyDeposited" { amount: 100, balanceAfter: 100 }, event @"moneyWithdrawn" { amount: 30, balanceAfter: 70 } ]
  assertEqual "replay is a fold homomorphism"
    (replay account Unopened (before <> after))
    (replay account (replay account Unopened before) after)

-- | Send a command, retry it, and read the views it fed.
runtime :: Aff Unit
runtime = do
  spans <- liftEffect (Ref.new [])
  rt <- start bank (testOptions \span -> liftEffect (Ref.modify_ (_ <> [ span ]) spans))
  let sam = AccountId "a-1"

  opened <- expectRight =<< rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: "sam" })
  assertEqual "open records the event" [ event @"accountOpened" { owner: "sam" } ] opened.events
  deposited <- expectRight =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  assertEqual "deposit replies with the balance" 100 deposited.reply.balance
  assertEqual "deposit is version 2" 2 (versionNumber deposited.version)

  retried <- expectRight =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  assertEqual "a retry returns the stored receipt" (show deposited) (show retried)
  history <- rt.account.history sam
  assertEqual "a retry appends nothing" [ 1, 2 ] (map (sequenceNumber <<< _.seq) history)

  refused <- rt.account.send (commandId "overdraw") sam (cmd @"withdrawMoney" { amount: 500 })
  assertEqual "an overdraft is refused" (Left (rejection @"insufficientFunds" { balance: 100, requested: 500 })) (map _.events refused)
  reused <- expectRight =<< rt.account.send (commandId "overdraw") sam (cmd @"withdrawMoney" { amount: 30 })
  assertEqual "a refused command id stores nothing and can be reused" 3 (versionNumber reused.version)

  summary <- rt.summary.read sam
  assertEqual "the summary follows the balance" (Just { owner: Just "sam", balance: 70 }) summary
  caughtUp <- rt.summary.readAt reused.version sam
  assertEqual "readAt sees the version the receipt names" (Just 70) (map _.balance caughtUp)
  statement <- rt.statement.read sam
  assertEqual "the statement has one line per movement"
    (Just [ { kind: "deposit", amount: 100, balance: 100 }, { kind: "withdrawal", amount: 30, balance: 70 } ])
    statement

  let ada = CustomerId "c-1"
  registered <- expectRight =<< rt.customer.send (commandId "register") ada (cmd @"registerCustomer" { name: "ada" })
  _ <- expectRight =<< rt.customer.send (commandId "register") ada (cmd @"registerCustomer" { name: "ada" })
  assertEqual "the customer is registered once" 1 (length registered.events)
  provisioned <- rt.summary.read (mainAccount ada)
  assertEqual "the policy opens the main account" (Just { owner: Just "ada", balance: 0 }) provisioned
  mainHistory <- rt.account.history (mainAccount ada)
  assertEqual "a retried trigger does not reopen the account" 1 (length mainHistory)

  recorded <- liftEffect (Ref.read spans)
  assertEqual "every command emits a span named by its aggregate label"
    [ "command.account", "command.account", "command.account", "command.account", "command.account", "command.account", "command.customer", "command.account", "command.customer" ]
    (map _.name recorded)
  assertEqual "span attributes come from labels"
    (Just "insufficientFunds")
    (index recorded 3 >>= \span -> Map.lookup "rejection" span.attributes)

  rt.stop
  afterStop <- attempt (rt.account.send (commandId "late") sam (cmd @"depositMoney" { amount: 1 }))
  assertEqual "a stopped runtime refuses commands" true (isLeft afterStop)
  where
  isLeft = case _ of
    Left _ -> true
    Right _ -> false

-- | The HTTP boundary is derived from the context plus the guards.
http :: Aff Unit
http = do
  rt <- start bank (testOptions \_ -> pure unit)
  let
    sam = { subject: "sam", roles: Set.empty, accounts: Set.singleton (AccountId "a-1") }
    call method path key body = serve bank guards rt sam
      { method, path, query: Map.empty, headers: maybe Map.empty (Map.singleton "idempotency-key") key, body }
  opened <- call POST "/account/a-1/openAccount" (Just "k-open") (encodeJson { owner: "sam" })
  assertEqual "a command route commits" 200 opened.status
  assertJson "the receipt is derived JSON"
    """{"events":[{"type":"accountOpened","value":{"owner":"sam"}}],"reply":{"balance":0},"version":1}"""
    opened.body
  overdraw <- call POST "/account/a-1/withdrawMoney" (Just "k-withdraw") (encodeJson { amount: 5 })
  assertEqual "a rejection is a 409 with the labelled rejection" 409 overdraw.status
  assertJson "the rejection body"
    """{"type":"insufficientFunds","value":{"balance":0,"requested":5}}"""
    overdraw.body
  unknown <- call POST "/account/a-1/closeAccount" (Just "k-close") jsonEmptyObject
  assertEqual "an unknown command is a 400" 400 unknown.status
  other <- call GET "/summary/a-2" Nothing jsonEmptyObject
  assertEqual "guards apply per entity" 403 other.status
  read <- call GET "/summary/a-1" Nothing jsonEmptyObject
  assertJson "a view route reads the view" """{"balance":0,"owner":"sam"}""" read.body
  register <- call POST "/customer/c-9/registerCustomer" (Just "k-reg") (encodeJson { name: "sam" })
  assertEqual "only admins register customers" 403 register.status
  rt.stop
  where
  maybe default f = case _ of
    Nothing -> default
    Just value -> f value

-- | `stop` refuses new commands but lets an in-flight command finish its
-- | policy cascade, even when the cascade targets another aggregate.
stopDrainsCascades :: Aff Unit
stopDrainsCascades = do
  let slowAccounts = { beforeCommand: \_ -> delay (Milliseconds 5.0), afterCommand: \_ -> pure unit }
  rt <- start bank ((testOptions \_ -> pure unit) { hooks = slowAccounts })
  let ada = CustomerId "c-2"
  registering <- forkAff (rt.customer.send (commandId "register") ada (cmd @"registerCustomer" { name: "ada" }))
  rt.stop
  _ <- expectRight =<< joinFiber registering
  provisioned <- rt.summary.read (mainAccount ada)
  assertEqual "stop waits for the policy cascade" (Just { owner: Just "ada", balance: 0 }) provisioned

codec :: Aff Unit
codec = do
  let deposited = event @"moneyDeposited" { amount: 40, balanceAfter: 40 }
  assertEqual "a stored event round-trips" (Right deposited) (decode accountEvents (encode accountEvents deposited))
  assertEqual "version 1 deposits are upcast" (Right deposited)
    (decode accountEvents =<< jsonParser """{"version":1,"payload":{"type":"moneyDeposited","value":{"amount":40}}}""")

testOptions :: (Span -> Aff Unit) -> Options
testOptions trace = defaults { now = pure epoch, trace = trace }

epoch :: Instant
epoch = unsafePartial (fromJust (instant (Milliseconds 0.0)))

commandId :: String -> CommandId
commandId raw = unsafePartial (fromJust (parseCommandId raw))

expectStory :: forall s. String -> Either String s -> Aff Unit
expectStory label = case _ of
  Right _ -> pure unit
  Left failure -> throwError (error ("FAIL " <> label <> ": " <> failure))

expectRight :: forall a b. Show a => Either a b -> Aff b
expectRight = case _ of
  Right value -> pure value
  Left failure -> throwError (error ("expected Right, got " <> show failure))

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Aff Unit
assertEqual label expected actual =
  unless (expected == actual) do
    throwError (error ("FAIL " <> label <> "\n  expected: " <> show expected <> "\n  actual:   " <> show actual))

assertJson :: String -> String -> Json -> Aff Unit
assertJson label expected actual = case jsonParser expected of
  Left failure -> throwError (error ("FAIL " <> label <> ": bad expected JSON: " <> failure))
  Right parsed -> unless (parsed == actual) do
    throwError (error ("FAIL " <> label <> "\n  expected: " <> expected <> "\n  actual:   " <> stringify actual))
