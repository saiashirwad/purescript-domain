module Test.Codec (testCodec) where

import Prelude

import Bank.Account
  ( AccountCommand
  , AccountEvent
  , AccountId(..)
  , accountOpened
  , accountStream
  , depositMoney
  , moneyDeposited
  , moneyWithdrawn
  , openAccount
  , withdrawMoney
  )
import Bank.Codec
  ( accountCommandCodec
  , accountEnvelopeCodec
  , accountEventCodec
  , accountEventSchema
  , accountIdCodec
  , customerCommandCodec
  , customerEventCodec
  , versionedAccountEventCodec
  )
import Bank.Customer
  ( CustomerCommand
  , CustomerEvent
  , customerRegistered
  , registerCustomer
  )
import Data.Argonaut.Core
  ( Json
  , fromNumber
  , fromObject
  , fromString
  , stringify
  , toObject
  )
import Data.Argonaut.Parser (jsonParser)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Profunctor as Profunctor
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Domain
  ( Envelope
  , commandIdText
  , firstSequence
  , mkEnvelope
  , parseCommandId
  , sequenceNumber
  , streamIdText
  )
import Domain.Codec
  ( Codec
  , decode
  , decodeVersioned
  , dimap
  , encode
  , jsonCodec
  , recordCodec
  , schemaVersion
  )
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck (quickCheck')
import Test.Support (assertEqual)

testCodec :: Aff Unit
testCodec = do
  testAccountEvents
  testAccountCommands
  testCustomerEvents
  testCustomerCommands
  testEnvelope
  testVersionedAccountEvents
  testDimap
  testRoundTripProperty

accountEvents :: Array AccountEvent
accountEvents =
  [ accountOpened "sam"
  , moneyDeposited 100 100
  , moneyWithdrawn 30 70
  ]

accountCommands :: Array AccountCommand
accountCommands =
  [ openAccount "sam"
  , depositMoney 100
  , withdrawMoney 30
  ]

customerEvents :: Array CustomerEvent
customerEvents = [ customerRegistered "sam" ]

customerCommands :: Array CustomerCommand
customerCommands = [ registerCustomer "sam" ]

testAccountEvents :: Aff Unit
testAccountEvents =
  for_ accountEvents \event ->
    assertRoundTrip ("account event " <> show event) accountEventCodec event

testAccountCommands :: Aff Unit
testAccountCommands =
  for_ accountCommands \command ->
    assertRoundTrip ("account command " <> show command) accountCommandCodec command

testCustomerEvents :: Aff Unit
testCustomerEvents =
  for_ customerEvents \event ->
    assertRoundTrip ("customer event " <> show event) customerEventCodec event

testCustomerCommands :: Aff Unit
testCustomerCommands =
  for_ customerCommands \command ->
    assertRoundTrip
      ("customer command " <> show command)
      customerCommandCodec
      command

testEnvelope :: Aff Unit
testEnvelope = do
  commandId <- expectRight "test command id" (parseCommandId "cmd-codec")
  let
    accountId = AccountId "a-1"
    envelope :: Envelope AccountId AccountEvent
    envelope =
      mkEnvelope accountId
        (accountStream accountId)
        firstSequence
        commandId
        fixedInstant
        (moneyDeposited 100 100)
    json = encode accountEnvelopeCodec envelope

  assertRoundTrip "account envelope" accountEnvelopeCodec envelope

  decoded <-
    expectRight "envelope decode" (decode accountEnvelopeCodec json)

  assertEqual "envelope.id" accountId decoded.id
  assertEqual "envelope.stream" "Account/a-1" (streamIdText decoded.stream)
  assertEqual "envelope.seq" 1 (sequenceNumber decoded.seq)
  assertEqual "envelope.commandId" "cmd-codec" (commandIdText decoded.commandId)
  assertEqual "envelope.at" (Milliseconds 1700000000123.0) (unInstant decoded.at)
  assertEqual "envelope.event" (moneyDeposited 100 100) decoded.event

  fields <- expectObject "envelope json" json
  assertEqual "envelope json has no extra fields" 6 (Object.size fields)

testVersionedAccountEvents :: Aff Unit
testVersionedAccountEvents = do
  assertEqual "schema version" 2 (schemaVersion accountEventSchema)

  -- A current payload round-trips unchanged.
  assertRoundTrip
    "versioned moneyDeposited"
    versionedAccountEventCodec
    (moneyDeposited 100 100)

  -- Version 1 of `moneyDeposited` has no `balanceAfter`; the upcaster supplies
  -- it so the current decoder can read the event.
  oldDeposit <-
    expectRight "old moneyDeposited decode"
      ( decodeVersioned accountEventSchema
          ( versionedJson 1
              ( tagged "moneyDeposited"
                  (object [ Tuple "amount" (fromNumber 100.0) ])
              )
          )
      )
  assertEqual "old moneyDeposited upcasts" (moneyDeposited 100 100) oldDeposit

  -- Version 1 payloads that did not change pass through the upcaster untouched.
  oldOpen <-
    expectRight "old accountOpened decode"
      ( decodeVersioned accountEventSchema
          ( versionedJson 1
              (tagged "accountOpened" (object [ Tuple "owner" (fromString "sam") ]))
          )
      )
  assertEqual "old accountOpened passes through" (accountOpened "sam") oldOpen

  -- A version newer than the schema is rejected rather than guessed at.
  case
    decodeVersioned accountEventSchema
      (versionedJson 3 (encode accountEventCodec (accountOpened "sam")))
    of
    Left _ -> pure unit
    Right _ -> throwError (error "FAIL newer payload version was accepted")

testDimap :: Aff Unit
testDimap = do
  assertRoundTrip "dimap newtype" countCodec (Count 42)

  decoded <- expectRight "dimap decode" (decode countCodec (fromNumber 12.0))
  assertEqual "dimap decodes the newtype" (Count 12) decoded

  owner <-
    expectRight "dimap wire/domain decode"
      (decode ownerNameCodec (encode ownerNameCodec { owner: "sam" }))
  assertEqual "dimap decodes a record into a newtype" (OwnerName "sam") owner

  -- The same transformation through the `Profunctor` instance itself.
  let viaProfunctor = Profunctor.dimap unCount Count (jsonCodec :: Codec Int Int)
  assertRoundTrip "profunctor dimap" viaProfunctor (Count 7)

newtype Count = Count Int

derive newtype instance eqCount :: Eq Count
derive newtype instance showCount :: Show Count

countCodec :: Codec Count Count
countCodec = dimap unCount Count (jsonCodec :: Codec Int Int)

unCount :: Count -> Int
unCount (Count value) = value

newtype OwnerName = OwnerName String

derive newtype instance eqOwnerName :: Eq OwnerName
derive newtype instance showOwnerName :: Show OwnerName

ownerNameCodec :: Codec { owner :: String } OwnerName
ownerNameCodec =
  dimap identity (\row -> OwnerName row.owner)
    (recordCodec :: Codec { owner :: String } { owner :: String })

testRoundTripProperty :: Aff Unit
testRoundTripProperty =
  liftEffect
    ( quickCheck' 200
        \(amount :: Int) (balanceAfter :: Int) (owner :: String) ->
          roundTrip accountEventCodec (moneyDeposited amount balanceAfter)
            == Right (moneyDeposited amount balanceAfter)
            && roundTrip accountEventCodec (moneyWithdrawn amount balanceAfter)
              == Right (moneyWithdrawn amount balanceAfter)
            && roundTrip accountEventCodec (accountOpened owner)
              == Right (accountOpened owner)
            && roundTrip accountCommandCodec (depositMoney amount)
              == Right (depositMoney amount)
            && roundTrip accountIdCodec (AccountId owner)
              == Right (AccountId owner)
    )

roundTrip :: forall a. Codec a a -> a -> Either String a
roundTrip codec value = do
  json <- jsonParser (stringify (encode codec value))
  decode codec json

assertRoundTrip
  :: forall a
   . Eq a
  => Show a
  => String
  -> Codec a a
  -> a
  -> Aff Unit
assertRoundTrip label codec value =
  case roundTrip codec value of
    Left failure -> throwError (error ("FAIL " <> label <> ": " <> failure))
    Right decoded -> assertEqual label value decoded

tagged :: String -> Json -> Json
tagged label payload =
  object [ Tuple "type" (fromString label), Tuple "value" payload ]

versionedJson :: Int -> Json -> Json
versionedJson version payload =
  object
    [ Tuple "version" (fromNumber (Int.toNumber version))
    , Tuple "payload" payload
    ]

object :: Array (Tuple String Json) -> Json
object = fromObject <<< Object.fromFoldable

expectObject :: String -> Json -> Aff (Object.Object Json)
expectObject label json =
  case toObject json of
    Just value -> pure value
    Nothing -> throwError (error ("FAIL " <> label <> ": expected a JSON object"))

fixedInstant :: Instant
fixedInstant =
  case instant (Milliseconds 1700000000123.0) of
    Just value -> value
    Nothing -> unsafeCrashWith "Test.Codec: test instant out of range"

expectRight :: forall a b. Show a => String -> Either a b -> Aff b
expectRight label = case _ of
  Right value -> pure value
  Left failure ->
    throwError (error ("FAIL " <> label <> ": " <> show failure))
