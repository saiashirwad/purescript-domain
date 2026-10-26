-- | JSON codecs for the bank's commands, events, envelopes, and the versioned
-- | account event payload.
module Bank.Codec
  ( accountIdCodec
  , customerIdCodec
  , accountCommandCodec
  , accountEventCodec
  , accountRejectionCodec
  , customerCommandCodec
  , customerEventCodec
  , customerRejectionCodec
  , summaryCodec
  , statementLineCodec
  , accountEnvelopeCodec
  , accountEventSchema
  , versionedAccountEventCodec
  , moneyDepositedUpcaster
  , upcastMoneyDeposited
  ) where

import Prelude

import Bank.Account
  ( AccountCommand
  , AccountEvent
  , AccountId(..)
  , AccountRejection
  )
import Bank.Customer
  ( CustomerCommand
  , CustomerEvent
  , CustomerId(..)
  , CustomerRejection
  )
import Bank.Views (StatementLine, Summary)
import Data.Argonaut.Core
  ( Json
  , fromObject
  , stringify
  , toObject
  , toString
  )
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Bifunctor as Bifunctor
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Domain (Envelope)
import Domain.Codec
  ( Codec
  , Schema
  , Upcaster
  , dimap
  , envelopeCodec
  , jsonCodec
  , schema
  , upcaster
  , variantCodec
  , versionedCodec
  )
import Foreign.Object as Object

accountIdCodec :: Codec AccountId AccountId
accountIdCodec =
  dimap unAccountId AccountId (jsonCodec :: Codec String String)

customerIdCodec :: Codec CustomerId CustomerId
customerIdCodec =
  dimap unCustomerId CustomerId (jsonCodec :: Codec String String)

accountCommandCodec :: Codec AccountCommand AccountCommand
accountCommandCodec = variantCodec

accountEventCodec :: Codec AccountEvent AccountEvent
accountEventCodec = variantCodec

accountRejectionCodec :: Codec AccountRejection AccountRejection
accountRejectionCodec = variantCodec

customerCommandCodec :: Codec CustomerCommand CustomerCommand
customerCommandCodec = variantCodec

customerEventCodec :: Codec CustomerEvent CustomerEvent
customerEventCodec = variantCodec

customerRejectionCodec :: Codec CustomerRejection CustomerRejection
customerRejectionCodec = variantCodec

summaryCodec :: Codec Summary Summary
summaryCodec = jsonCodec

statementLineCodec :: Codec StatementLine StatementLine
statementLineCodec = jsonCodec

accountEnvelopeCodec
  :: Codec (Envelope AccountId AccountEvent) (Envelope AccountId AccountEvent)
accountEnvelopeCodec = envelopeCodec accountIdCodec accountEventCodec

-- | Version 1 of `moneyDeposited` predates `balanceAfter`: it recorded only the
-- | amount. The current decoder needs the resulting balance, which for a single
-- | deposit is the amount itself.
upcastMoneyDeposited :: Json -> Either String Json
upcastMoneyDeposited json = do
  object <-
    note ("moneyDeposited upcaster: expected an object, got " <> stringify json)
      (toObject json)
  tagJson <- note "moneyDeposited upcaster: missing field: type" (Object.lookup "type" object)
  tag <- note "moneyDeposited upcaster: field type is not a string" (toString tagJson)
  if tag /= "moneyDeposited" then pure json
  else do
    payload <-
      note "moneyDeposited upcaster: missing field: value"
        (Object.lookup "value" object)
    payloadObject <-
      note "moneyDeposited upcaster: value is not an object"
        (toObject payload)
    case Object.lookup "balanceAfter" payloadObject of
      Just _ -> pure json
      Nothing -> do
        amountJson <-
          note "moneyDeposited upcaster: missing field: amount"
            (Object.lookup "amount" payloadObject)
        amount <-
          Bifunctor.lmap printJsonDecodeError
            (decodeJson amountJson :: Either _ Int)
        pure
          ( fromObject
              ( Object.insert "value"
                  ( fromObject
                      ( Object.insert "balanceAfter"
                          (encodeJson amount)
                          payloadObject
                      )
                  )
                  object
              )
          )

moneyDepositedUpcaster :: Upcaster
moneyDepositedUpcaster = upcaster 1 upcastMoneyDeposited

accountEventSchema :: Schema AccountEvent
accountEventSchema = schema 2 accountEventCodec [ moneyDepositedUpcaster ]

versionedAccountEventCodec :: Codec AccountEvent AccountEvent
versionedAccountEventCodec = versionedCodec accountEventSchema

unAccountId :: AccountId -> String
unAccountId (AccountId value) = value

unCustomerId :: CustomerId -> String
unCustomerId (CustomerId value) = value
