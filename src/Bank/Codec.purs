-- | The stored form of account events. Version 1 of `moneyDeposited`
-- | predates `balanceAfter`; for a single deposit the balance after it is the
-- | amount itself.
module Bank.Codec (accountEvents) where

import Prelude

import Bank.Account (AccountEvent)
import Data.Argonaut.Core (Json, fromObject, toObject, toString)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Domain.Codec (Codec, labelled, upcaster, versioned)
import Foreign.Object as Object

accountEvents :: Codec AccountEvent
accountEvents = versioned 2 [ upcaster 1 addBalanceAfter ] labelled

addBalanceAfter :: Json -> Either String Json
addBalanceAfter json = do
  object <- note "expected a labelled event" (toObject json)
  case Object.lookup "type" object >>= toString, Object.lookup "value" object >>= toObject of
    Just "moneyDeposited", Just payload
      | not (Object.member "balanceAfter" payload) -> do
        amount <- note "moneyDeposited v1: missing amount" (Object.lookup "amount" payload)
        pure
          ( fromObject
              ( Object.insert "value" (fromObject (Object.insert "balanceAfter" amount payload))
                  object
              )
          )
    _, _ -> pure json
