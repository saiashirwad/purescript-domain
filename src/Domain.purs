-- | Declare an aggregate's labels once, in a spec record type:
-- |
-- | ```purescript
-- | type Account =
-- |   { id :: AccountId, state :: AccountState
-- |   , command :: Variant (depositMoney :: { amount :: Int }, ...)
-- |   , event :: Variant (moneyDeposited :: Movement, ...)
-- |   , rejection :: Variant (notOpen :: {}, ...)
-- |   , reply :: { balance :: Int }
-- |   }
-- | ```
-- |
-- | Every other type takes that spec as its first parameter: `Aggregate
-- | Account`, `Command Account`, `View Account AccountId Summary`, `Policy
-- | Customer Account`. Handlers are records keyed by label. The library walks
-- | them against the spec, so a missing or misspelled handler is one compile
-- | error that names the label.
module Domain (module Domain.Internal.Kernel, module Domain.Internal.Id) where

import Domain.Internal.Id
  ( CommandId
  , Sequence
  , StreamId
  , Version
  , commandIdText
  , parseCommandId
  , sequenceNumber
  , streamIdText
  , versionNumber
  )
import Domain.Internal.Kernel
  ( class Labelled
  , class Spec
  , Aggregate
  , Command
  , Decision
  , Envelope
  , Event
  , Order
  , Policy
  , Receipt
  , Rejection
  , View
  , aggregate
  , command
  , commit
  , emit
  , event
  , fromVariant
  , order
  , policy
  , reject
  , rejection
  , replay
  , toVariant
  , view
  )
