-- | Write an aggregate as plain data and functions:
-- |
-- | ```purescript
-- | data AccountCommand = OpenAccount { owner :: CustomerId } | DepositMoney { amount :: Int } | ...
-- | data AccountEvent = AccountOpened { owner :: CustomerId } | MoneyDeposited Movement | ...
-- |
-- | account :: Aggregate AccountId AccountCommand AccountEvent AccountRejection AccountState Balance
-- | account = Aggregate { initial: Unopened, decide, evolve, reply }
-- | ```
-- |
-- | Commands, events, and rejections derive `Generic`; a constructor's name,
-- | first letter lowered, is its label in routes, spans, and JSON. Views are
-- | folds from `Domain.Fold`, keyed with `byId` or `keyed`. Policies are
-- | functions from a change to orders.
module Domain (module Domain.Internal.Kernel, module Domain.Internal.Id, module Domain.Internal.Labels) where

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
  ( Aggregate(..)
  , Change
  , Envelope
  , Order
  , Policy(..)
  , Receipt
  , View
  , byId
  , commit
  , keyed
  , replay
  )
import Domain.Internal.Labels (labelOf)
