module Domain.Id.Internal
  ( CommandId(..)
  , StreamId(..)
  , TenantId(..)
  , Version(..)
  , Sequence(..)
  ) where

import Prelude

newtype CommandId = CommandId String

derive newtype instance eqCommandId :: Eq CommandId
derive newtype instance ordCommandId :: Ord CommandId
derive newtype instance showCommandId :: Show CommandId

newtype StreamId = StreamId String

derive newtype instance eqStreamId :: Eq StreamId
derive newtype instance ordStreamId :: Ord StreamId
derive newtype instance showStreamId :: Show StreamId

newtype TenantId = TenantId String

derive newtype instance eqTenantId :: Eq TenantId
derive newtype instance ordTenantId :: Ord TenantId
derive newtype instance showTenantId :: Show TenantId

newtype Version = Version Int

derive newtype instance eqVersion :: Eq Version
derive newtype instance ordVersion :: Ord Version
derive newtype instance showVersion :: Show Version

newtype Sequence = Sequence Int

derive newtype instance eqSequence :: Eq Sequence
derive newtype instance ordSequence :: Ord Sequence
derive newtype instance showSequence :: Show Sequence
