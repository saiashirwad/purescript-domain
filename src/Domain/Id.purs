module Domain.Id
  ( IdError(..)
  , CommandId
  , StreamId
  , TenantId
  , Version
  , Sequence
  , parseCommandId
  , parseStreamId
  , parseTenantId
  , parseVersion
  , parseSequence
  , commandIdText
  , streamIdText
  , tenantIdText
  , versionNumber
  , sequenceNumber
  , zeroVersion
  , nextVersion
  , nextVersionBy
  , firstSequence
  , nextSequence
  ) where

import Prelude

import Data.Either (Either(..))
import Data.String as String
import Domain.Id.Internal as Internal

type CommandId = Internal.CommandId

type StreamId = Internal.StreamId

type TenantId = Internal.TenantId

type Version = Internal.Version

type Sequence = Internal.Sequence

data IdError
  = EmptyId
  | NegativeVersion
  | NegativeSequence

derive instance eqIdError :: Eq IdError
derive instance ordIdError :: Ord IdError

instance showIdError :: Show IdError where
  show EmptyId = "EmptyId"
  show NegativeVersion = "NegativeVersion"
  show NegativeSequence = "NegativeSequence"

parseCommandId :: String -> Either IdError CommandId
parseCommandId value
  | isBlank value = Left EmptyId
  | otherwise = Right (Internal.CommandId value)

parseStreamId :: String -> Either IdError StreamId
parseStreamId value
  | isBlank value = Left EmptyId
  | otherwise = Right (Internal.StreamId value)

parseTenantId :: String -> Either IdError TenantId
parseTenantId value
  | isBlank value = Left EmptyId
  | otherwise = Right (Internal.TenantId value)

parseVersion :: Int -> Either IdError Version
parseVersion value
  | value < 0 = Left NegativeVersion
  | otherwise = Right (Internal.Version value)

parseSequence :: Int -> Either IdError Sequence
parseSequence value
  | value < 1 = Left NegativeSequence
  | otherwise = Right (Internal.Sequence value)

commandIdText :: CommandId -> String
commandIdText (Internal.CommandId value) = value

streamIdText :: StreamId -> String
streamIdText (Internal.StreamId value) = value

tenantIdText :: TenantId -> String
tenantIdText (Internal.TenantId value) = value

versionNumber :: Version -> Int
versionNumber (Internal.Version value) = value

sequenceNumber :: Sequence -> Int
sequenceNumber (Internal.Sequence value) = value

zeroVersion :: Version
zeroVersion = Internal.Version 0

nextVersion :: Version -> Version
nextVersion (Internal.Version value) = Internal.Version (value + 1)

nextVersionBy :: Version -> Int -> Version
nextVersionBy (Internal.Version value) count = Internal.Version (value + count)

firstSequence :: Sequence
firstSequence = Internal.Sequence 1

nextSequence :: Sequence -> Sequence
nextSequence (Internal.Sequence value) = Internal.Sequence (value + 1)

isBlank :: String -> Boolean
isBlank = String.null <<< String.trim
