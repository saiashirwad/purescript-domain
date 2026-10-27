-- | Identifiers the runtime owns. Constructors stay private: callers parse
-- | a `CommandId` at the boundary, and the runtime is the only writer of
-- | streams, versions, and sequences.
module Domain.Id
  ( CommandId
  , StreamId
  , Version
  , Sequence
  , parseCommandId
  , commandIdText
  , streamIdText
  , parseVersion
  , versionNumber
  , sequenceNumber
  , zeroVersion
  , streamOf
  , advance
  , sequencesAfter
  , derivedCommandId
  )
  where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String

newtype CommandId = CommandId String

derive newtype instance Eq CommandId
derive newtype instance Ord CommandId
derive newtype instance Show CommandId

-- | `<category>/<id>`, where the category is the aggregate's context label.
newtype StreamId = StreamId String

derive newtype instance Eq StreamId
derive newtype instance Ord StreamId
derive newtype instance Show StreamId

-- | The number of events a stream holds.
newtype Version = Version Int

derive newtype instance Eq Version
derive newtype instance Ord Version
derive newtype instance Show Version

-- | An event's 1-based position in its stream.
newtype Sequence = Sequence Int

derive newtype instance Eq Sequence
derive newtype instance Ord Sequence
derive newtype instance Show Sequence

parseCommandId :: String -> Maybe CommandId
parseCommandId raw
  | String.null (String.trim raw) = Nothing
  | otherwise = Just (CommandId raw)

commandIdText :: CommandId -> String
commandIdText (CommandId value) = value

streamIdText :: StreamId -> String
streamIdText (StreamId value) = value

parseVersion :: Int -> Maybe Version
parseVersion value
  | value < 0 = Nothing
  | otherwise = Just (Version value)

versionNumber :: Version -> Int
versionNumber (Version value) = value

sequenceNumber :: Sequence -> Int
sequenceNumber (Sequence value) = value

zeroVersion :: Version
zeroVersion = Version 0

streamOf :: String -> String -> StreamId
streamOf category id = StreamId (category <> "/" <> id)

advance :: Version -> Int -> Version
advance (Version value) count = Version (value + count)

-- | The sequences of `count` events appended after `version`.
sequencesAfter :: Version -> Int -> Array Sequence
sequencesAfter (Version value) count
  | count <= 0 = []
  | otherwise = map Sequence (Array.range (value + 1) (value + count))

-- | The id of the `index`th command a policy named `policy` orders in
-- | reaction to the event at `seq` in `stream`. The same event always
-- | yields the same id, so redelivery is harmless, and two policies
-- | reacting to one event never collide.
derivedCommandId :: String -> StreamId -> Sequence -> Int -> CommandId
derivedCommandId policy (StreamId stream) (Sequence seq) index =
  CommandId (policy <> ":" <> stream <> "@" <> show seq <> "#" <> show index)
