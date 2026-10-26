module Domain.Story
  ( Step(..)
  , StoryResult
  , runStory
  ) where

import Prelude

import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Foldable (elem, foldl)
import Data.Maybe (Maybe(..))
import Domain (Decider, commit)

-- | A story is a list of steps in the business's own words:
-- | given this history, when this command, then these events.
data Step e c r b
  = Given e
  | When c
  | Then e
  | Rejected r
  | Replied b

type StoryResult s e r b =
  { state :: s
  , history :: Array e
  , lastEvents :: Array e
  , lastReply :: Maybe b
  , lastRejection :: Maybe r
  }

type Acc s e r b =
  { state :: s
  , version :: Int
  , history :: Array e
  , lastEvents :: Array e
  , lastReply :: Maybe b
  , lastRejection :: Maybe r
  }

runStory
  :: forall s c e r b
   . Eq e
  => Show e
  => Eq r
  => Show r
  => Eq b
  => Show b
  => Decider s c e r b
  -> Array (Step e c r b)
  -> Either String (StoryResult s e r b)
runStory decider steps =
  map toResult (foldl step (Right initial) steps)
  where
  initial =
    { state: decider.initial
    , version: 0
    , history: []
    , lastEvents: []
    , lastReply: Nothing
    , lastRejection: Nothing
    }

  step (Left error) _ = Left error
  step (Right acc) current = case current of
    Given event ->
      Right acc
        { state = decider.evolve acc.state event
        , history = snoc acc.history event
        , lastEvents = []
        , lastReply = Nothing
        , lastRejection = Nothing
        }
    When command ->
      case commit decider { state: acc.state, version: acc.version, stream: "story" } command of
        Left rejection ->
          Right acc
            { lastEvents = []
            , lastReply = Nothing
            , lastRejection = Just rejection
            }
        Right result ->
          Right acc
            { state = result.state
            , version = result.receipt.version
            , history = acc.history <> result.receipt.events
            , lastEvents = result.receipt.events
            , lastReply = Just result.receipt.reply
            , lastRejection = Nothing
            }
    Then event ->
      if elem event acc.lastEvents then Right acc
      else Left ("expected event " <> show event <> ", got " <> show acc.lastEvents)
    Rejected rejection ->
      if acc.lastRejection == Just rejection then Right acc
      else Left ("expected rejection " <> show rejection <> ", got " <> show acc.lastRejection)
    Replied reply ->
      if acc.lastReply == Just reply then Right acc
      else Left ("expected reply " <> show reply <> ", got " <> show acc.lastReply)

  toResult acc =
    { state: acc.state
    , history: acc.history
    , lastEvents: acc.lastEvents
    , lastReply: acc.lastReply
    , lastRejection: acc.lastRejection
    }
