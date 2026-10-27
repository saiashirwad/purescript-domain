-- | Given/when/then stories against an aggregate's pure decider.
-- |
-- | ```purescript
-- | story account
-- |   [ given @"accountOpened" { owner: "sam" }
-- |   , command @"depositMoney" { amount: 100 }
-- |   , emits @"moneyDeposited" { amount: 100, balanceAfter: 100 }
-- |   , replies { balance: 100 }
-- |   ]
-- | ```
module Domain.Story
  ( Step
  , given
  , command
  , emits
  , rejects
  , replies
  , story
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (elem, foldM)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Domain (class Spec, Aggregate, Command, Event, Rejection, cmd, commit, decider, event, rejection)
import Domain.Labels (class Field)
import Prim.RowList (class RowToList)

data Step spec b
  = Given (Event spec)
  | When (Command spec)
  | Emits (Event spec)
  | Rejects (Rejection spec)
  | Replies b

given
  :: forall @l spec id s c e r b a erl
   . Spec spec id s c e r b
  => RowToList e erl
  => Field "the aggregate declares no event " l erl a
  => IsSymbol l
  => a
  -> Step spec b
given = Given <<< event @l

command
  :: forall @l spec id s c e r b a crl
   . Spec spec id s c e r b
  => RowToList c crl
  => Field "the aggregate declares no command " l crl a
  => IsSymbol l
  => a
  -> Step spec b
command = When <<< cmd @l

emits
  :: forall @l spec id s c e r b a erl
   . Spec spec id s c e r b
  => RowToList e erl
  => Field "the aggregate declares no event " l erl a
  => IsSymbol l
  => a
  -> Step spec b
emits = Emits <<< event @l

rejects
  :: forall @l spec id s c e r b a rrl
   . Spec spec id s c e r b
  => RowToList r rrl
  => Field "the aggregate declares no rejection " l rrl a
  => IsSymbol l
  => a
  -> Step spec b
rejects = Rejects <<< rejection @l

replies :: forall spec b. b -> Step spec b
replies = Replies

type Acc spec s b =
  { state :: s
  , lastEvents :: Array (Event spec)
  , lastRejection :: Maybe (Rejection spec)
  , lastReply :: Maybe b
  }

-- | Runs the steps in order and returns the final state, or the first
-- | expectation that failed.
story
  :: forall spec id s c e r b
   . Spec spec id s c e r b
  => Eq (Event spec)
  => Show (Event spec)
  => Eq (Rejection spec)
  => Show (Rejection spec)
  => Eq b
  => Show b
  => Aggregate spec
  -> Array (Step spec b)
  -> Either String s
story aggregate steps = map _.state (foldM step start steps)
  where
  d = decider aggregate

  start :: Acc spec s b
  start =
    { state: d.initial
    , lastEvents: []
    , lastRejection: Nothing
    , lastReply: Nothing
    }

  step :: Acc spec s b -> Step spec b -> Either String (Acc spec s b)
  step acc = case _ of
    Given fact ->
      Right acc
        { state = d.evolve acc.state fact
        , lastEvents = []
        , lastRejection = Nothing
        , lastReply = Nothing
        }
    When sent -> Right case commit aggregate acc.state sent of
      Left refused ->
        acc { lastEvents = [], lastRejection = Just refused, lastReply = Nothing }
      Right committed ->
        acc
          { state = committed.state
          , lastEvents = committed.events
          , lastRejection = Nothing
          , lastReply = Just committed.reply
          }
    Emits expected
      | elem expected acc.lastEvents -> Right acc
      | otherwise -> Left ("expected event " <> show expected <> ", got " <> show acc.lastEvents)
    Rejects expected
      | acc.lastRejection == Just expected -> Right acc
      | otherwise -> Left ("expected rejection " <> show expected <> ", got " <> show acc.lastRejection)
    Replies expected
      | acc.lastReply == Just expected -> Right acc
      | otherwise -> Left ("expected reply " <> show expected <> ", got " <> show acc.lastReply)
