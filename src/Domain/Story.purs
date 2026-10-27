-- | Given/when/then stories against an aggregate's pure decider.
-- |
-- | ```purescript
-- | story account
-- |   [ given @"accountOpened" { owner: CustomerId "sam" }
-- |   , sends @"depositMoney" { amount: 100 }
-- |   , emits @"moneyDeposited" { amount: 100, balanceAfter: 100 }
-- |   , replies { balance: 100 }
-- |   ]
-- | ```
module Domain.Story (Step, given, sends, emits, rejects, replies, story) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol)
import Domain.Internal.Kernel
  (class Spec, Aggregate, Command, Event, Rejection, command, commit, decider, event, rejection)
import Domain.Internal.Labels (class FieldOf)

data Step spec b
  = Given (Event spec)
  | When (Command spec)
  | Emits (Event spec)
  | Rejects (Rejection spec)
  | Replies b

given ::
  forall @l spec id s c e r b a.
  Spec spec id s c e r b =>
  FieldOf "the aggregate declares no event " l e a =>
  IsSymbol l =>
  a ->
  Step spec b
given = Given <<< event @l

sends ::
  forall @l spec id s c e r b a.
  Spec spec id s c e r b =>
  FieldOf "the aggregate declares no command " l c a =>
  IsSymbol l =>
  a ->
  Step spec b
sends = When <<< command @l

emits ::
  forall @l spec id s c e r b a.
  Spec spec id s c e r b =>
  FieldOf "the aggregate declares no event " l e a =>
  IsSymbol l =>
  a ->
  Step spec b
emits = Emits <<< event @l

rejects ::
  forall @l spec id s c e r b a.
  Spec spec id s c e r b =>
  FieldOf "the aggregate declares no rejection " l r a =>
  IsSymbol l =>
  a ->
  Step spec b
rejects = Rejects <<< rejection @l

replies :: forall spec b. b -> Step spec b
replies = Replies

type Acc spec s b =
  { state :: s
  , events :: Array (Event spec)
  , expected :: Maybe (Array (Event spec))
  , rejection :: Maybe (Rejection spec)
  , reply :: Maybe b
  }

-- | Runs the steps in order and returns the final state, or the first
-- | expectation that failed. The `emits` steps after a command list its
-- | events exactly, in order.
story ::
  forall spec id s c e r b.
  Spec spec id s c e r b =>
  Eq (Event spec) =>
  Show (Event spec) =>
  Eq (Rejection spec) =>
  Show (Rejection spec) =>
  Eq b =>
  Show b =>
  Aggregate spec ->
  Array (Step spec b) ->
  Either String s
story aggregate steps = _.state <$> (settle =<< foldM step start steps)
  where
  d = decider aggregate

  start :: Acc spec s b
  start = { state: d.initial, events: [], expected: Nothing, rejection: Nothing, reply: Nothing }

  step :: Acc spec s b -> Step spec b -> Either String (Acc spec s b)
  step acc = case _ of
    Emits expected -> Right acc { expected = Just (fromMaybe [] acc.expected <> [ expected ]) }
    Given past -> settle acc <#> \ok -> (cleared ok) { state = d.evolve ok.state past }
    When sent -> settle acc <#> \ok -> case commit aggregate ok.state sent of
      Left rejected -> (cleared ok) { rejection = Just rejected }
      Right committed ->
        (cleared ok)
          { state = committed.state, events = committed.events, reply = Just committed.reply }
    Rejects expected -> settle acc >>= \ok ->
      if ok.rejection == Just expected
      then Right ok
      else Left ("expected rejection " <> show expected <> ", got " <> show ok.rejection)
    Replies expected -> settle acc >>= \ok ->
      if ok.reply == Just expected
      then Right ok
      else Left ("expected reply " <> show expected <> ", got " <> show ok.reply)

  settle :: Acc spec s b -> Either String (Acc spec s b)
  settle acc = case acc.expected of
    Just expected
      | expected /= acc.events ->
        Left ("expected events " <> show expected <> ", got " <> show acc.events)
    _ -> Right acc { expected = Nothing }

  cleared :: Acc spec s b -> Acc spec s b
  cleared acc = acc { events = [], rejection = Nothing, reply = Nothing }
