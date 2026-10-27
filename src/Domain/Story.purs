-- | Given/when/then stories against an aggregate's pure decider.
-- |
-- | ```purescript
-- | story account
-- |   [ given (AccountOpened { owner: CustomerId "sam" })
-- |   , sends (DepositMoney { amount: 100 })
-- |   , emits (MoneyDeposited { amount: 100, balanceAfter: 100 })
-- |   , replies { balance: 100 }
-- |   ]
-- | ```
module Domain.Story (Step, given, sends, emits, rejects, replies, story) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), fromMaybe)
import Domain.Internal.Kernel (Aggregate(..), commit)

data Step c e r b
  = Given e
  | Sends c
  | Emits e
  | Rejects r
  | Replies b

given :: forall c e r b. e -> Step c e r b
given = Given

sends :: forall c e r b. c -> Step c e r b
sends = Sends

emits :: forall c e r b. e -> Step c e r b
emits = Emits

rejects :: forall c e r b. r -> Step c e r b
rejects = Rejects

replies :: forall c e r b. b -> Step c e r b
replies = Replies

type Acc e r s b =
  { state :: s
  , events :: Array e
  , expected :: Maybe (Array e)
  , rejection :: Maybe r
  , reply :: Maybe b
  }

-- | Runs the steps in order and returns the final state, or the first
-- | expectation that failed. The `emits` steps after a command list its
-- | events exactly, in order.
story ::
  forall id c e r s b.
  Eq e =>
  Show e =>
  Eq r =>
  Show r =>
  Eq b =>
  Show b =>
  Aggregate id c e r s b ->
  Array (Step c e r b) ->
  Either String s
story aggregate@(Aggregate a) steps = _.state <$> (settle =<< foldM step start steps)
  where
  start :: Acc e r s b
  start = { state: a.initial, events: [], expected: Nothing, rejection: Nothing, reply: Nothing }

  step :: Acc e r s b -> Step c e r b -> Either String (Acc e r s b)
  step acc = case _ of
    Emits expected -> Right acc { expected = Just (fromMaybe [] acc.expected <> [ expected ]) }
    Given past -> settle acc <#> \ok -> (cleared ok) { state = a.evolve ok.state past }
    Sends sent -> settle acc <#> \ok -> case commit aggregate ok.state sent of
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

  settle :: Acc e r s b -> Either String (Acc e r s b)
  settle acc = case acc.expected of
    Just expected
      | expected /= acc.events ->
        Left ("expected events " <> show expected <> ", got " <> show acc.events)
    _ -> Right acc { expected = Nothing }

  cleared :: Acc e r s b -> Acc e r s b
  cleared acc = acc { events = [], rejection = Nothing, reply = Nothing }
