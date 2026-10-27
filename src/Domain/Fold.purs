-- | Left folds as values. A `Fold i o` holds its current result and the
-- | fold it becomes after one more input, so folds compose with `<$>`,
-- | `<*>`, and `lcmap` before anything runs. Import it qualified:
-- |
-- | ```purescript
-- | import Domain.Fold as Fold
-- |
-- | average :: Fold Number Number
-- | average = (/) <$> Fold.foldMap Additive <*> Fold.foldMap (const (Additive 1.0))
-- | ```
module Domain.Fold (Fold, fold, foldMap, premap, step, value, run) where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Profunctor (class Profunctor, dimap, lcmap)

newtype Fold i o = Fold { value :: o, step :: i -> Fold i o }

-- | `fold initial next` starts at `initial` and applies `next` to each input.
fold :: forall i o. o -> (o -> i -> o) -> Fold i o
fold current next = Fold { value: current, step: \input -> fold (next current input) next }

-- | Maps each input into a monoid and appends the results.
foldMap :: forall i o. Monoid o => (i -> o) -> Fold i o
foldMap f = fold mempty \acc input -> acc <> f input

-- | Feeds the fold a function of each input.
premap :: forall i j o. (j -> i) -> Fold i o -> Fold j o
premap = lcmap

value :: forall i o. Fold i o -> o
value (Fold f) = f.value

step :: forall i o. Fold i o -> i -> Fold i o
step (Fold f) = f.step

-- | Runs the fold over every input and returns the result.
run :: forall f i o. Foldable f => Fold i o -> f i -> o
run f = value <<< foldl step f

instance Functor (Fold i) where
  map f (Fold m) = Fold { value: f m.value, step: map f <<< m.step }

instance Apply (Fold i) where
  apply (Fold f) (Fold x) = Fold { value: f.value x.value, step: \input -> apply (f.step input) (x.step input) }

instance Applicative (Fold i) where
  pure o = Fold { value: o, step: \_ -> pure o }

instance Profunctor Fold where
  dimap l r (Fold m) = Fold { value: r m.value, step: dimap l r <<< m.step <<< l }
