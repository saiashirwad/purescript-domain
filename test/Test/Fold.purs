module Test.Fold (testFold) where

import Prelude

import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String as String
import Domain.Fold (Fold)
import Domain.Fold as Fold
import Effect.Aff (Aff)
import Test.Support (assertEqual)

testFold :: Aff Unit
testFold = do
  let
    total = Fold.foldMap Additive
    count = Fold.foldMap (const (Additive 1))
    mean = (\t n -> unwrap t / unwrap n) <$> total <*> count
    lengths = Fold.premap String.length total
  assertEqual "<*> runs both folds over the same inputs" 4 (Fold.run mean [ 2, 4, 6 ])
  assertEqual "pure ignores its inputs" 7 (Fold.run (pure 7 :: Fold Int Int) [ 1, 2 ])
  assertEqual "premap feeds the fold a function of each input" 5
    (unwrap (Fold.run lengths [ "ab", "cde" ]))
  assertEqual "a fold over nothing is its initial value" 0
    (Fold.run (Fold.fold 0 (+)) ([] :: Array Int))
