module Domain.Iris.Deriving
  ( WrappedChanges(..)
  , IrisCodec(..)
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Profunctor (class Profunctor)
import Data.Traversable (class Traversable)
import Domain (Changes)
import Domain.Codec (Codec)

-- | The same shape as `Domain.Changes`, but the instances come from Iris's
-- | deriving rather than hand-written code. This module is compiled only by
-- | Iris; the portable package keeps the manual instances.
newtype WrappedChanges a = WrappedChanges (Changes a)

derive newtype instance functorWrappedChanges :: Functor WrappedChanges
derive newtype instance foldableWrappedChanges :: Foldable WrappedChanges
derive newtype instance traversableWrappedChanges :: Traversable WrappedChanges

-- | A codec is a profunctor. Iris can derive the instance for a newtype
-- | wrapper; `dimap` below is the check that the derivation is real.
newtype IrisCodec a b = IrisCodec (Codec a b)

derive newtype instance profunctorIrisCodec :: Profunctor IrisCodec
