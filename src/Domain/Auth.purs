module Domain.Auth
  ( Principal
  , Predicate(..)
  , anyone
  , allows
  , hasRole
  ) where

import Prelude

import Data.Functor.Contravariant (class Contravariant)
import Data.Set (Set)
import Data.Set as Set

-- | The identity a request is executed for. Roles are strings because they
-- | cross an authentication boundary; the domain can map them to stronger
-- | types when it needs to.
type Principal =
  { subject :: String
  , roles :: Set String
  }

-- | A permission check. `cmap` adapts a predicate on a small principal to a
-- | predicate on a richer one, and `<>` is conjunction.
newtype Predicate a = Predicate (a -> Boolean)

instance contravariantPredicate :: Contravariant Predicate where
  cmap f (Predicate predicate) = Predicate (predicate <<< f)

instance semigroupPredicate :: Semigroup (Predicate a) where
  append (Predicate left) (Predicate right) =
    Predicate \value -> left value && right value

instance monoidPredicate :: Monoid (Predicate a) where
  mempty = Predicate \_ -> true

anyone :: forall a. Predicate a
anyone = mempty

allows :: forall a. Predicate a -> a -> Boolean
allows (Predicate predicate) = predicate

hasRole :: String -> Predicate Principal
hasRole role = Predicate \principal -> Set.member role principal.roles
