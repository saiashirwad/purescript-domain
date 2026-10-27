-- | Label lookups that turn the common mistakes (a missing handler, a
-- | misspelled label, a payload of the wrong type) into one compiler error
-- | that names the label. Every lookup is an instance chain that fails with
-- | a custom message, and no caller puts a `Prim.Row` constraint on a label
-- | it has not checked first.
module Domain.Labels
  ( class Field
  , class IsVariant
  , class SubRow
  , class Has
  , class Covers
  , class HasLabel
  ) where

import Data.Variant (Variant)
import Prim.RowList (Cons, Nil, RowList)
import Prim.TypeError (class Fail, Above, Beside, Quote, QuoteLabel, Text)

-- | `l` is a label of `rl`; otherwise compilation fails with `what` followed
-- | by the label.
class HasLabel :: Symbol -> Symbol -> RowList Type -> Constraint
class HasLabel what l rl

instance HasLabel what l (Cons l x t)
else instance HasLabel what l t => HasLabel what l (Cons m x t)
else instance Fail (Beside (Text what) (QuoteLabel l)) => HasLabel what l Nil

-- | Every label of `expected` has an entry in `given`.
class Covers :: Symbol -> RowList Type -> RowList Type -> Constraint
class Covers what expected given

instance Covers what Nil given
instance (HasLabel what l given, Covers what t given) => Covers what (Cons l x t) given

-- | The type of field `l` in `rl`.
class Field :: Symbol -> Symbol -> RowList Type -> Type -> Constraint
class Field what l rl a | l rl -> a

instance Field what l (Cons l a t) a
else instance Field what l t a => Field what l (Cons m x t) a
else instance Fail (Beside (Text what) (QuoteLabel l)) => Field what l Nil a

class IsVariant :: Symbol -> Type -> Row Type -> Constraint
class IsVariant what t row | t -> row

instance IsVariant what (Variant row) row
else instance
  Fail (Beside (Text what) (Beside (Text " must be a Variant, not ") (Quote t))) =>
  IsVariant what t row

-- | Every label of `small` appears in `big` with the same payload type.
class SubRow :: Symbol -> RowList Type -> RowList Type -> Constraint
class SubRow what small big

instance SubRow what Nil big
instance (Has what l a big, SubRow what t big) => SubRow what (Cons l a t) big

class Has :: Symbol -> Symbol -> Type -> RowList Type -> Constraint
class Has what l a rl

instance Has what l a (Cons l a t)
else instance
  Fail
    ( Above
        (Beside (Text "the payload of ") (QuoteLabel l))
        (Beside (Text "is ") (Beside (Quote a) (Beside (Text " here but ") (Beside (Quote b) (Text " in the spec")))))
    ) =>
  Has what l a (Cons l b t)
else instance Has what l a t => Has what l a (Cons m x t)
else instance Fail (Beside (Text what) (QuoteLabel l)) => Has what l a Nil
