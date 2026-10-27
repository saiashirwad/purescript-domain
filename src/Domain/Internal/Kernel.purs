-- | Specs, labelled values, aggregates, views, and policies. `Domain`
-- | re-exports the part users need. Neither `Erased` nor any constructor of
-- | a spec-indexed type leaves this module, which is what the Soundness
-- | section of docs/design.md rests on.
module Domain.Internal.Kernel
  ( class Spec
  , Command
  , Event
  , Rejection
  , command
  , event
  , rejection
  , class Labelled
  , toVariant
  , fromVariant
  , labelOf
  , Decision
  , emit
  , reject
  , Aggregate
  , aggregate
  , Decider
  , decider
  , commit
  , replay
  , View
  , view
  , runView
  , Policy
  , policy
  , Order
  , order
  , Addressed
  , orders
  , Envelope
  , Receipt
  )
  where

import Prelude

import Data.Array (last, mapWithIndex, scanl)
import Data.Bifunctor (bimap)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (class VariantShows, Unvariant(..), Variant, unvariant)
import Data.Variant.Internal (class VariantTags, VariantRep(..))
import Domain.Internal.Id (CommandId, Sequence, StreamId, Version, derivedCommandId)
import Domain.Internal.Labels
  (class Covers, class Field, class FieldOf, class IsVariant, class SubRow)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.TypeError (class Fail, Above, Beside, Doc, Quote, Text)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Projects an aggregate spec record into its parts by field name. A spec
-- | determines every part, which is what makes `Erased` sound.
class Spec :: Type -> Type -> Type -> Row Type -> Row Type -> Row Type -> Type -> Constraint
class Spec spec id s c e r b | spec -> id s c e r b

instance ( RowToList spec fields
         , Field "an aggregate spec needs the field " "id" fields id
         , Field "an aggregate spec needs the field " "state" fields s
         , Field "an aggregate spec needs the field " "command" fields command
         , Field "an aggregate spec needs the field " "event" fields event
         , Field "an aggregate spec needs the field " "rejection" fields rejection
         , Field "an aggregate spec needs the field " "reply" fields b
         , IsVariant "the spec's command field" command c
         , IsVariant "the spec's event field" event e
         , IsVariant "the spec's rejection field" rejection r
         ) =>
  Spec (Record spec) id s c e r b

-- | A value whose type the spec determines but a type indexed by the spec
-- | alone cannot name. Every `unsafeCoerce` in the library is in `erase`,
-- | `unerase`, `inject`, and `widen`. Soundness in docs/design.md says why
-- | each is safe.
data Erased

erase :: forall a. a -> Erased
erase = unsafeCoerce

unerase :: forall a. Erased -> a
unerase = unsafeCoerce

-- | `inj` for a label that `Field` has checked, so no `Prim.Row` constraint
-- | sees an unchecked label. Sound because `Field` proves `l` carries `a` in
-- | `row`.
inject :: forall @what @l a row. FieldOf what l row a => IsSymbol l => a -> Variant row
inject value = unsafeCoerce (VariantRep { type: reflectSymbol (Proxy :: Proxy l), value })

-- | Sound because `SubRow` proves each label of `small` is in `big` with the
-- | same payload.
widen ::
  forall @what small srl big brl.
  RowToList small srl => RowToList big brl => SubRow what srl brl => Variant small -> Variant big
widen = unsafeCoerce

-- | A variant's label, and its payload hidden until the handler for that
-- | label reads it.
untag :: forall row. Variant row -> { type :: String, value :: Erased }
untag v = case unvariant v of
  Unvariant split -> split \label value -> { type: reflectSymbol label, value: erase value }

-- | A variant of the spec's command row.
newtype Command :: Type -> Type
newtype Command spec = Command Erased

type role Command nominal

-- | A variant of the spec's event row.
newtype Event :: Type -> Type
newtype Event spec = Event Erased

type role Event nominal

-- | A variant of the spec's rejection row.
newtype Rejection :: Type -> Type
newtype Rejection spec = Rejection Erased

type role Rejection nominal

-- | `command @"depositMoney" { amount: 100 }`. The spec comes from the use
-- | site, so no label needs a hand-written constructor.
command ::
  forall @l spec id s c e r b a.
  Spec spec id s c e r b =>
  FieldOf "the aggregate declares no command " l c a =>
  IsSymbol l =>
  a ->
  Command spec
command = fromVariant <<< inject @"the aggregate declares no command " @l

event ::
  forall @l spec id s c e r b a.
  Spec spec id s c e r b =>
  FieldOf "the aggregate declares no event " l e a =>
  IsSymbol l =>
  a ->
  Event spec
event = fromVariant <<< inject @"the aggregate declares no event " @l

rejection ::
  forall @l spec id s c e r b a.
  Spec spec id s c e r b =>
  FieldOf "the aggregate declares no rejection " l r a =>
  IsSymbol l =>
  a ->
  Rejection spec
rejection = fromVariant <<< inject @"the aggregate declares no rejection " @l

-- | Commands, events, and rejections are variants of their spec's rows.
class Labelled :: Type -> Row Type -> Constraint
class Labelled a row | a -> row where
  toVariant :: a -> Variant row
  fromVariant :: Variant row -> a

instance Spec spec id s c e r b => Labelled (Command spec) c where
  toVariant (Command v) = unerase v
  fromVariant = Command <<< erase

instance Spec spec id s c e r b => Labelled (Event spec) e where
  toVariant (Event v) = unerase v
  fromVariant = Event <<< erase

instance Spec spec id s c e r b => Labelled (Rejection spec) r where
  toVariant (Rejection v) = unerase v
  fromVariant = Rejection <<< erase

-- | The label of a command, event, or rejection.
labelOf :: forall a row. Labelled a row => a -> String
labelOf = _.type <<< untag <<< toVariant

instance (Spec spec id s c e r b, Eq (Variant c)) => Eq (Command spec) where
  eq x y = toVariant x == toVariant y

instance (Spec spec id s c e r b, Eq (Variant e)) => Eq (Event spec) where
  eq x y = toVariant x == toVariant y

instance (Spec spec id s c e r b, Eq (Variant r)) => Eq (Rejection spec) where
  eq x y = toVariant x == toVariant y

instance (Spec spec id s c e r b, RowToList c rl, VariantTags rl, VariantShows rl) =>
  Show (Command spec) where
  show = show <<< toVariant

instance (Spec spec id s c e r b, RowToList e rl, VariantTags rl, VariantShows rl) =>
  Show (Event spec) where
  show = show <<< toVariant

instance (Spec spec id s c e r b, RowToList r rl, VariantTags rl, VariantShows rl) =>
  Show (Rejection spec) where
  show = show <<< toVariant

-- | What one handler decides. Each handler names the narrow rows it may
-- | produce, so a deposit cannot return `insufficientFunds`; the library
-- | checks them against the spec and widens them.
type Decision r e = Either (Variant r) (Array (Variant e))

-- | Record one event. Combine with `<>` to record several.
emit ::
  forall @l a e r.
  FieldOf "the handler's event row does not include " l e a => IsSymbol l => a -> Decision r e
emit payload = Right [ inject @"the handler's event row does not include " @l payload ]

reject ::
  forall @l a r e.
  FieldOf "the handler's rejection row does not include " l r a => IsSymbol l => a -> Decision r e
reject payload = Left (inject @"the handler's rejection row does not include " @l payload)

-- | A pure decider in spec terms. `evolve` is total, because an event has
-- | already happened.
type Decider spec s b =
  { initial :: s
  , decide :: s -> Command spec -> Either (Rejection spec) (Array (Event spec))
  , evolve :: s -> Event spec -> s
  , reply :: s -> b
  }

newtype Aggregate :: Type -> Type
newtype Aggregate spec = Aggregate Erased

type role Aggregate nominal

-- | Build an aggregate from handler records keyed by label. `decide` and
-- | `evolve` must cover the spec's rows exactly.
aggregate ::
  forall spec id s c e r b dh eh crl erl drl ehl.
  Spec spec id s c e r b =>
  RowToList c crl =>
  RowToList e erl =>
  RowToList dh drl =>
  RowToList eh ehl =>
  Covers "decide is missing a handler for the command " crl drl =>
  Covers "evolve is missing a handler for the event " erl ehl =>
  DecideTable drl dh s spec =>
  Table "evolve handles an undeclared event " ehl eh erl s s =>
  { initial :: s, decide :: Record dh, evolve :: Record eh, reply :: s -> b } ->
  Aggregate spec
aggregate definition = Aggregate (erase typed)
  where
  decisions = decideTable (Proxy :: Proxy drl) definition.decide
  transitions = table (Proxy :: Proxy "evolve handles an undeclared event ") (Proxy :: Proxy erl)
    (Proxy :: Proxy ehl) definition.evolve

  typed :: Decider spec s b
  typed =
    { initial: definition.initial
    , decide: \state sent -> dispatch decisions (\_ -> Right []) state (toVariant sent)
    , evolve: \state happened -> dispatch transitions identity state (toVariant happened)
    , reply: definition.reply
    }

decider :: forall spec id s c e r b. Spec spec id s c e r b => Aggregate spec -> Decider spec s b
decider (Aggregate erased) = unerase erased

-- | Decide, then fold the new events into the state. `states` holds the
-- | state right after each event.
commit ::
  forall spec id s c e r b.
  Spec spec id s c e r b =>
  Aggregate spec ->
  s ->
  Command spec ->
  Either
    (Rejection spec)
    { state :: s, states :: Array s, events :: Array (Event spec), reply :: b }
commit aggregate' state sent = do
  let
    d = decider aggregate'
  events <- d.decide state sent
  let
    states = scanl d.evolve state events
    next = fromMaybe state (last states)
  pure { state: next, states, events, reply: d.reply next }

-- | Replay is a fold, so `replay a s (xs <> ys) == replay a (replay a s xs) ys`.
replay ::
  forall spec id s c e r b. Spec spec id s c e r b => Aggregate spec -> s -> Array (Event spec) -> s
replay aggregate' = foldl (decider aggregate').evolve

-- | A read model over one aggregate's events: one row of `v` per key `k`.
-- | Handlers name only the events the view cares about. Rows are keyed by
-- | the source entity id unless the definition has a `key`, which reads the
-- | entity's state right after each event and names the row the event lands
-- | in, or `Nothing` to skip it.
newtype View :: Type -> Type -> Type -> Type
newtype View spec k v = View { initial :: v, step :: v -> Event spec -> v, key :: Erased }

type role View nominal representational representational

view ::
  forall spec id s c e r b k v def drl hs hrl erl.
  Spec spec id s c e r b =>
  RowToList def drl =>
  ViewDefinition drl def id s k v hs =>
  RowToList hs hrl =>
  RowToList e erl =>
  Table "a view handles an undeclared event " hrl hs erl v v =>
  Record def ->
  View spec k v
view definition = View
  { initial: parts.initial
  , step: \row happened -> dispatch handlers identity row (toVariant happened)
  , key: erase parts.key
  }
  where
  parts :: { initial :: v, on :: Record hs, key :: id -> s -> Maybe k }
  parts = viewDefinition (Proxy :: Proxy drl) definition
  handlers = table (Proxy :: Proxy "a view handles an undeclared event ") (Proxy :: Proxy erl)
    (Proxy :: Proxy hrl) parts.on

runView ::
  forall spec id s c e r b k v.
  Spec spec id s c e r b =>
  View spec k v ->
  { initial :: v, step :: v -> Event spec -> v, key :: id -> s -> Maybe k }
runView (View fold) = fold { key = unerase fold.key }

-- | Reads a view definition: `{ initial, on }`, keyed by the source id, or
-- | `{ initial, key, on }`.
class ViewDefinition ::
  RowList Type -> Row Type -> Type -> Type -> Type -> Type -> Row Type -> Constraint
class ViewDefinition drl def id s k v hs | drl -> v hs where
  viewDefinition ::
    Proxy drl -> Record def -> { initial :: v, on :: Record hs, key :: id -> s -> Maybe k }

instance (Row.Cons "initial" v initialTail def, Row.Cons "on" (Record hs) onTail def) =>
  ViewDefinition (Cons "initial" v (Cons "on" (Record hs) Nil)) def id s id v hs where
  viewDefinition _ definition =
    { initial: Record.get (Proxy :: Proxy "initial") definition
    , on: Record.get (Proxy :: Proxy "on") definition
    , key: \id _ -> Just id
    }
else instance Fail (Beside (Text "a view with rows keyed by ") (Beside (Quote k) (NeedsKey s k))) =>
  ViewDefinition (Cons "initial" v (Cons "on" (Record hs) Nil)) def id s k v hs where
  viewDefinition _ _ = unsafeCrashWith "unreachable: rejected at compile time"
else instance ( Row.Cons "initial" v initialTail def
              , Row.Cons "key" f keyTail def
              , Row.Cons "on" (Record hs) onTail def
              , ViewKey f s k
              ) =>
  ViewDefinition (Cons "initial" v (Cons "key" f (Cons "on" (Record hs) Nil))) def id s k v hs where
  viewDefinition _ definition =
    { initial: Record.get (Proxy :: Proxy "initial") definition
    , on: Record.get (Proxy :: Proxy "on") definition
    , key: \_ -> viewKey (Record.get (Proxy :: Proxy "key") definition)
    }
else instance Fail (Text "a view definition has the fields initial and on, and optionally key") =>
  ViewDefinition drl def id s k v hs where
  viewDefinition _ _ = unsafeCrashWith "unreachable: rejected at compile time"

type NeedsKey :: Type -> Type -> Doc
type NeedsKey s k =
  Beside (Text " needs key :: ") (Beside (Quote s) (Beside (Text " -> Maybe ") (Quote k)))

-- | A view's key reads the aggregate's state and names a row.
class ViewKey :: Type -> Type -> Type -> Constraint
class ViewKey f s k where
  viewKey :: f -> s -> Maybe k

instance ViewKey (s -> Maybe k) s k where
  viewKey = identity
else instance Fail
  ( Above
      (Beside (Text "a view keyed by ") (Beside (Quote k) (NeedsKey s k)))
      (Beside (Text "but its key is ") (Quote f))
  ) =>
  ViewKey f s k where
  viewKey _ _ = unsafeCrashWith "unreachable: rejected at compile time"

-- | Reacts to events of `src` by ordering commands on `tgt`.
newtype Policy :: Type -> Type -> Type
newtype Policy src tgt = Policy Erased

type role Policy nominal nominal

-- | A command a policy wants sent, addressed to a target entity.
newtype Order :: Type -> Type
newtype Order tgt = Order { to :: Erased, command :: Command tgt }

type role Order nominal

policy ::
  forall src id s c e r b tgt hs hrl erl.
  Spec src id s c e r b =>
  RowToList hs hrl =>
  RowToList e erl =>
  Table "a policy reacts to an undeclared event " hrl hs erl id (Array (Order tgt)) =>
  Record hs ->
  Policy src tgt
policy handlers = Policy (erase typed)
  where
  typed :: Map String (id -> Erased -> Array (Order tgt))
  typed = table (Proxy :: Proxy "a policy reacts to an undeclared event ") (Proxy :: Proxy erl)
    (Proxy :: Proxy hrl) handlers

-- | `order @"openAccount" accountId { owner }`.
order ::
  forall @l tgt id s c e r b a.
  Spec tgt id s c e r b =>
  FieldOf "the target aggregate declares no command " l c a =>
  IsSymbol l =>
  id ->
  a ->
  Order tgt
order to payload = Order
  { to: erase to
  , command: fromVariant (inject @"the target aggregate declares no command " @l payload)
  }

-- | An order with its command id derived from the event that caused it.
type Addressed id spec = { commandId :: CommandId, to :: id, command :: Command spec }

-- | The orders a policy, known by `name`, places for one event.
orders ::
  forall src sid ss sc se sr sb tgt tid ts tc te tr tb.
  Spec src sid ss sc se sr sb =>
  Spec tgt tid ts tc te tr tb =>
  String ->
  Policy src tgt ->
  Envelope sid (Event src) ->
  Array (Addressed tid tgt)
orders name (Policy handlers) envelope =
  mapWithIndex address (dispatch typed (\_ -> []) envelope.id (toVariant envelope.event))
  where
  typed :: Map String (sid -> Erased -> Array (Order tgt))
  typed = unerase handlers

  address index (Order placed) =
    { commandId: derivedCommandId name envelope.stream envelope.seq index
    , to: unerase placed.to
    , command: placed.command
    }

-- | A committed event with its address.
type Envelope id ev =
  { id :: id
  , stream :: StreamId
  , seq :: Sequence
  , commandId :: CommandId
  , at :: Instant
  , event :: ev
  }

-- | What a committed command returns. Retrying the command id returns the
-- | same receipt.
type Receipt ev b = { commandId :: CommandId, version :: Version, events :: Array ev, reply :: b }

dispatch ::
  forall x row out. Map String (x -> Erased -> out) -> (x -> out) -> x -> Variant row -> out
dispatch handlers fallback x v =
  let
    { type: tag, value } = untag v
  in
    maybe (fallback x) (\handler -> handler x value) (Map.lookup tag handlers)

-- | Builds the `decide` dispatch table. Each handler's payload comes from
-- | the spec, and its narrow rows must be sub-rows of the spec's.
class DecideTable :: RowList Type -> Row Type -> Type -> Type -> Constraint
class DecideTable rl hs s spec where
  decideTable ::
    Proxy rl ->
    Record hs ->
    Map String (s -> Erased -> Either (Rejection spec) (Array (Event spec)))

instance DecideTable Nil hs s spec where
  decideTable _ _ = Map.empty

instance ( IsSymbol l
         , Spec spec id s c e r b
         , RowToList c crl
         , RowToList e erl
         , RowToList r rrl
         , Field "decide handles an undeclared command " l crl a
         , Row.Cons l (s -> a -> Decision ri ei) hsTail hs
         , RowToList ri rirl
         , RowToList ei eirl
         , SubRow "a decide handler rejects with an undeclared rejection " rirl rrl
         , SubRow "a decide handler emits an undeclared event " eirl erl
         , DecideTable t hs s spec
         ) =>
  DecideTable (Cons l h t) hs s spec where
  decideTable _ handlers =
    Map.insert (reflectSymbol (Proxy :: Proxy l)) run (decideTable (Proxy :: Proxy t) handlers)
    where
    run state payload =
      bimap (fromVariant <<< widen @"a decide handler rejects with an undeclared rejection ")
        (map (fromVariant <<< widen @"a decide handler emits an undeclared event "))
        (Record.get (Proxy :: Proxy l) handlers state (unerase payload))

-- | Builds a dispatch table for handlers of shape `x -> payload -> out`,
-- | where each payload type comes from `labels`.
class Table :: Symbol -> RowList Type -> Row Type -> RowList Type -> Type -> Type -> Constraint
class Table what rl hs labels x out where
  table :: Proxy what -> Proxy labels -> Proxy rl -> Record hs -> Map String (x -> Erased -> out)

instance Table what Nil hs labels x out where
  table _ _ _ _ = Map.empty

instance ( IsSymbol l
         , Field what l labels a
         , Row.Cons l (x -> a -> out) hsTail hs
         , Table what t hs labels x out
         ) =>
  Table what (Cons l h t) hs labels x out where
  table what labels _ handlers = Map.insert (reflectSymbol (Proxy :: Proxy l))
    (\x payload -> Record.get (Proxy :: Proxy l) handlers x (unerase payload))
    (table what labels (Proxy :: Proxy t) handlers)
