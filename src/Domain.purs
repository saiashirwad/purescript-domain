-- | The pure kernel. A user declares an aggregate's labels once, in a spec
-- | record type:
-- |
-- | ```purescript
-- | type Account =
-- |   { id :: AccountId, state :: AccountState
-- |   , command :: Variant (depositMoney :: { amount :: Int }, ...)
-- |   , event :: Variant (moneyDeposited :: Movement, ...)
-- |   , rejection :: Variant (notOpen :: {}, ...)
-- |   , reply :: { balance :: Int }
-- |   }
-- | ```
-- |
-- | Every other type takes that spec as its first parameter: `Aggregate
-- | Account`, `Command Account`, `View Account AccountId Summary`, `Policy
-- | Customer Account`. Handlers are records keyed by label. The library walks them
-- | against the spec, so a missing or misspelled handler is one compile
-- | error that names the label.
module Domain
  ( class Spec
  , Command
  , Event
  , Rejection
  , cmd
  , event
  , rejection
  , class Labelled
  , toVariant
  , fromVariant
  , tagOf
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
  , class ViewDefinition
  , viewDefinition
  , Policy
  , policy
  , Order
  , order
  , Addressed
  , orders
  , Envelope
  , Receipt
  , class DecideTable
  , decideTable
  , class Table
  , table
  , module Domain.Id
  )
  where

import Prelude

import Data.Array (mapWithIndex)
import Data.Bifunctor (bimap)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (class VariantShows, Variant)
import Data.Variant.Internal (class VariantTags, VariantRep(..))
import Domain.Id
  ( CommandId
  , Sequence
  , StreamId
  , Version
  , commandIdText
  , parseCommandId
  , parseVersion
  , sequenceNumber
  , streamIdText
  , versionNumber
  , zeroVersion
  )
import Domain.Id (derivedCommandId)
import Domain.Labels (class Covers, class Field, class IsVariant, class SubRow)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Projects an aggregate spec record into its parts by field name. A spec
-- | determines every part, which is what makes the erased representations
-- | below sound.
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

-- | Stands in for a type the spec determines.
data Erased

newtype Command :: Type -> Type
newtype Command spec = Command (Variant ())

type role Command nominal

newtype Event :: Type -> Type
newtype Event spec = Event (Variant ())

type role Event nominal

newtype Rejection :: Type -> Type
newtype Rejection spec = Rejection (Variant ())

type role Rejection nominal

tagged :: forall a. String -> a -> Variant ()
tagged tag value = unsafeCoerce (VariantRep { type: tag, value })

untagged :: forall r. Variant r -> { type :: String, value :: Erased }
untagged v = case (unsafeCoerce v :: VariantRep Erased) of
  VariantRep rep -> rep

-- | `cmd @"depositMoney" { amount: 100 }`. The spec comes from the use
-- | site, so no label needs a hand-written constructor.
cmd ::
  forall @l spec id s c e r b a crl.
  Spec spec id s c e r b =>
  RowToList c crl =>
  Field "the aggregate declares no command " l crl a =>
  IsSymbol l =>
  a ->
  Command spec
cmd = Command <<< tagged (reflectSymbol (Proxy :: Proxy l))

event ::
  forall @l spec id s c e r b a erl.
  Spec spec id s c e r b =>
  RowToList e erl =>
  Field "the aggregate declares no event " l erl a =>
  IsSymbol l =>
  a ->
  Event spec
event = Event <<< tagged (reflectSymbol (Proxy :: Proxy l))

rejection ::
  forall @l spec id s c e r b a rrl.
  Spec spec id s c e r b =>
  RowToList r rrl =>
  Field "the aggregate declares no rejection " l rrl a =>
  IsSymbol l =>
  a ->
  Rejection spec
rejection = Rejection <<< tagged (reflectSymbol (Proxy :: Proxy l))

-- | Commands, events, and rejections are variants of their spec's rows.
class Labelled :: Type -> Row Type -> Constraint
class Labelled a row | a -> row where
  toVariant :: a -> Variant row
  fromVariant :: Variant row -> a

instance Spec spec id s c e r b => Labelled (Command spec) c where
  toVariant (Command v) = unsafeCoerce v
  fromVariant v = Command (unsafeCoerce v)

instance Spec spec id s c e r b => Labelled (Event spec) e where
  toVariant (Event v) = unsafeCoerce v
  fromVariant v = Event (unsafeCoerce v)

instance Spec spec id s c e r b => Labelled (Rejection spec) r where
  toVariant (Rejection v) = unsafeCoerce v
  fromVariant v = Rejection (unsafeCoerce v)

-- | The label, for span attributes and wire tags, so nothing re-lists labels.
tagOf :: forall a row. Labelled a row => a -> String
tagOf = _.type <<< untagged <<< toVariant

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
  forall @l a e erl r.
  RowToList e erl =>
  Field "the handler's event row does not include " l erl a =>
  IsSymbol l =>
  a ->
  Decision r e
emit payload = Right [ unsafeCoerce (tagged (reflectSymbol (Proxy :: Proxy l)) payload) ]

reject ::
  forall @l a r rrl e.
  RowToList r rrl =>
  Field "the handler's rejection row does not include " l rrl a =>
  IsSymbol l =>
  a ->
  Decision r e
reject payload = Left (unsafeCoerce (tagged (reflectSymbol (Proxy :: Proxy l)) payload))

-- | A pure decider in spec terms. `evolve` is total: events are facts.
type Decider spec s b =
  { initial :: s
  , decide :: s -> Command spec -> Either (Rejection spec) (Array (Event spec))
  , evolve :: s -> Event spec -> s
  , reply :: s -> b
  }

newtype Aggregate :: Type -> Type
newtype Aggregate spec = Aggregate (Decider spec Erased Erased)

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
aggregate definition = Aggregate (unsafeCoerce typed)
  where
  decisions = decideTable (Proxy :: Proxy drl) definition.decide
  facts = table (Proxy :: Proxy "evolve handles an undeclared event ") (Proxy :: Proxy erl)
    (Proxy :: Proxy ehl) definition.evolve

  typed :: Decider spec s b
  typed =
    { initial: definition.initial
    , decide: \state (Command v) -> dispatch decisions (\_ -> Right []) state v
    , evolve: \state (Event v) -> dispatch facts identity state v
    , reply: definition.reply
    }

decider :: forall spec id s c e r b. Spec spec id s c e r b => Aggregate spec -> Decider spec s b
decider (Aggregate erased) = unsafeCoerce erased

-- | Decide, then fold the new events into the state.
commit ::
  forall spec id s c e r b.
  Spec spec id s c e r b =>
  Aggregate spec ->
  s ->
  Command spec ->
  Either (Rejection spec) { state :: s, events :: Array (Event spec), reply :: b }
commit aggregate' state command = do
  let
    d = decider aggregate'
  events <- d.decide state command
  let
    next = foldl d.evolve state events
  pure { state: next, events, reply: d.reply next }

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
newtype View spec k v =
  View { initial :: v, step :: v -> Event spec -> v, key :: Erased -> Erased -> Maybe k }

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
  , step: \state (Event v) -> dispatch handlers identity state v
  , key: unsafeCoerce parts.key
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
runView (View fold) = fold { key = unsafeCoerce fold.key }

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
else instance Fail
  ( Beside
      (Text "a view with rows keyed by ")
      ( Beside
          (Quote k)
          (Beside (Text " needs key :: ") (Beside (Quote s) (Beside (Text " -> Maybe ") (Quote k))))
      )
  ) =>
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

-- | A view's key reads the aggregate's state and names a row.
class ViewKey :: Type -> Type -> Type -> Constraint
class ViewKey f s k where
  viewKey :: f -> s -> Maybe k

instance ViewKey (s -> Maybe k) s k where
  viewKey = identity
else instance Fail
  ( Above
      ( Beside
          (Text "a view keyed by ")
          ( Beside
              (Quote k)
              ( Beside
                  (Text " needs key :: ")
                  (Beside (Quote s) (Beside (Text " -> Maybe ") (Quote k)))
              )
          )
      )
      (Beside (Text "but its key is ") (Quote f))
  ) =>
  ViewKey f s k where
  viewKey _ _ = unsafeCrashWith "unreachable: rejected at compile time"

-- | Reacts to events of `src` by ordering commands on `tgt`.
newtype Policy :: Type -> Type -> Type
newtype Policy src tgt = Policy (Object (Erased -> Erased -> Array (Order tgt)))

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
policy handlers = Policy (unsafeCoerce typed)
  where
  typed :: Object (id -> Erased -> Array (Order tgt))
  typed = table (Proxy :: Proxy "a policy reacts to an undeclared event ") (Proxy :: Proxy erl)
    (Proxy :: Proxy hrl) handlers

-- | `order @"openAccount" accountId { owner }`.
order ::
  forall @l tgt id s c e r b a crl.
  Spec tgt id s c e r b =>
  RowToList c crl =>
  Field "the target aggregate declares no command " l crl a =>
  IsSymbol l =>
  id ->
  a ->
  Order tgt
order to payload = Order
  { to: unsafeCoerce to, command: Command (tagged (reflectSymbol (Proxy :: Proxy l)) payload) }

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
orders name (Policy handlers) envelope = mapWithIndex address
  ( dispatch handlers (\_ -> []) (unsafeCoerce envelope.id)
      ( case envelope.event of
          Event v -> v
      )
  )
  where
  address index (Order placed) =
    { commandId: derivedCommandId name envelope.stream envelope.seq index
    , to: unsafeCoerce placed.to
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

dispatch :: forall x out. Object (x -> Erased -> out) -> (x -> out) -> x -> Variant () -> out
dispatch handlers fallback x v =
  let
    { type: tag, value } = untagged v
  in
    maybe (fallback x) (\handler -> handler x value) (Object.lookup tag handlers)

-- | Builds the `decide` dispatch table. Each handler's payload comes from
-- | the spec, and its narrow rows must be sub-rows of the spec's.
class DecideTable :: RowList Type -> Row Type -> Type -> Type -> Constraint
class DecideTable rl hs s spec where
  decideTable ::
    Proxy rl -> Record hs -> Object (s -> Erased -> Either (Rejection spec) (Array (Event spec)))

instance DecideTable Nil hs s spec where
  decideTable _ _ = Object.empty

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
    Object.insert (reflectSymbol (Proxy :: Proxy l)) run (decideTable (Proxy :: Proxy t) handlers)
    where
    run state payload = bimap (Rejection <<< unsafeCoerce) (map (Event <<< unsafeCoerce))
      (Record.get (Proxy :: Proxy l) handlers state (unsafeCoerce payload))

-- | Builds a dispatch table for handlers of shape `x -> payload -> out`,
-- | where each payload type comes from `labels`.
class Table :: Symbol -> RowList Type -> Row Type -> RowList Type -> Type -> Type -> Constraint
class Table what rl hs labels x out where
  table :: Proxy what -> Proxy labels -> Proxy rl -> Record hs -> Object (x -> Erased -> out)

instance Table what Nil hs labels x out where
  table _ _ _ _ = Object.empty

instance ( IsSymbol l
         , Field what l labels a
         , Row.Cons l (x -> a -> out) hsTail hs
         , Table what t hs labels x out
         ) =>
  Table what (Cons l h t) hs labels x out where
  table what labels _ handlers = Object.insert (reflectSymbol (Proxy :: Proxy l))
    (\x payload -> Record.get (Proxy :: Proxy l) handlers x (unsafeCoerce payload))
    (table what labels (Proxy :: Proxy t) handlers)
