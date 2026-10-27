-- | Runs a bounded context. `start` walks the context record and returns
-- | one handle per entry, under the same label:
-- |
-- | ```purescript
-- | rt <- start bank defaults
-- | receipt <- rt.account.send commandId accountId (command @"depositMoney" { amount: 100 })
-- | balance <- rt.summary.read accountId
-- | rt.stop
-- | ```
-- |
-- | An aggregate becomes `{ send, history }`, a view becomes `{ read }`, and
-- | a policy is wired to the aggregates its spec types name. Commands for
-- | one stream run one at a time; different streams run concurrently. A view
-- | applies a stream's events inside that stream's turn, so a read right
-- | after a send sees the write.
module Domain.Runtime
  ( Options
  , Span
  , module Exports
  , defaults
  , start
  , AggregateHandle
  , ViewHandle
  )
  where

import Prelude

import Data.Array (concatMap, intercalate, length)
import Data.Array as Array
import Data.DateTime.Instant (Instant, diff)
import Data.Either (Either(..), either)
import Data.Foldable (foldl, for_, sequence_, sum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Domain.Internal.Id
  (CommandId, StreamId, commandIdText, streamIdText, streamOf, versionNumber)
import Domain.Internal.Kernel
  (class Spec, Aggregate, Command, Envelope, Event, Policy, Receipt, View, labelOf, orders, runView)
import Domain.Internal.Stream (Refusal(..)) as Exports
import Domain.Internal.Stream (Fact, Refusal(..), Stored, Stream, Turn(..), open, turn)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Aff (Aff, attempt, bracket, finally, joinFiber, launchAff, throwError)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import Effect.Now as Now
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.TypeError (class Fail, Above, Beside, Quote, QuoteLabel, Text)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

-- | One timed command. Attribute names are fixed by the runtime; their
-- | values come from labels, so no user code lists them.
type Span =
  { name :: String
  , startedAt :: Instant
  , duration :: Milliseconds
  , attributes :: Map String String
  }

-- | `beforeTurn` runs on a command's stream once every earlier command on
-- | that stream has finished, before this one is decided. A failure aborts
-- | the command. Tests use it to hold a stream or to fail a command.
type Options = { now :: Aff Instant, trace :: Span -> Aff Unit, beforeTurn :: StreamId -> Aff Unit }

defaults :: Options
defaults = { now: liftEffect Now.now, trace: \_ -> pure unit, beforeTurn: \_ -> pure unit }

type AggregateHandle spec id b =
  { send :: CommandId -> id -> Command spec -> Aff (Either (Refusal spec) (Receipt (Event spec) b))
  , history :: id -> Aff (Array (Envelope id (Event spec)))
  }

type ViewHandle k v = { read :: k -> Aff (Maybe v) }

-- | Start every aggregate, view, and policy in `ctx`. The result has one
-- | handle per aggregate and view label, plus `stop`, which refuses new
-- | commands and waits for every accepted command and its policy cascade.
start ::
  forall ctx rl hosts rt.
  RowToList ctx rl =>
  Hosts rl ctx hosts =>
  Wire rl rl ctx hosts rt =>
  Row.Lacks "stop" rt =>
  Record ctx ->
  Options ->
  Aff (Record (stop :: Aff Unit | rt))
start ctx options = liftEffect do
  system <- newSystem options
  hosts <- Builder.buildFromScratch <$> makeHosts (Proxy :: Proxy rl) ctx system
  handles <- Builder.buildFromScratch <$> wire (Proxy :: Proxy rl) (Proxy :: Proxy rl) ctx hosts
  pure (Record.insert (Proxy :: Proxy "stop") (stopSystem system) handles)

-- | State every aggregate in one context shares.
type System = { options :: Options, closed :: Ref Boolean, inFlight :: Ref Int, idle :: AVar Unit }

newSystem :: Options -> Effect System
newSystem options = do
  closed <- Ref.new false
  inFlight <- Ref.new 0
  idle <- EffectAVar.empty
  pure { options, closed, inFlight, idle }

-- | Run an external command on a fiber the system owns, and wait for it.
-- | The caller only joins, so killing the caller ends the wait, not the
-- | work: an accepted command still commits, dispatches its orders, and
-- | emits its span, and `stop` waits for all of it. The stop check, the
-- | in-flight count, and the launch share one synchronous effect, so either
-- | `stop` counts this command or this caller sees the runtime stopped.
-- | Policy orders skip this: they run inside the work of the command that
-- | caused them. `Nothing` means the runtime is stopped.
submit :: forall a. System -> Aff a -> Aff (Maybe a)
submit system work = do
  launched <- liftEffect do
    closed <- Ref.read system.closed
    if closed
    then pure Nothing
    else do
      Ref.modify_ (_ + 1) system.inFlight
      Just <$> launchAff (finally (liftEffect leave) (attempt work))
  for launched \fiber -> joinFiber fiber >>= either throwError pure
  where
  leave = do
    remaining <- Ref.modify (_ - 1) system.inFlight
    closed <- Ref.read system.closed
    when (closed && remaining == 0) (void (EffectAVar.tryPut unit system.idle))

stopSystem :: System -> Aff Unit
stopSystem system = do
  busy <- liftEffect do
    Ref.write true system.closed
    (_ > 0) <$> Ref.read system.inFlight
  when busy (AVar.read system.idle)

-- | One aggregate's streams and subscribers. `handle` is what callers get.
-- | `sendOrder` skips `submit`, because an order runs inside the work of the
-- | command that placed it.
type Host spec id s b =
  { handle :: AggregateHandle spec id b
  , sendOrder ::
      CommandId -> id -> Command spec -> Aff (Either (Refusal spec) (Receipt (Event spec) b))
  , subscribeView :: (Array (Fact id s spec) -> Effect (Effect Unit)) -> Effect Unit
  , subscribePolicy :: (Array (Envelope id (Event spec)) -> Aff Int) -> Effect Unit
  }

newHost ::
  forall spec id s c e r b.
  Spec spec id s c e r b =>
  Newtype id String =>
  System ->
  String ->
  Aggregate spec ->
  Effect (Host spec id s b)
newHost system name aggregate = do
  streams <- Ref.new (Map.empty :: Map StreamId (Stream id s spec b))
  locks <- Ref.new (Map.empty :: Map StreamId (AVar Unit))
  views <- Ref.new ([] :: Array (Array (Fact id s spec) -> Effect (Effect Unit)))
  policies <- Ref.new ([] :: Array (Array (Envelope id (Event spec)) -> Aff Int))
  let
    options = system.options

    -- One synchronous effect. Every view computes its next rows before
    -- anything is stored, so the stream and its views change together, or
    -- not at all when a view throws.
    decide :: CommandId -> id -> StreamId -> Instant -> Command spec -> Effect (Turn id s spec b)
    decide commandId id stream at sent = do
      current <- fromMaybe (open aggregate stream id) <<< Map.lookup stream <$> Ref.read streams
      let
        decided = turn aggregate at commandId sent current
      case decided of
        Committed committed -> do
          writes <- Ref.read views >>= traverse (_ $ committed.facts)
          Ref.modify_ (Map.insert stream committed.stream) streams
          sequence_ writes
        _ -> pure unit
      pure decided

    -- One command per stream at a time, in arrival order: an AVar serves
    -- waiting takes first come, first served. The lock is taken inside the
    -- work `submit` owns, which nothing kills, and `bracket` returns it
    -- however the turn ends.
    holding :: forall a. StreamId -> Aff a -> Aff a
    holding stream job = do
      lock <- liftEffect do
        current <- Ref.read locks
        case Map.lookup stream current of
          Just lock -> pure lock
          Nothing -> do
            lock <- EffectAVar.new unit
            Ref.write (Map.insert stream lock current) locks
            pure lock
      bracket (AVar.take lock) (\_ -> AVar.put unit lock) (\_ -> job)

    -- Policies run after the stream's turn, so an order can target any
    -- stream without deadlock. A duplicate re-runs them: orders carry
    -- derived command ids, so a retry re-sends an order that a failure
    -- after the commit lost, and the target returns its stored receipt for
    -- an order that already landed.
    propagate :: Array (Envelope id (Event spec)) -> Aff Int
    propagate envelopes = do
      subscribers <- liftEffect (Ref.read policies)
      sum <$> for subscribers (_ $ envelopes)

    -- Every command emits one span, a failed one included, so the cause of
    -- a failure that `serve` hides from its client reaches the operator.
    traced :: CommandId -> id -> Command spec -> Aff (Outcome id s spec b) -> Aff (Answer spec b)
    traced commandId id sent work = do
      startedAt <- options.now
      result <- attempt work
      finishedAt <- options.now
      options.trace
        { name: "command." <> name
        , startedAt
        , duration: diff finishedAt startedAt
        , attributes: attributes commandId (streamOf name (unwrap id)) sent result
        }
      case result of
        Left failure -> throwError failure
        Right { decided } -> pure case decided of
          Committed committed -> Right committed.stored.receipt
          Duplicate stored -> Right stored.receipt
          Refused refusal -> Left refusal

    send :: CommandId -> id -> Command spec -> Aff (Answer spec b)
    send commandId id sent = traced commandId id sent do
      let
        stream = streamOf name (unwrap id)
      decided <- holding stream do
        options.beforeTurn stream
        at <- options.now
        liftEffect (decide commandId id stream at sent)
      ordered <- case decided of
        Committed committed -> propagate committed.stored.envelopes
        Duplicate stored -> propagate stored.envelopes
        Refused _ -> pure 0
      pure { decided, ordered }

    admit :: CommandId -> id -> Command spec -> Aff (Answer spec b)
    admit commandId id sent = submit system (send commandId id sent) >>= case _ of
      Just answer -> pure answer
      Nothing ->
        traced commandId id sent (throwError (error "Domain.Runtime: the runtime is stopped"))

    history :: id -> Aff (Array (Envelope id (Event spec)))
    history id = liftEffect do
      current <- Ref.read streams
      pure
        ( maybe [] (Array.fromFoldable <<< _.history)
            (Map.lookup (streamOf name (unwrap id)) current)
        )

  pure
    { handle: { send: admit, history }
    , sendOrder: send
    , subscribeView: \subscriber -> Ref.modify_ (_ <> [ subscriber ]) views
    , subscribePolicy: \subscriber -> Ref.modify_ (_ <> [ subscriber ]) policies
    }

type Answer spec b = Either (Refusal spec) (Receipt (Event spec) b)

-- | A finished turn and the number of orders its policies placed.
type Outcome id s spec b = { decided :: Turn id s spec b, ordered :: Int }

-- | A committed or duplicate command's name and events come from its
-- | stored receipt, so a span never pairs one command's name with
-- | another's events.
attributes ::
  forall spec id s c e r b.
  Spec spec id s c e r b =>
  CommandId ->
  StreamId ->
  Command spec ->
  Either Error (Outcome id s spec b) ->
  Map String String
attributes commandId stream sent result = Map.fromFoldable
  ( [ Tuple "command.id" (commandIdText commandId), Tuple "stream.id" (streamIdText stream) ]
      <> case result of
        Left failure ->
          [ Tuple "command.name" (labelOf sent)
          , Tuple "outcome" "failed"
          , Tuple "error" (message failure)
          ]
        Right { decided, ordered } -> [ Tuple "policy.orders" (show ordered) ] <> case decided of
          Committed committed -> storedAttributes "committed" committed.stored
          Duplicate stored -> storedAttributes "duplicate" stored
          Refused (Rejected rejected) ->
            [ Tuple "command.name" (labelOf sent)
            , Tuple "outcome" "rejected"
            , Tuple "rejection" (labelOf rejected)
            ]
          Refused (Reused reused) ->
            [ Tuple "command.name" (labelOf sent)
            , Tuple "outcome" "reused"
            , Tuple "stored.command" reused.command
            ]
  )
  where
  storedAttributes :: String -> Stored id spec b -> Array (Tuple String String)
  storedAttributes outcome stored =
    [ Tuple "command.name" stored.command
    , Tuple "outcome" outcome
    , Tuple "version" (show (versionNumber stored.receipt.version))
    , Tuple "events" (intercalate "," (map labelOf stored.receipt.events))
    ]

-- | One row per key. Rows of a keyed view have many writers, one per
-- | source stream, but each update is computed and stored inside one
-- | synchronous effect of its stream's turn, so concurrent streams never
-- | interleave inside an update. A view sees each committed event once: a
-- | retried command returns its stored receipt without reaching the views.
newView ::
  forall spec id s c e r b k v.
  Spec spec id s c e r b => Ord k => View spec k v -> Host spec id s b -> Effect (ViewHandle k v)
newView definition host = do
  rows <- Ref.new (Map.empty :: Map k v)
  host.subscribeView \facts -> do
    current <- Ref.read rows
    let
      next = foldl apply current facts
    pure (Ref.write next rows)
  pure { read: \key -> liftEffect (Map.lookup key <$> Ref.read rows) }
  where
  fold = runView definition

  apply current { envelope, state } = case fold.key envelope.id state of
    Nothing -> current
    Just key ->
      Map.insert key (fold.step (fromMaybe fold.initial (Map.lookup key current)) envelope.event)
        current

wirePolicy ::
  forall src sid ss sc se sr sb tgt tid ts tc te tr tb.
  Spec src sid ss sc se sr sb =>
  Spec tgt tid ts tc te tr tb =>
  String ->
  Policy src tgt ->
  Host src sid ss sb ->
  Host tgt tid ts tb ->
  Effect Unit
wirePolicy name definition source target = source.subscribePolicy \envelopes -> do
  let
    addressed = concatMap (orders name definition) envelopes
  for_ addressed \placed -> target.sendOrder placed.commandId placed.to placed.command
  pure (length addressed)

-- | Pass one: a host per aggregate entry, keyed by the entry's label.
class Hosts :: RowList Type -> Row Type -> Row Type -> Constraint
class Hosts rl ctx hosts | rl -> hosts where
  makeHosts :: Proxy rl -> Record ctx -> System -> Effect (Builder {} (Record hosts))

instance Hosts Nil ctx () where
  makeHosts _ _ _ = pure identity

instance ( IsSymbol l
         , Spec spec id s c e r b
         , Newtype id String
         , Row.Cons l (Aggregate spec) ctxTail ctx
         , Hosts t ctx tailHosts
         , Row.Cons l (Host spec id s b) tailHosts hosts
         , Row.Lacks l tailHosts
         ) =>
  Hosts (Cons l (Aggregate spec) t) ctx hosts where
  makeHosts _ ctx system = do
    host <- newHost system (reflectSymbol (Proxy :: Proxy l)) (Record.get (Proxy :: Proxy l) ctx)
    rest <- makeHosts (Proxy :: Proxy t) ctx system
    pure (Builder.insert (Proxy :: Proxy l) host <<< rest)
else instance Hosts t ctx hosts => Hosts (Cons l entry t) ctx hosts where
  makeHosts _ = makeHosts (Proxy :: Proxy t)

-- | Pass two: a handle per aggregate and view, and the policy wiring.
-- | `all` is the whole context, where views and policies find their
-- | aggregates by spec.
class Wire :: RowList Type -> RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class Wire rl all ctx hosts rt | rl -> rt where
  wire :: Proxy rl -> Proxy all -> Record ctx -> Record hosts -> Effect (Builder {} (Record rt))

instance Wire Nil all ctx hosts () where
  wire _ _ _ _ = pure identity

instance ( IsSymbol l
         , Spec spec id s c e r b
         , Row.Cons l (Host spec id s b) hostsTail hosts
         , Wire t all ctx hosts tailRt
         , Row.Cons l (AggregateHandle spec id b) tailRt rt
         , Row.Lacks l tailRt
         ) =>
  Wire (Cons l (Aggregate spec) t) all ctx hosts rt where
  wire _ all ctx hosts = do
    let
      host = Record.get (Proxy :: Proxy l) hosts
    rest <- wire (Proxy :: Proxy t) all ctx hosts
    pure (Builder.insert (Proxy :: Proxy l) host.handle <<< rest)
else instance ( IsSymbol l
              , Spec spec id s c e r b
              , Ord k
              , FindAggregate spec all source
              , IsSymbol source
              , Row.Cons source (Host spec id s b) hostsTail hosts
              , Row.Cons l (View spec k v) ctxTail ctx
              , Wire t all ctx hosts tailRt
              , Row.Cons l (ViewHandle k v) tailRt rt
              , Row.Lacks l tailRt
              ) =>
  Wire (Cons l (View spec k v) t) all ctx hosts rt where
  wire _ all ctx hosts = do
    handle <- newView (Record.get (Proxy :: Proxy l) ctx) (Record.get (Proxy :: Proxy source) hosts)
    rest <- wire (Proxy :: Proxy t) all ctx hosts
    pure (Builder.insert (Proxy :: Proxy l) handle <<< rest)
else instance ( IsSymbol l
              , Spec src sid ss sc se sr sb
              , Spec tgt tid ts tc te tr tb
              , FindAggregate src all source
              , FindAggregate tgt all target
              , IsSymbol source
              , IsSymbol target
              , Row.Cons source (Host src sid ss sb) sourceTail hosts
              , Row.Cons target (Host tgt tid ts tb) targetTail hosts
              , Row.Cons l (Policy src tgt) ctxTail ctx
              , Wire t all ctx hosts rt
              ) =>
  Wire (Cons l (Policy src tgt) t) all ctx hosts rt where
  wire _ all ctx hosts = do
    wirePolicy (reflectSymbol (Proxy :: Proxy l)) (Record.get (Proxy :: Proxy l) ctx)
      (Record.get (Proxy :: Proxy source) hosts) (Record.get (Proxy :: Proxy target) hosts)
    wire (Proxy :: Proxy t) all ctx hosts
else instance Fail
  ( Beside
      (Text "a context entry must be an Aggregate, a View, or a Policy, but ")
      (Beside (QuoteLabel l) (Beside (Text " is ") (Quote entry)))
  ) =>
  Wire (Cons l entry t) all ctx hosts rt where
  wire _ _ _ _ = unsafeCrashWith "unreachable: rejected at compile time"

-- | The label of the aggregate in `rl` whose spec is `spec`.
class FindAggregate :: Type -> RowList Type -> Symbol -> Constraint
class FindAggregate spec rl l | spec rl -> l

instance FindAggregate spec (Cons l (Aggregate spec) t) l
else instance FindAggregate spec t l => FindAggregate spec (Cons m entry t) l
else instance Fail
  ( Above (Text "a view or policy reads an aggregate this context does not contain:") (Quote spec)
  ) =>
  FindAggregate spec Nil l
