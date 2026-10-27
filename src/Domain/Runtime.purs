-- | Runs a bounded context. `start` walks the context record and returns
-- | one handle per entry, under the same label:
-- |
-- | ```purescript
-- | rt <- start bank defaults
-- | receipt <- rt.account.send commandId accountId (cmd @"depositMoney" { amount: 100 })
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
  , Hooks
  , Span
  , defaults
  , start
  , AggregateHandle
  , ViewHandle
  , class Hosts
  , makeHosts
  , class Wire
  , wire
  , class FindAggregate
  )
  where

import Prelude

import Data.Array (concatMap, intercalate, length)
import Data.Array as Array
import Data.CatQueue (CatQueue)
import Data.CatQueue as CatQueue
import Data.DateTime.Instant (Instant, diff)
import Data.Either (Either(..), either)
import Data.Foldable (foldl, for_, sum)
import Data.JSDate as JSDate
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Domain
  ( class Spec
  , Aggregate
  , Command
  , Envelope
  , Event
  , Policy
  , Receipt
  , Rejection
  , View
  , orders
  , runView
  , tagOf
  )
import Domain.Id (CommandId, StreamId, commandIdText, streamIdText, streamOf, versionNumber)
import Domain.Stream (Fact, Stream, Turn(..), open, turn)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Aff (Aff, Fiber, attempt, finally, joinFiber, launchAff, throwError)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error)
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

-- | Test seams around each command's turn on its stream. A failing
-- | `beforeCommand` aborts before the commit; a failing `afterCommand`
-- | reports a failure after it.
type Hooks = { beforeCommand :: StreamId -> Aff Unit, afterCommand :: StreamId -> Aff Unit }

type Options = { now :: Aff Instant, trace :: Span -> Aff Unit, capacity :: Int, hooks :: Hooks }

defaults :: Options
defaults =
  { now: do
      date <- liftEffect JSDate.now
      maybe (throwError (error "Domain.Runtime: system clock out of range")) pure
        (JSDate.toInstant date)
  , trace: \_ -> pure unit
  , capacity: 64
  , hooks: { beforeCommand: \_ -> pure unit, afterCommand: \_ -> pure unit }
  }

type AggregateHandle spec id b =
  { send ::
      CommandId -> id -> Command spec -> Aff (Either (Rejection spec) (Receipt (Event spec) b))
  , history :: id -> Aff (Array (Envelope id (Event spec)))
  }

type ViewHandle k v = { read :: k -> Aff (Maybe v) }

-- | Start every aggregate, view, and policy in `ctx`. The result has one
-- | handle per aggregate and view label, plus `stop`, which refuses new
-- | commands, lets in-flight commands and their policy cascades finish,
-- | and drains every stream.
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
type System =
  { options :: Options
  , serial :: Serial
  , closed :: Ref Boolean
  , inFlight :: Ref Int
  , idle :: AVar Unit
  }

newSystem :: Options -> Effect System
newSystem options = do
  serial <- newSerial options.capacity
  closed <- Ref.new false
  inFlight <- Ref.new 0
  idle <- EffectAVar.empty
  pure { options, serial, closed, inFlight, idle }

-- | Admit an external command, or refuse it once the system is stopping.
-- | Policy commands skip this gate: they run inside the external command
-- | that caused them, which `stopSystem` already waits for.
admitted :: forall a. System -> Aff a -> Aff a
admitted system action = do
  admit <- liftEffect do
    closed <- Ref.read system.closed
    unless closed (Ref.modify_ (_ + 1) system.inFlight)
    pure (not closed)
  unless admit (throwError (error "Domain.Runtime: the runtime is stopped"))
  finally (liftEffect leave) action
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
  closeSerial system.serial

-- | One aggregate's streams and subscribers. `handle` is what callers get;
-- | `send` skips the stop gate and is for policies only.
type Host spec id s b =
  { handle :: AggregateHandle spec id b
  , send ::
      CommandId -> id -> Command spec -> Aff (Either (Rejection spec) (Receipt (Event spec) b))
  , onCommit :: (Array (Fact id s spec) -> Effect Unit) -> Effect Unit
  , afterCommit :: (Array (Envelope id (Event spec)) -> Aff Int) -> Effect Unit
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
  views <- Ref.new ([] :: Array (Array (Fact id s spec) -> Effect Unit))
  policies <- Ref.new ([] :: Array (Array (Envelope id (Event spec)) -> Aff Int))
  let
    options = system.options

    -- One synchronous effect, so the commit and every view update land
    -- together.
    decide :: CommandId -> id -> StreamId -> Instant -> Command spec -> Effect (Turn id s spec b)
    decide commandId id stream at sent = do
      current <- fromMaybe (open aggregate stream id) <<< Map.lookup stream <$> Ref.read streams
      let
        decided = turn aggregate at commandId sent current
      case decided of
        Committed committed -> do
          Ref.modify_ (Map.insert stream committed.stream) streams
          Ref.read views >>= \subscribers -> for_ subscribers (_ $ committed.facts)
        _ -> pure unit
      pure decided

    -- Policies run after the stream's turn, so an order can target any
    -- stream without deadlock. A duplicate re-runs them: orders carry
    -- derived command ids, so a retry heals a crash between commit and
    -- dispatch without sending anything twice.
    propagate :: Array (Envelope id (Event spec)) -> Aff Int
    propagate envelopes = do
      subscribers <- liftEffect (Ref.read policies)
      sum <$> for subscribers (_ $ envelopes)

    send ::
      CommandId -> id -> Command spec -> Aff (Either (Rejection spec) (Receipt (Event spec) b))
    send commandId id sent = do
      let
        stream = streamOf name (unwrap id)
      startedAt <- options.now
      decided <- serially system.serial (streamIdText stream) do
        options.hooks.beforeCommand stream
        at <- options.now
        result <- liftEffect (decide commandId id stream at sent)
        options.hooks.afterCommand stream
        pure result
      let
        settled = case decided of
          Committed committed -> Right committed.stored
          Duplicate stored -> Right stored
          Refused refused -> Left refused
      ordered <- either (\_ -> pure 0) (propagate <<< _.envelopes) settled
      finishedAt <- options.now
      options.trace
        { name: "command." <> name
        , startedAt
        , duration: diff finishedAt startedAt
        , attributes: attributes commandId stream sent decided ordered
        }
      pure (map _.receipt settled)

    history :: id -> Aff (Array (Envelope id (Event spec)))
    history id = liftEffect do
      current <- Ref.read streams
      pure
        ( maybe [] (Array.fromFoldable <<< _.history)
            (Map.lookup (streamOf name (unwrap id)) current)
        )

  pure
    { handle: { send: \commandId id sent -> admitted system (send commandId id sent), history }
    , send
    , onCommit: \subscriber -> Ref.modify_ (_ <> [ subscriber ]) views
    , afterCommit: \subscriber -> Ref.modify_ (_ <> [ subscriber ]) policies
    }

attributes ::
  forall spec id s c e r b.
  Spec spec id s c e r b =>
  CommandId ->
  StreamId ->
  Command spec ->
  Turn id s spec b ->
  Int ->
  Map String String
attributes commandId stream sent decided ordered = Map.fromFoldable
  ( [ Tuple "command.id" (commandIdText commandId)
    , Tuple "stream.id" (streamIdText stream)
    , Tuple "command.name" (tagOf sent)
    , Tuple "policy.orders" (show ordered)
    ]
      <> case decided of
        Committed committed -> committedAttributes "committed" committed.stored
        Duplicate stored -> committedAttributes "duplicate" stored
        Refused refused -> [ Tuple "outcome" "rejected", Tuple "rejection" (tagOf refused) ]
  )
  where
  committedAttributes label stored =
    [ Tuple "outcome" label
    , Tuple "version" (show (versionNumber stored.receipt.version))
    , Tuple "events" (intercalate "," (map tagOf stored.receipt.events))
    ]

-- | One row per key. Rows of a keyed view have many writers, one per
-- | source stream, but each update is one synchronous effect inside its
-- | stream's turn, so concurrent streams never interleave inside an update.
-- | A view sees each committed event once: a retried command returns its
-- | stored receipt without reaching the views.
newView ::
  forall spec id s c e r b k v.
  Spec spec id s c e r b => Ord k => View spec k v -> Host spec id s b -> Effect (ViewHandle k v)
newView definition host = do
  rows <- Ref.new (Map.empty :: Map k v)
  host.onCommit \facts -> Ref.modify_ (\current -> foldl apply current facts) rows
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
wirePolicy name definition source target = source.afterCommit \envelopes -> do
  let
    addressed = concatMap (orders name definition) envelopes
  for_ addressed \placed -> target.send placed.commandId placed.to placed.command
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

-- | A keyed serial executor: work for one key runs one job at a time in
-- | arrival order, and different keys run concurrently. Each key has a
-- | bounded mailbox, so a fast producer waits instead of growing memory.
newtype Serial =
  Serial { capacity :: Int, workers :: Ref (Map String Worker), closed :: Ref Boolean }

type Worker = { mailbox :: Mailbox, fiber :: Fiber Unit }

newSerial :: Int -> Effect Serial
newSerial capacity = do
  workers <- Ref.new Map.empty
  closed <- Ref.new false
  pure (Serial { capacity: max 1 capacity, workers, closed })

-- | Run `job` after every job already queued for `key`. Fails once the
-- | executor is closed.
serially :: forall a. Serial -> String -> Aff a -> Aff a
serially serial@(Serial state) key job = do
  worker <- workerFor serial key
  result <- liftEffect EffectAVar.empty
  reserve worker.mailbox
  -- The closed check and the post share one effect: `closeSerial` either
  -- finds this job queued or this caller observes the close.
  accepted <- liftEffect do
    closed <- Ref.read state.closed
    if closed
    then unreserve worker.mailbox $> false
    else post worker.mailbox (attempt job >>= \outcome -> AVar.put outcome result) $> true
  unless accepted (throwError (error ("Domain.Runtime: " <> key <> " is shut down")))
  AVar.take result >>= either throwError pure

-- | Refuse new work, finish queued work, and wait for every worker to exit.
closeSerial :: Serial -> Aff Unit
closeSerial (Serial state) = do
  workers <- liftEffect do
    Ref.write true state.closed
    current <- Ref.read state.workers
    for_ current \worker -> wake worker.mailbox
    pure current
  for_ workers \worker -> joinFiber worker.fiber

workerFor :: Serial -> String -> Aff Worker
workerFor serial@(Serial state) key = do
  closed <- liftEffect (Ref.read state.closed)
  when closed (throwError (error ("Domain.Runtime: " <> key <> " is shut down")))
  liftEffect do
    current <- Ref.read state.workers
    case Map.lookup key current of
      Just worker -> pure worker
      Nothing -> do
        mailbox <- mkMailbox state.capacity
        fiber <- launchAff (loop serial mailbox)
        let
          worker = { mailbox, fiber }
        Ref.modify_ (Map.insert key worker) state.workers
        pure worker

loop :: Serial -> Mailbox -> Aff Unit
loop serial@(Serial state) mailbox = do
  next <- liftEffect (claim mailbox)
  case next of
    Just job -> job *> loop serial mailbox
    Nothing -> do
      closed <- liftEffect (Ref.read state.closed)
      unless closed (awaitWork mailbox *> loop serial mailbox)

-- | A bounded FIFO. Posting and waking share one effect so a queued job is
-- | never left without a wake token; claiming a job frees its slot.
newtype Mailbox =
  Mailbox { pending :: Ref (CatQueue (Aff Unit)), ready :: AVar Unit, slots :: Semaphore }

mkMailbox :: Int -> Effect Mailbox
mkMailbox capacity = do
  pending <- Ref.new CatQueue.empty
  ready <- EffectAVar.empty
  slots <- mkSemaphore capacity
  pure (Mailbox { pending, ready, slots })

post :: Mailbox -> Aff Unit -> Effect Unit
post (Mailbox box) job = do
  Ref.modify_ (\pending -> CatQueue.snoc pending job) box.pending
  void (EffectAVar.tryPut unit box.ready)

claim :: Mailbox -> Effect (Maybe (Aff Unit))
claim (Mailbox box) = do
  pending <- Ref.read box.pending
  case CatQueue.uncons pending of
    Nothing -> pure Nothing
    Just (Tuple job rest) -> do
      Ref.write rest box.pending
      release box.slots
      pure (Just job)

awaitWork :: Mailbox -> Aff Unit
awaitWork (Mailbox box) = AVar.take box.ready

wake :: Mailbox -> Effect Unit
wake (Mailbox box) = void (EffectAVar.tryPut unit box.ready)

reserve :: Mailbox -> Aff Unit
reserve (Mailbox box) = acquire box.slots

unreserve :: Mailbox -> Effect Unit
unreserve (Mailbox box) = release box.slots

-- | A counting semaphore whose counter changes only inside synchronous
-- | effects. `gate` is full exactly when a permit is available.
newtype Semaphore = Semaphore { permits :: Ref Int, gate :: AVar Unit }

mkSemaphore :: Int -> Effect Semaphore
mkSemaphore permits = do
  remaining <- Ref.new permits
  gate <- EffectAVar.new unit
  pure (Semaphore { permits: remaining, gate })

acquire :: Semaphore -> Aff Unit
acquire (Semaphore state) = do
  AVar.take state.gate
  liftEffect do
    remaining <- Ref.modify (_ - 1) state.permits
    when (remaining > 0) (void (EffectAVar.tryPut unit state.gate))

release :: Semaphore -> Effect Unit
release (Semaphore state) = do
  permits <- Ref.modify (_ + 1) state.permits
  when (permits == 1) (void (EffectAVar.tryPut unit state.gate))
