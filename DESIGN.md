# purescript-domain

A design for a PureScript library that borrows Alchemy Domain's shape and uses PureScript's type system where it helps.

The repository is a working prototype. `spago test` runs the bank stories from Alchemy's tutorial.

## What Alchemy Domain does

Alchemy Domain models a bounded context with five pieces:

- Commands ask for a change.
- Aggregates decide commands against state and record events.
- Events are facts.
- Views fold events into read models.
- Policies react to events with more commands.

A model runs in memory for tests and on durable storage in production. A story test says "given this history, when this command, then these events." The API is typed from the declarations, so a caller sees the exact rejections and reply.

The part worth keeping is the shape, not the class syntax. Alchemy uses classes and declaration merging to attach schemas to names. PureScript can attach the same information to rows, and the compiler can check more of it.

## Why PureScript can go further

PureScript gives us:

- Row types for open records and open variants.
- Exhaustive matching on variants, checked by the compiler.
- Higher-kinded types and type classes for the algebraic pieces.
- Newtypes for branded ids with no runtime cost.
- A small core and a real module system. No `any`, no structural casts, no decorators.

Iris is a superset of PureScript. The type-level effect system is still PureScript's `Effect` and `Aff`. What Iris changes is code generation. An `Effect` is a zero-argument action, and Iris fuses `pure`, `bind`, `discard`, `map`, and `apply` for `Effect` and `ST`, preserving evaluation timing. That matters here because the domain kernel stays pure while the runtime boundary can use `Effect` or `ST` without paying for a tower of bind calls. If you meant a different effect system, I did not find one in the compiler. The monad is still `Effect` and `Aff`.

The bigger language feature is deriving. Iris derives `Functor`, `Bifunctor`, `Contravariant`, `Profunctor`, `Foldable`, `Bifoldable`, `Traversable`, `Bitraversable`, `Eq1`, `Ord1`, `Generic`, and `Newtype`. PureScript 0.15.16 derives only `Eq`, `Ord`, `Functor`, `Generic`, and `Newtype`. This changes the library design. The boring instances disappear, and the library can ask for a `Traversable` or a `Profunctor` without making the user write it.

Two examples show why this is not decoration.

```purescript
data Changes a = Changes
  { version :: Int
  , values :: Array a
  }

derive instance Functor Changes
derive instance Foldable Changes
derive instance Traversable Changes
```

`traverse` over a `Changes` batch lets a projection validate, encode, or enrich every event in one pass. The version stays fixed while the values move.

```purescript
newtype Codec a b = Codec
  { encode :: a -> String
  , decode :: String -> Either String b
  }

derive instance Profunctor Codec
```

A codec is a profunctor. `dimap` adapts a codec between a domain type and a wire type. That is the useful part, not a slogan.

```purescript
newtype Predicate a = Predicate (a -> Boolean)

derive instance Contravariant Predicate
```

`cmap` adapts a permission check to a richer record. Compose predicates with a `Semigroup` instance for `&&`. That is the security boundary, and it is three lines.

Iris also has virtual modules and compiler intrinsics. The `Iris.StyleX` modules are the working example. A domain library does not need an intrinsic, but the mechanism is there if a `Domain` module ever needs compiler support. The `iris watch query` server is useful for agents, and the module metadata is useful for bundlers.

The practical rule: use Iris deriving for structural boilerplate, but keep the domain API in ordinary PureScript so it runs on both compilers. The exception is a module that explicitly targets Iris, such as an Iris-only `Codec` newtype that derives `Profunctor`.

These examples compile with Iris 0.1.13. The bank prototype also builds and passes its stories under both `purs` 0.15.16 and Iris 0.1.13. Keep the output directories separate. The two compilers generate different JavaScript, and mixing the trees produces confusing runtime errors.

## The core shape

```purescript
type Decider s c e r b =
  { initial :: s
  , decide :: s -> c -> Either r (Array e)
  , evolve :: s -> e -> s
  , reply :: s -> c -> b
  }
```

This is a Mealy machine with input `c` and output `Either r (Array e)`, plus a reply. `decide` may refuse. `evolve` may not, because an event is a fact. Replay is a fold:

```purescript
replay :: forall s e. (s -> e -> s) -> s -> Array e -> s
replay evolve = foldl evolve
```

The law to test is the monoid homomorphism:

```purescript
replay evolve (xs <> ys) s == replay evolve ys (replay evolve xs s)
```

The common shape behind a view, a projection, and a saga is one step:

```purescript
type Step s i o = s -> i -> Tuple s (Array o)
```

A view is a keyed step:

```purescript
type View k s e out =
  { initial :: s
  , keyOf :: Envelope e -> Maybe k
  , step :: Step s (Envelope e) out
  }
```

This is deliberately one function, not an `evolve` and an `emit`. The next state and the emitted values can depend on each other, which is what a real view needs.

A policy is a Kleisli arrow from events to commands:

```purescript
type Policy e c =
  { on :: Envelope e -> Array (CommandEnvelope c) }
```

A saga is a stateful policy:

```purescript
type Process k s e c =
  { initial :: s
  , keyOf :: Envelope e -> Maybe k
  , step :: Step s (Envelope e) (CommandEnvelope c)
  }
```

The whole model is a feedback loop. Aggregates turn commands into events. Policies turn events into commands. The runtime closes the loop, commits atomically, and delivers events to views and policies. In categorical terms, `decide` and `evolve` are a coalgebra, replay is a catamorphism, and the runtime is the interpreter. I would not put those words in the API. I would put the laws in the test suite.

## Design space

I considered three shapes for commands, events, and rejections.

**Closed ADTs.** `data Command = OpenAccount ... | DepositMoney ...`. Simple, exhaustive, easy to read. The cost is that the aggregate's `decide` is one large function, and a command's declaration does not say which events or rejections it can produce. Adding a command touches the ADT and the decision function, and the compiler cannot tell you which events that command is allowed to record.

**Row-typed variants and handler records.** Commands, events, and rejections are `Variant` rows. `decide` is a record of handlers, one per command, and `match` requires every label. Each handler can name a narrow row and `expand` it into the aggregate's full row. This is the shape I chose. It is the closest to Alchemy's declaration while using PureScript's strengths. The cost is some `inj` and `Proxy` noise at the constructors, which smart constructors hide.

**Type-level state machine.** A `Transit`-style type-level spec makes illegal state transitions unrepresentable and can generate a diagram. It is excellent for a protocol with a small, stable state space. It is awkward for event sourcing, because replay must accept every historical event, including events that a later version of the state machine no longer produces. I would offer it as an optional verification layer for new aggregates, not as the core representation.

The prototype uses the second shape. The account's `decide` shows the narrowing.

## The bank, in PureScript

### Account

The state is a closed ADT. The lifecycle is explicit, and an `Unopened` account has no balance field to forget to check.

```purescript
data AccountState
  = Unopened
  | Open { owner :: String, balance :: Int }
```

The event row is the source of truth for the history. The command and rejection rows are the contract.

```purescript
type AccountEventRow =
  ( accountOpened :: { owner :: String }
  , moneyDeposited :: { amount :: Int, balanceAfter :: Int }
  , moneyWithdrawn :: { amount :: Int, balanceAfter :: Int }
  )

type AccountCommandRow =
  ( openAccount :: { owner :: String }
  , depositMoney :: { amount :: Int }
  , withdrawMoney :: { amount :: Int }
  )

type AccountRejectionRow =
  ( alreadyOpen :: {}
  , notOpen :: {}
  , insufficientFunds :: { balance :: Int, requested :: Int }
  )
```

`decide` is a record of handlers. `match` is exhaustive. A new command label without a handler fails to compile.

```purescript
decide :: AccountState -> AccountCommand -> Either AccountRejection (Array AccountEvent)
decide state = match
  { openAccount: \input ->
      openAccountHandler state input
        # lmap expand
        # map (map expand)
  , depositMoney: \input ->
      depositMoneyHandler state input
        # lmap expand
        # map (map expand)
  , withdrawMoney: \input ->
      withdrawMoneyHandler state input
        # lmap expand
        # map (map expand)
  }
```

The handler types are the interesting part. `openAccountHandler` can only return `AlreadyOpen` or `AccountOpened`. `depositMoneyHandler` can only return `NotOpen` or `MoneyDeposited`. `withdrawMoneyHandler` can return `NotOpen`, `InsufficientFunds`, or `MoneyWithdrawn`. `expand` embeds each narrow row into the aggregate's full row.

```purescript
openAccountHandler
  :: AccountState
  -> { owner :: String }
  -> Either
       (Variant (alreadyOpen :: {}))
       (Array (Variant (accountOpened :: { owner :: String })))
```

This is the part that is hard to get in TypeScript without a lot of generic machinery. In PureScript it is a row and a `Union` constraint.

`evolve` is total. An event that cannot follow the current state is a bug in `decide`, so the impossible branch crashes with a message, the same way Alchemy's `mustBeOpen` throws. A stricter version could return `Either`, or use a type-level state machine. I chose the crash because the event is already committed and the correct response is to fail the command, not to return a domain rejection.

```purescript
evolve :: AccountState -> AccountEvent -> AccountState
evolve state = match
  { accountOpened: \event -> Open { owner: event.owner, balance: 0 }
  , moneyDeposited: \event -> case state of
      Open account -> Open (account { balance = event.balanceAfter })
      Unopened -> unsafeCrashWith "MoneyDeposited on an unopened account"
  , moneyWithdrawn: \event -> case state of
      Open account -> Open (account { balance = event.balanceAfter })
      Unopened -> unsafeCrashWith "MoneyWithdrawn on an unopened account"
  }
```

### Stories

The story runner is generic over a decider:

```purescript
data Step e c r b
  = Given e
  | When c
  | Then e
  | Rejected r
  | Replied b
```

The Alchemy stories read almost the same:

```purescript
expectAccountStory "deposits into an open account and replies with the balance"
  [ Given (accountOpened "sam")
  , When (depositMoney 100)
  , Then (moneyDeposited 100 100)
  , Replied { balance: 100 }
  ]
```

```purescript
expectAccountStory "refuses to withdraw more than the balance"
  [ Given (accountOpened "sam")
  , Given (moneyDeposited 100 100)
  , When (withdrawMoney 150)
  , Rejected (insufficientFunds 100 150)
  ]
```

The runner replays `Given` events through `evolve`, commits `When` through `decide` and `evolve`, and checks `Then`, `Rejected`, and `Replied`. It does not need a database or an effect. The same stories run in the test suite.

### Views

`AccountSummary` folds the account events into one summary per account.

```purescript
summary :: View String Summary AccountEvent Unit
summary =
  { initial: { owner: Nothing, balance: 0 }
  , keyOf: \envelope -> Just envelope.stream
  , step: \state envelope ->
      Tuple
        ( match
            { accountOpened: \event -> state { owner = Just event.owner }
            , moneyDeposited: \event -> state { balance = event.balanceAfter }
            , moneyWithdrawn: \event -> state { balance = event.balanceAfter }
            }
            envelope.event
        )
        []
  }
```

`Statement` emits a line per movement. The view has no state of its own. Its emitted events are its history.

```purescript
statement :: View String Unit AccountEvent StatementLine
statement =
  { initial: unit
  , keyOf: \envelope -> Just envelope.stream
  , step: \_ envelope ->
      Tuple unit
        ( match
            { accountOpened: \_ -> []
            , moneyDeposited: \event ->
                [ { kind: "deposit", amount: event.amount, balance: event.balanceAfter } ]
            , moneyWithdrawn: \event ->
                [ { kind: "withdrawal", amount: event.amount, balance: event.balanceAfter } ]
            }
            envelope.event
        )
  }
```

The `keyOf` function is where a view can be re-keyed. The customer dashboard would return `Just customerId` from the summary state, and one account event would update the dashboard for its owner. The type is the same.

### Policies

The provisioning policy reads a customer registration and asks for an account command.

```purescript
mainAccountProvisioning :: Policy CustomerEvent AccountCommand
mainAccountProvisioning =
  { on: \envelope -> match
      { customerRegistered: \event ->
          [ { target: envelope.id <> "-main"
            , command: openAccount event.name
            }
          ]
      }
      envelope.event
  }
```

The target is derived from the event, not from a fresh id. A policy that runs twice sends the same command to the same account. The runtime is responsible for command ids and for treating `AlreadyOpen` as success, the same way Alchemy's `MainAccountProvisioning` catches `AlreadyOpen`.

### The model

The `Bank` record wires the aggregates, views, and policy. The `World` record is an in-memory host: maps of streams, summaries, and statements. `sendAccount` commits a command, appends envelopes, and applies the views. `sendCustomer` commits a customer command, then runs the policy and sends the resulting account commands.

```purescript
type Bank =
  { account
      :: Aggregate AccountId AccountState AccountCommand AccountEvent AccountRejection
           { balance :: Int }
  , customer
      :: Aggregate CustomerId CustomerState CustomerCommand CustomerEvent CustomerRejection
           Unit
  , summary :: View String Summary AccountEvent Unit
  , statement :: View String Unit AccountEvent StatementLine
  , provisioning :: Policy CustomerEvent AccountCommand
  }

sendAccount
  :: AccountId
  -> AccountCommand
  -> World
  -> Either AccountRejection
       { world :: World
       , receipt :: Receipt AccountEvent { balance :: Int }
       }
```

This is deliberately small. It is enough to run the stories and to show where a real host plugs in. A production host would replace the `Map` with a durable store, add per-key queues, and make delivery at least once.

## Category theory, kept practical

The useful abstractions are the ones that give you a law or a type.

| Idea | Code | What it buys |
| --- | --- | --- |
| Mealy machine | `decide` plus `evolve` | A state machine with explicit input and output. `purescript-machines` already has `MealyT` if you want composition. |
| Monoid action | `replay` | State is a fold of events. The split law is a property test. |
| Catamorphism | `replay` over a list | The same shape as `purescript-matryoshka` folds, if you later use a richer event structure. |
| Kleisli arrow | `Policy e c` | A policy is a function into commands in a context. The context is `Array` now, `Aff` later. |
| Fold or optic | `View` read side | A view is a fold over events and a getter over state. `purescript-profunctor-lenses` lets you compose the read side. |
| Free algebra | `purescript-run` | The runtime is an interpreter. In-memory and production hosts are two interpreters for the same model. |
| Monoid or semilattice | view state | If `evolve` is commutative and idempotent, duplicate delivery is harmless. Test it with `purescript-laws` or QuickCheck. |

If a view's state is a join-semilattice, `evolve` can be `join`, and the idempotence law `x <> x == x` is the proof that a repeated delivery does not change the view. That is Alchemy's "make it safe to run twice" expressed as a law instead of a code review note.

The trap is turning every one of these into a type class. A `Decider` does not need a `Monad` instance. A view does not need a profunctor optic until you have several views to compose. Start with records and functions. Add the algebra when it removes a law you would otherwise test by hand.

## Prior art in PureScript

There is very little direct event-sourcing or CQRS work in PureScript. A GitHub search finds a [Connect 4 project](https://github.com/battermann/connect-4) that uses Haskell and PureScript with event sourcing, and little else. The pieces exist, but nobody has assembled them into a Domain-style library. That is the gap.

The pieces worth borrowing:

- [`purescript-variant`](https://github.com/natefaubion/purescript-variant) gives row-typed open sums and exhaustive `match`. This is the backbone of the command, event, and rejection contracts.
- [`purescript-transit`](https://github.com/m-bock/purescript-transit) gives a type-level state machine DSL with compile-time transition checking and diagram generation. Useful as an optional layer for new aggregates.
- [`purescript-machines`](https://github.com/purescript-contrib/purescript-machines) gives Mealy machines. The aggregate and the saga are both Mealy machines.
- [`purescript-run`](https://github.com/natefaubion/purescript-run) gives extensible effects and free DSLs. The host and the test runner are interpreters.
- [`purescript-matryoshka`](https://github.com/purescript-contrib/purescript-matryoshka) gives folds, unfolds, and traversals for fixed-point structures. Useful if events become a recursive structure rather than a list.
- [`purescript-profunctor-lenses`](https://github.com/purescript-contrib/purescript-profunctor-lenses) gives optics. The read side of a view is a `Fold`; a validated view is a `Getter` into a permission check.
- [`purescript-event`](https://github.com/paf31/purescript-event) and [`purescript-hyrule`](https://github.com/mikesol/purescript-hyrule) give event streams and behaviors. The `watch` side of a view is an event stream, not a poll. The original is archived; Hyrule is the maintained fork.
- [`purescript-selda`](https://github.com/Kamirus/purescript-selda) gives typed SQL. A projection to your own database can be typed by the same event row.
- [`purescript-hedwig`](https://github.com/utkarshkukreti/purescript-hedwig) is a TEA-style UI library. The UI side of Alchemy's foldkit integration is a `Model`, `Msg`, `update` loop, which is the same decider shape.
- `purescript-aff`, `purescript-avar`, and `purescript-refs` give the runtime pieces: fibers, queues, and mutable references for a per-key actor.
- `purescript-codec` and `purescript-argonaut-codecs` give serialization. Events need a codec at the boundary; the domain should not know about JSON.
- `purescript-quickcheck` and `purescript-laws` give property tests for the replay law and for view idempotence.

The Haskell and TypeScript prior art is also worth reading. Alchemy and `fmodel-ts` in TypeScript, and `eventful` in Haskell, have already worked through the hard parts of command ids, at-least-once delivery, and projections. The PureScript version should steal those semantics, not reinvent them.

## What I would steal from Haskell

The Haskell ecosystem has already tried most of these shapes. The useful move is to take the idea and drop the ceremony.

| Haskell idea | PureScript counterpart | What to use it for | What not to do |
| --- | --- | --- | --- |
| `free` / `freer-simple` | `purescript-run`, `purescript-free` | A command language with several interpreters: in-memory, durable, test, mock. | Do not make the domain user write `Free` by hand. The runtime can hide it. |
| `profunctors` | `purescript-profunctor` | Codecs and optics. A codec is a profunctor; `dimap` adapts it to the wire. | Do not turn every function into a profunctor. Use it where composition pays. |
| `recursion-schemes` | `purescript-matryoshka` | Replay, folds, and event trees if the history stops being a list. | Do not use a fixed-point type for a flat event log. A list is already a catamorphism. |
| `singletons` | type-level symbols and rows | IDs, stream names, and command labels. | Do not encode the whole schema at the type level. The compiler is not a database. |
| `generics-sop` | `purescript-generics-rep`, Iris deriving | JSON codecs, equality, traversal, and handlers. | Do not derive what a human should read. Keep the domain logic explicit. |
| `servant` | `purescript-routing-duplex`, `httpure` | A typed API surface and path codecs. | Do not generate a client before the server works. |
| `opaleye` / `hasql` | `purescript-selda` | Typed projections and read models. | Do not let the database schema drive the aggregate. |
| `polysemy` / `fused-effects` | `purescript-run` or a capability record | Runtime effects, external systems, tracing. | Do not build an n-squared MTL stack. A record of capabilities is enough. |
| `lens` | `purescript-profunctor-lenses` | Nested state, view validation, read-your-writes. | Do not use optics where a record update is clearer. |
| `streamly` / `conduit` | `purescript-event`, `purescript-hyrule`, `purescript-avar` | `watch`, projections, and per-key queues. | Do not poll. Streams are the natural read side. |

The pattern is consistent. Use the Haskell structure to understand the algebra, then expose the smallest PureScript API that keeps the law. The user should see `decide`, `evolve`, `step`, `send`, `view`, and `watch`. The user should not see `Free`, `Cofree`, `Day`, or `Profunctor` unless they ask for them.

## Scalability notes

The prototype is in memory and single-threaded. Three costs would bite in production.

- `ViewState.emitted <> ...` is an array append per event, so a long statement view is O(n²). Use a sequence with O(1) append, or prepend and reverse at read time. Alchemy keeps an input log and pages it; do the same.
- `Stream.history <> envelopes` is an array append per command, so replaying a long stream is O(n²). A real store appends to a log and loads a snapshot plus a tail.
- Command receipts grow without bound. Keep a deduplication window or a TTL, and store the receipt with the command's timestamp.

The concurrency story is the other half. One queue per stream, a bounded queue for backpressure, and a fiber per shard. `Aff` and `AVar` are enough to start. If the host is Cloudflare Durable Objects, the object is the queue and the single-writer boundary.

## What Alchemy has that this prototype does not

The domain core is the easy part. The guarantees are the work.

- **Atomic commit.** Events, state, reply, and deliveries commit together. The prototype commits in memory. A real host needs a transaction or a single-writer actor.
- **Per-instance ordering.** One command at a time per aggregate. The prototype is single-threaded. A real host needs a per-key queue or a durable object.
- **Exactly-once commands.** A command id maps to a stored receipt. A retry returns the receipt instead of deciding twice. The prototype has no command ids.
- **At-least-once delivery.** Views and policies receive every event at least once, in order per source. The prototype applies views synchronously and never retries.
- **Read-your-writes.** `get` with `atLeast` waits for the view to reach a receipt's version. The prototype is synchronous, so it gets this for free. A distributed host needs the version check.
- **Idempotent policies.** Commands derived from an event id make a second run harmless. The prototype derives the target, but not a command id.
- **Schema evolution.** Events are stored forever. A codec and an upcaster are needed when a field changes. The prototype uses PureScript values, not serialized events.
- **Observability.** Spans, attributes, and per-command traces. The prototype has none.
- **API and auth.** Alchemy's `Api` selects which commands and views each client can reach, and each entry carries middleware and validation. The prototype has no boundary.

These are the pieces a real library would need. They are also where PureScript's type system can help beyond Alchemy. A command id can be a branded type. A receipt can carry a version indexed by the stream. A view's `validate` can return a typed rejection instead of a boolean.

## A suggested library shape

```
Domain
  Decider
  View
  Policy
  Process
  Story
  Envelope
  Receipt
  Commit
  Runtime
    InMemory
    Durable
  Codec
  Api
```

The core stays pure. `Runtime` owns effects, queues, and storage. `Codec` owns the boundary. `Api` owns authorization and transport.

The public API I would aim for:

```purescript
-- pure
decide :: state -> command -> Either rejection (Array event)
evolve :: state -> event -> state

-- test
story decider
  [ Given event
  , When command
  , Then event
  , Replied reply
  ]

-- host
send :: forall id. Aggregate id s c e r b -> id -> c -> Aff (Either r (Receipt e b))
view :: forall k. View k s e out -> k -> Aff s
watch :: forall k. View k s e out -> k -> Event s
```

The host boundary is a record of capabilities, not a monad transformer stack:

```purescript
type Store m =
  { load :: StreamId -> m (Maybe Snapshot)
  , append :: StreamId -> Version -> Array Event -> m Receipt
  , watch :: StreamId -> Event (Envelope Event)
  }

type Runtime m =
  { now :: m Instant
  , store :: Store m
  , publish :: Envelope Event -> m Unit
  , trace :: Span -> m Unit
  }
```

A `ReaderT Runtime Aff` is enough. The record makes dependencies explicit, keeps tests honest, and avoids the n-squared instances of an MTL stack. If the runtime needs a mock clock, a mock store, or a mock payment provider, you replace one field.

The model is a record of components, and the `main` function is the wiring. I would not build a framework that owns a heterogeneous list of aggregates. PureScript can express that list, but the existentials and row plumbing cost more than they save. A record of named components plus a handful of interpreters is easier to read, easier to test, and easier to split into packages. The user's `Bank` record is the model. The library supplies the verbs.

## What I would build first

The prototype already has the kernel and the stories. The next three pieces are the ones that decide whether the library is a toy.

1. **A runtime with command receipts.** `CommandId` is a branded newtype. The store maps `CommandId` to `Receipt`. A retry returns the stored receipt. This is the difference between a demo and a system that can survive a network.
2. **A per-key actor.** Each stream has a queue and a fiber. Commands for one key run in order; different keys run in parallel. The type of the key is the aggregate id, so the routing is not stringly typed.
3. **A codec layer.** Events and commands cross a boundary. `Codec a b` is a profunctor, and `dimap` adapts a wire codec to a domain codec. Iris can derive the instance. A `Generic`-based JSON codec handles the boring cases.

After those, the API, projections, and observability are ordinary work. They matter, but they do not change the shape of the library.

## Status

This repository is a working prototype, not a library.

- `src/Domain.purs` has the core types and `commit`.
- `src/Domain/Story.purs` has the story runner.
- `src/Bank/Account.purs` has the account aggregate with narrowed handlers.
- `src/Bank/Views.purs` has the summary and statement views.
- `src/Bank/Customer.purs` and `src/Bank/Policy.purs` have the customer and provisioning policy.
- `src/Bank/Model.purs` has the in-memory world.
- `test/Test/Main.purs` runs the Alchemy tutorial stories plus the replay law.

`spago test` and `iris test` both pass when run separately. They write different JavaScript, so delete `output` before switching compilers.

The next useful step is not more domain code. It is a runtime with per-key queues and command receipts, because that is where the Alchemy guarantees actually live.
