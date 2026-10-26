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

Iris is an implementation of PureScript. It compiles the same language, so a library built on these features works on Iris and on `purs`. The design does not depend on Iris-specific extensions. It does benefit from Iris's faster builds and language server, because row-heavy types are where compile times would otherwise hurt.

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

A view is the same fold with a key:

```purescript
type View k s e out =
  { initial :: s
  , keyOf :: Envelope e -> Maybe k
  , evolve :: s -> Envelope e -> s
  , emit :: Envelope e -> Array out
  }
```

A policy is a Kleisli arrow from events to commands:

```purescript
type Policy e c =
  { on :: Envelope e -> Array (CommandEnvelope c) }
```

A saga is a stateful policy:

```purescript
type Process s e c k =
  { initial :: s
  , keyOf :: Envelope e -> Maybe k
  , step :: s -> Envelope e -> Tuple s (Array (CommandEnvelope c))
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
  , evolve: \state envelope -> match
      { accountOpened: \event -> state { owner = Just event.owner }
      , moneyDeposited: \event -> state { balance = event.balanceAfter }
      , moneyWithdrawn: \event -> state { balance = event.balanceAfter }
      }
      envelope.event
  , emit: \_ -> []
  }
```

`Statement` emits a line per movement. The view has no state of its own. Its emitted events are its history.

```purescript
statement :: View String Unit AccountEvent StatementLine
statement =
  { initial: unit
  , keyOf: \envelope -> Just envelope.stream
  , evolve: \_ _ -> unit
  , emit: \envelope -> match
      { accountOpened: \_ -> []
      , moneyDeposited: \event ->
          [ { kind: "deposit", amount: event.amount, balance: event.balanceAfter } ]
      , moneyWithdrawn: \event ->
          [ { kind: "withdrawal", amount: event.amount, balance: event.balanceAfter } ]
      }
      envelope.event
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

## Status

This repository is a working prototype, not a library.

- `src/Domain.purs` has the core types and `commit`.
- `src/Domain/Story.purs` has the story runner.
- `src/Bank/Account.purs` has the account aggregate with narrowed handlers.
- `src/Bank/Views.purs` has the summary and statement views.
- `src/Bank/Customer.purs` and `src/Bank/Policy.purs` have the customer and provisioning policy.
- `src/Bank/Model.purs` has the in-memory world.
- `test/Test/Main.purs` runs the Alchemy tutorial stories plus the replay law.

`spago test` passes.

The next useful step is not more domain code. It is a runtime with per-key queues and command receipts, because that is where the Alchemy guarantees actually live.
