# Design

This document is for the person who maintains purescript-domain next. It explains the model, what the runtime guarantees and how, why the erased representations are sound, what the library leaves out on purpose, and which compiler limits shaped it. The README has the tutorial. `src/Bank` is the usage spec. If a library change makes bank code worse, the change is wrong.

## The model

A user writes each label once, in a spec record type. Every other type takes the spec as its first parameter, and the library derives constructors, dispatch, wiring, routes, codecs, and span names from the spec's rows.

### Spec

A spec is a record type with the fields `id`, `state`, `command`, `event`, `rejection`, and `reply`. `command`, `event`, and `rejection` are variant rows. `class Spec spec id s c e r b | spec -> id s c e r b` in `Domain` projects the record into its parts. Iris rejects a labeled row in an instance head, so the single instance reads the fields from `RowToList spec` through `Field` lookups instead of matching `{ id :: id, ... }`. A missing field fails with `an aggregate spec needs the field reply`.

The fundep is what makes the erased representations sound. Each spec fixes exactly one id type, state type, reply type, and row per role, so any two functions constrained on the same spec agree on all of them.

### Commands, events, and rejections

`Command spec`, `Event spec`, and `Rejection spec` are nominal newtypes over an erased variant of the spec's row. Iris has no type families, so `Event Account` cannot be a synonym for the event row. A newtype indexed by the spec is the next best thing, and it makes role mix-ups short. Passing an event where a command belongs reports `Cannot unify 'Command' with 'Event'` instead of two full rows. Users build them with `cmd @"label"`, `event @"label"`, and `rejection @"label"`, and read them as variants with `toVariant`.

### Aggregate

`aggregate { initial, decide, evolve, reply }` takes handler records keyed by label and walks each record's `RowList` once. It checks every label against the spec and builds an `Object` dispatch table keyed by label. At run time, dispatch reads the variant's tag and looks up the handler.

- `decide` must cover every command and `evolve` every event (`Covers`).
- Each `decide` handler returns a `Decision r e`, whose rows are the rejections and events that handler may produce. `SubRow` proves each narrow row is a sub-row of the spec's row with the same payload types, so widening it is a coercion.
- `reply` computes the reply from the state after the command.

`commit` decides and folds the new events into the state. `replay` is a fold over `evolve`, and `test/Test/Replay.purs` checks the law `replay a s (xs <> ys) == replay a (replay a s xs) ys` as a QuickCheck property.

### View

`View spec k v` is a read model with one row of `v` per key `k`. Its handlers name any subset of the spec's events. Without a `key` field, `k` is the aggregate's id. With `key :: s -> Maybe k`, the runtime calls `key` with the aggregate's state right after each event, and `Nothing` skips the event. The bank's dashboard uses `key: customerOf` to fold every account a customer owns into one row, so `moneyDeposited` does not need to carry the customer.

Two other key sources were rejected:

- Deriving the key from the id, as in `AccountId { customer, number }`, needs a library-defined text codec for every id, freezes ownership, and makes every account id name a customer.
- Learning a routing table from `accountOpened` repeats inside the view a fold of ownership that `evolve` already does, and it is hidden state per view.

The aggregate's state is its own answer to "who owns this account", so the chosen key cannot disagree with the write model, and the view keeps no state beyond its rows. `ViewDefinition` is an instance chain over the definition's fields. It enforces the optional `key`, the key's type, and the rule that a view without a key has `k` equal to the source id, each with its own one-line error.

### Policy

`Policy src tgt` reacts to events of the `src` spec by placing orders on the `tgt` spec. A handler returns `order @"label" targetId payload` values. The runtime derives each order's command id from the event that caused it, so the user never invents one, and a retry derives the same ids again. The runtime guarantees below describe the id format.

### Context

A context is a record of aggregates, views, and policies, such as `bank = { account, customer, summary, statement, dashboard, provisioning }`. Each entry's label names its stream category (`account/a-1`), its span (`command.account`), its runtime handle (`rt.account`), and its HTTP route prefix (`/account`).

`start` makes two type-level passes over the context. `Hosts` builds one host per aggregate entry. `Wire` turns each aggregate into `{ send, history }` and each view into `{ read }`, and subscribes each view and policy to the aggregate whose spec matches (`FindAggregate`, an instance chain with a non-linear head). Typing views and policies by spec, not by label, keeps every module type-checkable without the context. `start` reserves the label `stop` for the shutdown action.

### Derived boundaries

- `Domain.Story` runs given/when/then steps against the pure aggregate, with no runtime.
- `Domain.Codec.labelled` encodes a command, event, or rejection as `{ "type": label, "value": payload }`. `versioned` wraps a codec with a version number and upcasters. `Bank.Codec.accountEvents` upcasts version 1 deposits.
- `Domain.Http.serve` routes `POST /<aggregate>/<id>/<command>` with an `Idempotency-Key` header to `send`, and `GET /<view>/<key>` to `read`. The only hand-written input is one guard per aggregate and view label. A missing guard is a compile error.

## Runtime guarantees

`Domain.Stream.turn` is the pure core. It takes a stream's state and one command, and returns `Committed`, `Duplicate`, or `Refused`. `Domain.Runtime` is a thin host around it. The host reads the clock, runs one turn per stream at a time, keeps the stream that a commit returns, applies views, emits a span, and dispatches policy orders.

### Each command commits once per stream

A stream keeps its receipts in its own state, keyed by command id. Sending a command id that the stream has already committed returns the stored receipt and appends nothing, even if the payload differs. The same id sent to another stream is a different command and commits there. A refusal stores nothing, so the same id is decided again later and may commit. `test/Test/Runtime.purs` covers all three cases, and `test/Test/Stream.purs` covers them against `turn` directly.

### Commands on one stream run in order

A keyed serial executor, private to `Domain.Runtime`, runs one job at a time per stream key, in arrival order. Different streams run concurrently. `test/Test/Concurrency.purs` sends 40 deposits to one stream and checks that they commit in send order with no gaps in the sequence. A second test holds two streams inside `beforeCommand` at the same time, which would deadlock if the runtime serialized across streams.

### Mailboxes are bounded

Each stream's mailbox holds at most `capacity` jobs (64 in `defaults`, and at least 1). A producer that finds the mailbox full waits outside it, so a fast producer cannot grow memory. The bounded-mailbox test runs with capacity 1 and shows that a waiting producer's command never reaches the stream when the producer is killed.

### Views update in the same step as the commit

The host stores the committed stream and applies every view in one synchronous `Effect`. A read right after `send` returns sees the write, and no receipt a caller can hold is ahead of the views. A retried command returns its stored receipt without reaching the views, so a view sees each committed event once.

A keyed row, such as a customer's dashboard, has one writer per source stream, and those streams run concurrently. They never interleave inside one update, because each update is one synchronous effect. The rows of a keyed view therefore update in commit order across streams, not in a per-row sequence. The dashboard sums deltas, so its result is the same in every order. An order-sensitive fold across accounts would see the interleaving.

### Policy orders have derived ids

Policies run after the stream's turn, so an order can target any stream without deadlock. Each order's command id is `policy:stream@seq#index`, for example `provisioning:customer/c-1@1#0`. A retried command reruns the policies over its stored envelopes. The derived ids are the same, so the target returns its stored receipts. A failure between the commit and the dispatch, such as a failing `afterCommand` hook, heals on retry without sending anything twice. Including the policy label and the source stream keeps two policies that react to one event from colliding.

A rejected order does not fail the command that caused it. The rejection shows on the target command's span, and the triggering span counts the orders in `policy.orders`.

### Stop drains

`stop` refuses new external commands, waits for in-flight commands and their policy cascades, and then closes the executor after its queued jobs finish. A send after `stop` fails with `Domain.Runtime: the runtime is stopped`. Orders skip the gate because they run inside the external command that caused them, which `stop` already waits for. The two shutdown tests in `test/Test/Concurrency.purs` cover a queued command and a cascade into another aggregate.

### Failures stay with one command

A failing `beforeCommand` hook aborts its command before the commit and reaches the caller. The stream keeps running, and the same command id can run again. A domain rejection is a value, not a failure.

### Each completed command emits one span

The span is named `command.<label>`. Its attributes are `command.id`, `command.name`, `stream.id`, `outcome` (`committed`, `duplicate`, or `rejected`), `policy.orders`, and either `version` and `events` or `rejection`. The runtime takes every value from labels, so no user code lists them.

## Soundness

`Domain` has four `unsafeCoerce` calls, one in each of four unexported helpers. The rest of `Domain` is type-checked.

| Helper | What it does | Why it is sound |
|---|---|---|
| `erase`, `unerase` | Store a value as `Erased` and read it back. | Each `Erased` is read back at the type it was written with. The next table lists every pair. |
| `inject` | Build a `Variant row` from a label and a payload. | `Field` proves that the label carries that payload type in `row`. |
| `widen` | Turn a handler's narrow row into the spec's row. | `SubRow` proves that each narrow label is in the spec's row with the same payload. |

`Erased` stands in for a type that the spec determines but that a newtype indexed by the spec alone cannot name. Each pair below writes one field and reads it back.

| Written by | Read by | Type at both ends |
|---|---|---|
| `fromVariant` | `toVariant` | `Variant c`, `Variant e`, or `Variant r`, inside `Command`, `Event`, or `Rejection` |
| `aggregate` | `decider` | `Decider spec s b`, inside `Aggregate` |
| `view` | `runView` | the key, `id -> s -> Maybe k`, inside `View` |
| `policy` | `orders` | the handler table, `Object (id -> Erased -> Array (Order tgt))`, inside `Policy` |
| `order` | `orders` | the target id, inside `Order` |
| `untag` | the handler that `table` or `decideTable` stored under that label | the payload of the variant's own label |

The invariant is that the spec fixes every type in the last column. Three facts enforce it, each in one place:

1. `class Spec spec id s c e r b | spec -> id s c e r b` gives each spec exactly one id, state, reply, and row per role. Both ends of the first five pairs have a `Spec` constraint on the same spec, so they agree on the type. The last pair depends on labels instead, as the last item of the next list explains.
2. `Domain` exports no constructor of `Command`, `Event`, `Rejection`, `Aggregate`, `View`, `Policy`, or `Order`, and does not export `Erased` or the helpers. Only `Domain` can write an `Erased`.
3. Each of those newtypes declares its spec parameter `nominal` with `type role`, so `coerce` cannot move a value from one spec to another.

These changes would break it:

- Exporting a constructor. User code could unwrap an `Event Customer` and rewrap its contents as an `Event Account`.
- Deleting a `type role` line. `Safe.Coerce.coerce` could do the same.
- Weakening `Spec` so that a spec no longer fixes its parts. Two ends of a pair could then disagree on `s` or on a row. Deleting the fundep outright fails the library's own build.
- Storing a `table` or `decideTable` handler under a key other than the label that its `Field` check used. A handler would then read a payload of the wrong type. Both builders key each handler by `reflectSymbol` of that label.

`test/compile-errors.sh` asserts the first two from user code. With `Event(..)` exported, the case "forge an event through its constructor" compiles and fails. With `type role Event nominal` deleted, the case "coerce an event across specs" compiles and fails. A third case asserts that `fromVariant` rejects another spec's row. It guards the fundeps on `Spec` and `Labelled`. Deleting either one fails the library's own build first, so no one-line change makes that case fail alone.

Two other unchecked operations exist outside the table. `Domain.Http` reads each guard with `Record.Unsafe.unsafeGet`, by the same label for which `Field` has already proved that the guard exists with the right type. Every instance that ends a chain with `Fail` has a body of `unsafeCrashWith "unreachable: rejected at compile time"`, which no program that compiles can reach.

## Compiler errors

The rule behind legible errors is that no `Prim.Row` constraint ever sees a label that an instance chain has not checked first. Every label check ends in an instance chain in `Domain.Labels` whose last instance fails with a custom message that names the label. Before the rule, a missing handler printed `CannotUnify` and two `Union` failures around the custom message.

`test/compile-errors.sh` applies one mistake at a time to the bank, builds, asserts a fragment of the error, and restores the file. To add a case, add an `expect` line with the file, the text to replace, the replacement, and the expected fragment. A case marked `KNOWN` pins an error that is not yet a one-line custom message. Two remain. A `cmd` with a payload of the wrong type prints a plain unification error at the enclosing signature. A view over an aggregate missing from the context prints the whole spec after its header.

A `decide` handler without a type signature also gives an illegible error, `No instance found for: RowToList @Type ...`, because Iris cannot infer the narrow rows. The library accepts that cost, because the signature is where a handler documents what it may produce.

## What the library does not do

- **Sagas.** A policy maps one event to orders and keeps no state, which is what lets its order ids be derived from the event alone. A process that waits on several events can be an aggregate of its own.
- **A durable store.** No durable backend exists to test the shape against, and an append that returns `Aff` would split today's single synchronous step that stores a commit and applies its views. The open questions below sketch the seam.
- **An HTTP client.** No PureScript caller of the API exists to shape one. A client can encode commands with `Domain.Codec.labelled`, which is the format `serve` reads.
- **`purs` compatibility.** The library targets Iris only. It is built and tested with Iris 0.1.13, and `test/compile-errors.sh` asserts Iris's exact error text, which `purs` renders differently.
- **Views over two aggregates.** A view reads one aggregate's events. A row over two aggregates needs two views.

## Iris limits that shaped the design

- **No type families.** A type indexed by the spec cannot compute the spec's rows, so `Command`, `Event`, `Rejection`, `Aggregate`, `View`, `Policy`, and `Order` are nominal newtypes over `Erased`, and the Soundness section exists.
- **No labeled rows in instance heads.** `instance Spec { id :: id } id` fails with `[InstanceHeadLabeledRow] ... Only the ( | r ) form is allowed`. The `Spec` instance reads fields through `RowToList` instead.
- **What Iris does allow.** Visible type applications give `cmd @"depositMoney"`. Instance chains with `Prim.TypeError.Fail` give the custom errors. Non-linear instance heads give `FindAggregate`, which matches a context entry whose spec equals the view's spec.

## Tradeoffs accepted

- Four `unsafeCoerce` calls in `Domain` buy one-parameter types and short errors. They are sound only while the three facts under Soundness hold.
- Every `decide` handler needs a signature, in exchange for narrow rows that document and guarantee what each handler produces.
- Dispatch goes through a label-keyed `Object` with an unreachable fallback, so that no `Row.Cons` sees an unchecked label.
- `start` reserves the label `stop`, in exchange for a flat `rt.account.send`.
- Every view has a third type parameter (`View Account AccountId Summary`), so that one view type covers keyed and unkeyed views and its signature says what `read` takes.
- A keyed row's update order across streams is commit order, as described under the runtime guarantees.

## Alternatives considered

- **Derive the rows from the handler records, with no spec type.** Event payload types would come from lambda inference, rejection rows from a `Nub` of unions, and every view and policy would need the source rows before the context exists. Errors would read as row unification.
- **Keep raw `Variant` rows with a type synonym per role.** This needs no erasure, but signatures go back to positional rows, and a role mix-up prints two full rows instead of `Command` versus `Event`.
- **Name a view's source by label (`view @"account"`).** This avoids the `FindAggregate` search, but a view could not type-check its handlers until the context exists.

## Open questions

### A store seam

A durable store would sit behind the host as a record of capabilities, with the in-memory map as its first implementation. The sketch has three parts:

1. **Load.** Read a stream's envelopes in order and replay them through `evolve` to get the state and version.
2. **Append with an expected version.** Write a commit's envelopes only if the stream is still at the version the turn started from. A mismatch means another writer got there first.
3. **Rebuild receipts from history.** Every envelope carries its `commandId`. Grouping a stream's envelopes by command id gives each receipt's events and version, and replaying up to the last envelope of each group gives its reply. Refusals are never stored, so nothing else is needed.

The event format would be the versioned `Codec (Event spec)` that `Bank.Codec.accountEvents` already round-trips. The envelope fields (`seq`, `commandId`, `at`) still need a codec.

The cost is the guarantee that a commit and its view updates happen in one synchronous step. An append that returns `Aff` lets a crash or a reader land between the write and the view update. Views would then need their own position in each stream and a way to catch up on restart. Until a real backend needs the store, that cost buys nothing.

### Others

- Should a policy's rejected order surface somewhere other than its span, such as a dead-letter handle on `rt`?
- Can two aggregates in one context share a spec? `FindAggregate` takes the first match.
- `Domain` exports its plumbing classes (`DecideTable`, `Table`, `ViewDefinition`), and `Domain.Runtime` exports `Hosts`, `Wire`, and `FindAggregate`. Iris 0.1.13 still builds and passes the tests with `Hosts` and `Wire` removed from the export list, so these could be hidden.
- `Domain.Id` exports runtime internals (`streamOf`, `advance`, `sequencesAfter`, `derivedCommandId`) to any importer. `Domain` re-exports only the parts callers need.

## Modules

| Module | What it owns |
|---|---|
| `Domain` | Specs, labelled values, aggregates, views, policies, and the four coercion helpers. Pure. |
| `Domain.Labels` | The label checks behind the custom compile errors. |
| `Domain.Id` | Command ids, stream ids, versions, sequences, and derived order ids. |
| `Domain.Stream` | One command's turn on one stream, as a pure function. |
| `Domain.Runtime` | The host around `turn`, with the per-stream serial executor, views, policies, spans, and `stop`. |
| `Domain.Story` | Given/when/then stories against the pure aggregate. |
| `Domain.Codec` | Labelled and versioned JSON codecs. |
| `Domain.Http` | Routes, status codes, and guards over a transport-neutral request. |
| `Bank`, `Bank.*` | The example bank, which is also the usage spec. |
