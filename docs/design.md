# Design

This document is for the person who maintains purescript-domain next. It explains the model, what the runtime guarantees and how, what is left unchecked, which compiler errors users see, what the library leaves out on purpose, and which designs came before. The README has the tutorial. `src/Bank` is the usage spec. If a library change makes bank code worse, the change is wrong.

`test/check.sh` is the one gate for every change. It runs `iris test` and requires the line `All tests passed.`, then runs `test/compile-errors.sh` and `test/check-snippets.py`.

## The model

A user writes commands, events, and rejections as ordinary sum types, and an aggregate as a decider over them. The library reads labels from `Generic` and finds how the pieces of a context connect from their types.

### Labels come from constructor names

Commands, events, and rejections derive `Generic`. `Domain.Internal.Labels` turns a constructor's name into its label by lowering the first letter, so `MoneyDeposited` is `moneyDeposited`. The label names the HTTP route segment, the `command.name` and `rejection` span attributes, the name a receipt stores, and the `"type"` field in JSON. The JSON format is `{ "type": label, "value": payload }`, where the payload is the constructor's one argument through argonaut, or `{}` for a constructor with none. This is the format the previous design wrote, so stored events and clients did not change.

`LabelOf`, `EncodeCases`, and `DecodeCases` are classes over the `Generic` representation, with instances for `Sum`, `Constructor`, and `NoConstructors`. The `NoConstructors` instances let an aggregate that never rejects use an empty data type, such as `TallyRejection` in `test/Test/Tally.purs`. A constructor with two or more arguments has a `Product` representation, and the `Product` instances end in `Fail` with the message `a labelled constructor takes at most one argument; put its fields in a record`.

### Aggregate

`Aggregate id c e r s b` is a newtype over a decider record: `initial :: s`, `decide :: c -> s -> Either r (Array e)`, `evolve :: s -> e -> s`, and `reply :: s -> b`. `id` appears only in the type. It names the aggregate's streams, and the runtime requires `Newtype id String`.

`decide` and `evolve` are ordinary functions, usually a `case` with no wildcard over the command or event. The compiler's exhaustiveness check then does the job that `Covers` and `DecideTable` did in the previous design: a new command or event fails the build at every function that has not handled it.

`commit` in `Domain.Internal.Kernel` decides and folds the new events into the state, keeping the state after each event. `replay` is a fold over `evolve`. `test/Test/Replay.purs` checks two QuickCheck properties: the fold law `replay a s (xs <> ys) == replay a (replay a s xs) ys`, and that any run of commands leaves the bank's balance equal to the sum of its movements and never negative.

### Views are folds

`Domain.Fold` defines `Fold i o`, a left fold as a value: its current result and the fold it becomes after one more input. It is a `Functor`, an `Applicative`, and a `Profunctor`. `fold` builds one from an initial value and a step, `foldMap` from a function into a monoid, `premap` adapts its input, and `run` runs it over a `Foldable`. `test/Test/Fold.purs` covers the instances.

A view's input is a `Change id s e`, the record `{ id, state, event }`: a committed event, the id of its stream, and the stream's state right after it. `View i k v` pairs a fold with a key function `i -> Maybe k`. The runtime keeps one fold per key and steps it with each change routed to that key. `Nothing` skips the change. `byId` keys by the change's `id`. `keyed` takes any key function.

The bank's dashboard uses `keyed (customerOf <<< _.state)` to fold every account a customer owns into one row, so `MoneyDeposited` does not need to carry the customer. Two other key sources were rejected:

- Deriving the key from the id, as in `AccountId { customer, number }`, needs a library-defined text codec for every id, freezes ownership, and makes every account id name a customer.
- Learning a routing table from `AccountOpened` repeats inside the view a fold of ownership that `evolve` already does, and it is hidden state per view.

The aggregate's state is its own answer to "who owns this account", so the key cannot disagree with the write model, and the view keeps no state beyond its rows.

### Policy

`Policy i id c` wraps a function from a change to an array of orders, where an order is `{ to :: id, command :: c }`. The user never gives an order a command id. `orders` in `Domain.Internal.Stream` derives one for each order from the event that caused it, and the runtime's `wirePolicy` calls it. A retry derives the same ids again. The runtime guarantees below describe the id format.

### Context

A context is a record of aggregates, views, and policies, such as `bank = { account, customer, summary, statement, dashboard, provisioning }`. Each entry's label names its stream category (`account/a-1`), its span (`command.account`), its runtime handle (`rt.account`), and its HTTP route prefix (`/account`).

`start` in `Domain.Runtime` makes two type-level passes over the context. `Hosts` builds one host per aggregate entry. `Wire` turns each aggregate into `{ send, history }` and each view into `{ read }`, and subscribes each view and policy to its aggregates. The wiring finds them by type, not by label:

- `ChangeOf` reads the event type from the `event` field of a view's or policy's input record.
- `FindSource` finds the aggregate in the context whose event type is that type.
- `FindTarget` finds the aggregate whose command type is the policy's command type.
- `TypeEquals (Change id s e) i` then proves that the input is exactly that aggregate's `Change`.

Finding aggregates by type keeps every module type-checkable without the context. `start` reserves the label `stop` for the shutdown action.

### Derived boundaries

- `Domain.Story` runs given/when/then steps against the pure aggregate, with no runtime. The `emits` steps after a command must list its events exactly, in order, so a story cannot pass while the command emits an event it does not mention.
- `Domain.Codec.labelled` is the labelled JSON codec for any type with a `Generic` instance whose constructors take at most one argument. `versioned` wraps a codec with a version number and upcasters, and applies them one version at a time, so a version older than every upcaster fails at its first lookup. `Bank.Codec.accountEvents` upcasts version 1 deposits.
- `Domain.Http.serve` routes `POST /<aggregate>/<id>/<command>` with an `Idempotency-Key` header to `send`, and `GET /<view>/<key>` to `read`. The only hand-written input is the guards. An aggregate's guard is `principal -> id -> command -> Boolean`, and the bank writes it as a `case` over the command with no wildcard, so a new command needs a decision about who may send it. A view's guard is `principal -> key -> Boolean`. `serve` decodes the command from the route's command segment and the body before it calls the guard, so a body that does not decode is a 400, even for a principal the guard would refuse. A missing guard is a compile error that names the label, and so is a guard whose label names no aggregate or view of the context. Any exception inside the runtime becomes a 500 with a fixed body, so an internal message never reaches the client. The runtime has already reported the exception: every command emits a span, and a failed command's span carries the message.

## Runtime guarantees

`Domain.Internal.Stream.turn` is the pure core. It takes a stream's state and one command, and returns `Committed`, `Duplicate`, or `Refused` with the `Refusal` that `send` returns. `Domain.Runtime` is a thin host around it. The host reads the clock, runs one turn per stream at a time, keeps the stream that a commit returns, applies views, dispatches policy orders, and emits a span.

### Each command commits once per stream

A stream keeps its receipts in its own state, keyed by command id, and each receipt records the label of the command that made it. Sending a command id that the stream has already committed for the same label returns the stored receipt and appends nothing, even if the payload differs. Sending it with another label is an error the caller sees: `send` returns `Left (Reused { commandId, command })`, where `command` names the stored command, and HTTP answers 422. The same id sent to another stream is a different command and commits there. A refusal stores nothing, so the same id is decided again later and may commit. `test/Test/Runtime.purs` covers each case. `test/Test/Stream.purs` pins the format of a derived order id through `orders`, because a stream stores receipts under it.

A `CommandId` is either a client's key, from `parseCommandId`, or an id the runtime derives for a policy's order. The two kinds never compare equal, whatever their text, so a client that sends the key `provisioning:customer/c-1@1#0` cannot claim that order's id and suppress it. `clientsCannotClaimOrderIds` in `test/Test/Tally.purs` covers this with a tally, whose bumps are not idempotent by themselves.

### Commands on one stream run in order

Each stream has a lock, an `AVar Unit` that a turn takes before it starts and puts back when it ends. An `AVar` serves waiting takes first come, first served, so commands on one stream are decided in the order they reach the lock. With a synchronous `now`, as in `defaults`, that is the order of the `send` calls. Different streams have different locks and run concurrently. The lock covers `beforeTurn` and the turn itself. It does not cover the policy dispatch, so an order can target any stream, the source stream included, without deadlock.

`sameStreamOrder` in `test/Test/Concurrency.purs` sends 40 deposits to one held stream and checks that they commit in send order with no gaps in the sequence. `busyStreamWaits` checks that a command sent to a held stream does not start its turn until the stream is free. `differentStreamsOverlap` holds two streams inside `beforeTurn` at the same time, which would deadlock if the runtime serialized across streams.

### An accepted command finishes without its caller

`send` does not run a command on the caller's fiber. It admits the command and launches it on a fiber that the runtime owns, then joins that fiber. The launched fiber runs the whole unit: it takes the stream's lock, runs the turn, stores the stream and its views, dispatches the policies' orders, and emits the span. It lowers the in-flight count in its `finally`. Killing the caller cancels only the join, never the work. Nothing else holds a reference to the launched fiber, so nothing kills it. Its result travels as an `Either`, so a failure after its caller left is never an unhandled error.

Every point at which a caller can be killed leads to the same outcome:

- **Before `send` admits the command.** The stop check, the in-flight increment, and the launch run in one synchronous effect, and a kill can land only between effects. Either nothing was admitted, or the command was admitted and its fiber already owns the in-flight count.
- **While the command waits for its stream's lock.** The take belongs to the launched fiber, which keeps waiting, so the command still commits in its place in line. `bracket` returns the lock however the turn ends.
- **While the turn runs, or after the commit while orders are dispatched.** The launched fiber carries on through the policies and the span.

`stop` sets the closed flag and reads the in-flight count in one synchronous effect. If the count is above zero, `stop` waits until the last launched fiber's `finally` brings it to zero. That `finally` runs after the policy dispatch and the span, so `stop` returns only once every admitted command, its orders, and the orders those orders place have committed or failed. A send after `stop` fails with `Domain.Runtime: the runtime is stopped`. Orders skip admission, because they run inside the fiber of the external command that caused them, which `stop` already waits for. A `stop` after `stop` has returned returns at once.

`abandonedCommands` in `test/Test/Concurrency.purs` kills a caller at each of the three points above, then calls `stop`. It checks that `stop` is still waiting while the stream is held, and that the command and its order commit. With the work moved back onto the caller's fiber, all three cases fail.

### Views update in the same step as the commit

The host stores the committed stream and applies every view in one synchronous `Effect`. Each view first computes its next folds from its current folds and the committed changes. The host stores the stream and the new folds only after every view has computed, so if a view throws, nothing is stored, and the caller sees the error. A read right after `send` returns sees the write, and no receipt a caller can hold is ahead of the views. A retried command returns its stored receipt without reaching the views, so a view sees each committed event once. `failingViewCommitsNothing` in `test/Test/Tally.purs` checks that a view that throws leaves the stream and the other views unchanged.

The runtime handles a label only as a string: in a stored receipt, in a span, and as a key of the HTTP route `Map`. A constructor named `ValueOf` or `Constructor` labels as `valueOf` or `constructor`, which are names on every JavaScript object, and behaves like any other. `prototypeLabels` in `test/Test/Tally.purs` covers the runtime and the codec, and `test/Test/Http.purs` covers a route named `constructor`.

A keyed row, such as a customer's dashboard, has one writer per source stream, and those streams run concurrently. They never interleave inside one update, because each update is one synchronous effect. The rows of a keyed view therefore update in commit order across streams, not in a per-row sequence. The dashboard sums deltas, so its result is the same in every order. An order-sensitive fold across accounts would see the interleaving.

### Policy orders have derived ids

Each order's command id is `policy:stream@seq#index`, for example `provisioning:customer/c-1@1#0`. Including the policy label and the source stream keeps two policies that react to one event from colliding. A retried command reruns the policies over its stored facts. The derived ids are the same, so the target returns its stored receipt for an order that already committed, and sends nothing twice. `derivedOrderIds` in `test/Test/Tally.purs` checks this with a tally, which would count a re-sent order twice.

The same rerun heals a lost order. If the dispatch fails after the commit, for example because the target's `beforeTurn` fails, the caller sees the failure, and the order is missing. Retrying the triggering command with its id sends the order again. `retryHealsALostOrder` covers this.

A rejected order does not fail the command that caused it. The rejection shows on the target command's span, and the triggering span counts the orders in `policy.orders`.

### Failures stay with one command

A failing `beforeTurn` aborts its command before the commit and reaches the caller. The stream keeps running, and the same command id can run again. A domain rejection is a value, not a failure.

### Each command emits one span

The span is named `command.<aggregate>`. Its attributes are `command.id`, `command.name`, `stream.id`, and `outcome`, which is `committed`, `duplicate`, `reused`, `rejected`, or `failed`. Every outcome but `failed` adds `policy.orders`. A committed or duplicate span adds `version` and `events`, and takes `command.name` from the stored receipt. A reused span adds `stored.command`, the command that the id already names. A rejected span adds `rejection`. A failed span adds `error`, the exception's message. A command fails when a hook, a view, or one of its orders throws, or when it arrives after `stop`. The runtime takes every other value from labels, so no user code lists them. `test/Test/Trace.purs` covers each outcome.

## What is unchecked

The library has no `unsafeCoerce`. Every value has the type it was built with, so no invariant has to hold for the types to be true. Two unchecked operations remain:

- `guardOf` in `Domain.Http` reads a guard with `Record.Unsafe.unsafeGet`. The `HasGuard` instance chain has already proved, from `RowToList guards`, that the record has a field with that label and that type. A `Row.Cons` constraint would make the read checked, but it reports a missing guard as a unification of the whole guards row instead of `serve needs a guard for statement`.
- The `Fail` instance at the end of the `Wire` chain has the body `unsafeCrashWith "unreachable: rejected at compile time"`, which no program that compiles can reach.

The `NoConstructors` instances in `Domain.Internal.Labels` recurse on their argument. No value of an empty type exists, so they never run.

## Compiler errors

A missing case is the compiler's own error. Iris reports the missing patterns as a `MissingPatterns` warning that names the constructor, and the build fails with `No instance found for: Partial` at the same position, because the function's signature has no `Partial` constraint. `guards` in `src/Bank/Api.purs` has no signature, so a missing case there makes `guards` itself partial. The warning stays in `Api.purs`, and the error lands at each call to `serve`.

Every other mistake the library can name ends in an instance chain whose last instance fails with a custom message:

- `HasGuard`: `serve needs a guard for <label>`.
- `KnownGuards`: `a guard names no aggregate or view of the context: <label>`.
- The `Product` instances in `Domain.Internal.Labels`: `a labelled constructor takes at most one argument; put its fields in a record`. The error appears where the library first needs the codec, such as a call to `serve`, not at the data declaration.
- `Wire`: `a context entry must be an Aggregate, a View, or a Policy, but <label> is <type>`.
- `ChangeOf`: `a view or policy must read a Change, not <type>`.
- `FindSource` and `FindTarget`: `a view or policy reads events of an aggregate this context does not contain:` and `a policy orders commands of an aggregate this context does not contain:`, each above the type. A `NoInstanceFound` for a `Row.Cons` over the hosts row follows the custom message.

`test/compile-errors.sh` applies one mistake at a time to the bank, builds, asserts a fragment of the output, and restores the file. To add a case, add an `expect` line with the file, the text to replace, the replacement, and the expected fragment. A compiler error block in the README must quote the fragment of an `expect` case and point into that case's file, and `test/check-snippets.py` checks its source line against that case's mistake. A case marked `KNOWN` pins an error that is not yet a one-line custom message. Two remain. An event sent where a command belongs prints `Cannot unify 'AccountEvent' with 'AccountCommand'` at the enclosing signature. A view whose key type disagrees with its key function prints a plain unification error.

## What the library does not do

- **Sagas.** A policy maps one change to orders and keeps no state, which is what lets its order ids be derived from the event alone. A process that waits on several events can be an aggregate of its own.
- **A durable store.** No durable backend exists to test the shape against, and an append that returns `Aff` would split today's single synchronous step that stores a commit and applies its views. The open questions below sketch the seam.
- **An HTTP client.** No PureScript caller of the API exists to shape one. A client can encode commands with `Domain.Codec.labelled`, which is the format `serve` reads.
- **`purs` compatibility.** The library targets Iris only. It is built and tested with Iris 0.1.13, and `test/compile-errors.sh` asserts Iris's exact error text, which `purs` renders differently.
- **Views over two aggregates.** A view reads one aggregate's changes. A row over two aggregates needs two views.

## Iris limits that still apply

- **No labeled rows in instance heads.** An instance head such as `{ event :: e | r }` fails with `[InstanceHeadLabeledRow]`. `ChangeOf` matches `Record row` and reads the `event` field with a `Row.Cons` constraint instead.
- **No type families.** The wiring cannot compute "the aggregate whose events are `e`" as a type. `FindSource` and `FindTarget` search the context's `RowList` with an instance chain and a functional dependency instead.
- **What Iris does allow.** Instance chains with `Prim.TypeError.Fail` give the custom errors. Non-linear instance heads give `FindSource`, `FindTarget`, and `HasGuard`, which match an entry whose type or label equals the one sought. A module can export a function whose constraint names a class it does not export, so `start` and `serve` keep `Hosts`, `Wire`, `FindSource`, `FindTarget`, `ChangeOf`, `Routes`, `HasGuard`, and `KnownGuards` private.

## Tradeoffs accepted

- A handler's type no longer says what it may produce. In the previous design, a deposit handler's signature proved it could not return `insufficientFunds`. Now `decide` returns any `AccountRejection`. Stories test what each command produces instead.
- A constructor takes at most one argument, so a payload with several fields is a record.
- A label is a constructor's name, so renaming a constructor of an event changes the stored format. A rename needs an upcaster in the event codec, as `Bank.Codec.accountEvents` does for version 1 deposits.
- A missing case prints two messages, a warning and an error about `Partial`, instead of one custom line. In exchange, the check is the compiler's own and needs no library class.
- Views and policies find aggregates by event and command type. If two aggregates in one context share an event type, every view over that type reads the first of them in label order, and the same holds for a shared command type and policy targets.
- `start` reserves the label `stop`, in exchange for a flat `rt.account.send`.
- Every view has three type parameters (`View AccountChange AccountId Summary`), so that one view type covers keyed and unkeyed views and its signature says what `read` takes.
- A keyed row's update order across streams is commit order, as described under the runtime guarantees.

## Alternatives considered

- **Declare once, in a spec record type.** This was the previous design. A spec was a record type with the fields `id`, `state`, `command`, `event`, `rejection`, and `reply`, where the three middle fields were variant rows. `aggregate` took handler records keyed by label, and a view was a record of handlers with an optional `key` field that the library detected by the record's shape. Each handler's narrow `Decision` row was checked against the spec. Two problems decided against it. First, label-keyed handler records and a field detected by shape read like a TypeScript options bag, not like PureScript. Second, `Command spec`, `Event spec`, and the other spec-indexed types could not compute the spec's rows without type families, so they were newtypes over an erased representation, read back through four `unsafeCoerce` helpers. Their soundness rested on a functional dependency, unexported constructors, and nominal type roles, with compile-error cases that tried to forge an event. Plain ADTs and a decider remove all of that machinery and keep the wire format.
- **Raw `Variant` rows.** Commands as `Variant (openAccount :: ..., ...)` give per-handler narrow rows for free. But every use site spells the label as a string (`inj @"openAccount"`), and coverage needs library classes instead of the compiler's exhaustiveness check.
- **Name a view's source by label (`view @"account"`).** This avoids the `FindSource` search, but a view could not type-check until the context exists.

## Open questions

### A store seam

A durable store would sit behind the host as a record of capabilities, with the in-memory map as its first implementation. The sketch has three parts:

1. **Load.** Read a stream's envelopes in order and replay them through `evolve` to get the state and version.
2. **Append with an expected version.** Write a commit's envelopes only if the stream is still at the version the turn started from. A mismatch means another writer got there first.
3. **Rebuild receipts from history.** Every envelope carries its `commandId`. Grouping a stream's envelopes by command id gives each receipt's events and version, and replaying up to the last envelope of each group gives its reply. A receipt also keeps the label of its command, which no envelope carries, so the store must write it with each commit. A stored command id must keep its kind, client or derived, because a client key can have the same text as a derived id. Refusals are never stored, so nothing else is needed.

The event format would be the versioned `Codec AccountEvent` that `Bank.Codec.accountEvents` already round-trips. The envelope fields (`seq`, `commandId`, `at`) still need a codec.

The cost is the guarantee that a commit and its view updates happen in one synchronous step. An append that returns `Aff` lets a crash or a reader land between the write and the view update. Views would then need their own position in each stream and a way to catch up on restart. Until a real backend needs the store, that cost buys nothing.

### Others

- Should a policy's rejected order surface somewhere other than its span, such as a dead-letter handle on `rt`?
- Should two aggregates in one context be allowed to share an event type? `FindSource` takes the first match in label order.

## Modules

| Module | What it owns |
|---|---|
| `Domain` | The user's view of the kernel and the ids: aggregates, changes, views, policies, envelopes, receipts, command ids, and `labelOf`. Re-exports only. |
| `Domain.Fold` | Left folds as values, which views run. |
| `Domain.Story` | Given/when/then stories against the pure aggregate. |
| `Domain.Runtime` | The host around `turn`, with per-stream locks, runtime-owned command fibers, views, policies, spans, and `stop`. |
| `Domain.Codec` | Labelled and versioned JSON codecs. |
| `Domain.Http` | Routes, status codes, and guards over a transport-neutral request. |
| `Domain.Internal.Kernel` | `Aggregate`, `commit`, `replay`, `Change`, `View`, `Policy`, `Order`, `Envelope`, and `Receipt`. Pure. |
| `Domain.Internal.Labels` | Labels from `Generic`, and the labelled JSON encoding and decoding. |
| `Domain.Internal.Id` | Command ids, stream ids, versions, sequences, and derived order ids. |
| `Domain.Internal.Stream` | One command's turn on one stream, and the orders a policy places for one fact, as pure functions. |
| `Bank`, `Bank.*` | The example bank, which is also the usage spec. |

User code imports the first six.
