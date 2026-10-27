# Rationale: declare once, derive everything

## Problem

At 2bb6630 the library stopped at the pure kernel, and the user wrote the rest. `Bank.Runtime` (409 lines) hand-wired sends, views, policies, read-your-writes, and spans. `Bank.Api` (479 lines) hand-wrote endpoints, JSON shapes, and a client. `Bank.Trace`, `Bank.Route`, `Bank.Codec`, and one smart constructor per label re-listed labels the variant rows already held. Types took up to six positional parameters (`Aggregate id s c e r b`). This direction moves that work into the library. A user writes each label once, and every type takes the spec as its only parameter. The constraints come from Iris 0.1.13. It accepts visible type applications, instance chains with `Prim.TypeError.Fail`, and non-linear instance heads. It has no type families, and it rejects labeled rows in instance heads.

## Usage (caller's view)

```purescript
type Account =                                   -- the one place labels are declared
  { id :: AccountId, state :: AccountState
  , command :: Variant (openAccount :: { owner :: String }, depositMoney :: { amount :: Int }, withdrawMoney :: { amount :: Int })
  , event :: Variant (accountOpened :: { owner :: String }, moneyDeposited :: Movement, moneyWithdrawn :: Movement)
  , rejection :: Variant (alreadyOpen :: {}, notOpen :: {}, insufficientFunds :: Shortfall)
  , reply :: { balance :: Int } }

account :: Aggregate Account
account = aggregate { initial: Unopened, decide: { openAccount, depositMoney, withdrawMoney }
                    , evolve: { accountOpened: \_ { owner } -> Open { owner, balance: 0 }, moneyDeposited: settle, moneyWithdrawn: settle }
                    , reply: \state -> { balance: balanceOf state } }

depositMoney :: AccountState -> { amount :: Int } -> Decision (notOpen :: {}) (moneyDeposited :: Movement)
depositMoney Unopened _ = reject @"notOpen" {}
depositMoney (Open { balance }) { amount } = emit @"moneyDeposited" { amount, balanceAfter: balance + amount }

summary :: View Account Summary                  -- handles a subset of events
provisioning :: Policy Customer Account          -- order @"openAccount" (mainAccount id) { owner: name }
bank = { account, customer, summary, statement, provisioning }   -- the context; no signature needed

rt <- start bank defaults
receipt <- rt.account.send commandId accountId (cmd @"depositMoney" { amount: 100 })
balance <- rt.summary.read accountId
response <- serve bank guards rt principal request   -- POST /account/a-1/depositMoney, GET /summary/a-1
story account [ given @"accountOpened" { owner: "sam" }, command @"depositMoney" { amount: 100 }, replies { balance: 100 } ]
```

## Shape

**The spec record is the single source of truth.** `class Spec spec id s c e r b | spec -> id s c e r b` projects a plain record type by field name. Iris rejects `instance Spec { id :: id, ... }` with `[InstanceHeadLabeledRow] Only the ( | r ) form is allowed`. The instance therefore reads fields with `RowToList` lookups (`Field`) on `Record spec`. Each library type is indexed by the spec alone: `Aggregate`, `Command`, `Event`, `Rejection`, `Order`, `View spec v`, `Policy src tgt`. Iris has no type families, so `Event Account` cannot be a synonym for `Variant (...)`. It is a nominal newtype over an erased variant. Coercions are sound because the fundep gives each spec exactly one row per role, and `type role ... nominal` blocks `coerce` across specs (per type-system-discipline). Role-typed values also make errors short. Passing an event as a command reports `Cannot unify 'Command' with 'Event'`, where raw variants would print two full rows.

**Handlers are records keyed by label, compiled to dispatch tables.** `aggregate`, `view`, and `policy` walk the handler record's `RowList` once. They check each label against the spec and build an `Object` keyed by label. Dispatch reads the variant's tag. A `decide` handler names its narrow rows in its signature. `SubRow` proves each narrow row is a sub-row of the spec's row, with the same payload types, so widening is a coercion and `expand` disappears. `decide` and `evolve` must cover their rows (`Covers`). Views and policies name any subset.

**Legible errors come from the structure.** No `Prim.Row` constraint ever sees a label that an instance chain has not checked. Before this rule, a missing handler printed `CannotUnify` and two `Union` failures around the custom message. After it, `test/compile-errors.sh` applies each mistake to the bank, builds, asserts the expected message, and reverts. Each mistake gets one line, quoted from `iris test`:

| Mistake | Error, at the user's line |
|---|---|
| missing `decide` handler | `decide is missing a handler for the command withdrawMoney` |
| misspelled `evolve` handler | `evolve handles an undeclared event moneyWithdrawm` |
| handler rejects an undeclared label | `a decide handler rejects with an undeclared rejection frozen` |
| `emit` outside the handler's row | `the handler's event row does not include moneyWithdrawn` |
| narrow payload differs from the spec | `the payload of alreadyOpen is Unit here but {} in the spec` |
| view or policy label typo | `a view handles an undeclared event moneyWithdrew` / `the target aggregate declares no command openAcount` |
| `cmd` typo | `the aggregate declares no command depositMony` |
| spec field missing | `an aggregate spec needs the field reply` |
| context entry of the wrong type | `a context entry must be an Aggregate, a View, or a Policy, but provisioning is Int` |
| view over an absent aggregate | `a view or policy reads an aggregate this context does not contain:` plus the spec |

**The runtime is two type-level passes over the context.** `Hosts` builds one host per `Aggregate` entry. `Wire` turns each aggregate into `{ send, history }` and each view into `{ read, readAt }`. It subscribes each view and policy to the aggregate whose spec matches (`FindAggregate`, a non-linear instance chain). The label names the stream category (`account/a-1`), the span (`command.account`), and the route prefix. One keyed serial executor (`Domain.Runtime.Serial`) runs every aggregate, so the in-memory host and the actor host are no longer two ways to do one task. A command's turn on its stream commits, stores the receipt, and applies every view in one synchronous effect. Each view entry has exactly one writer, its source stream, so views never lag or reorder (per separate-before-serializing-shared-state). Policies run after the turn, so an order can target any stream without deadlock.

**Idempotency covers crashes, not only retries** (per make-operations-idempotent). A retried command id returns the stored receipt and runs its policies again. Each order's id is `policy:stream@seq#index`, so the rerun dedupes at the target and heals a crash between commit and dispatch. The old id scheme left out the policy and the source stream, so two policies reacting to one event would have collided. `stop` refuses new external commands, waits for in-flight commands and their cascades, and then drains the executor. A test reverts the gate and fails with `customer/c-2 is shut down`.

**Interface depth.** A user imports `aggregate`, `emit`, `reject`, `view`, `policy`, `order`, `cmd`, `start`, `serve`, and the story builders. Behind them sit dispatch tables, receipts, watermarks, derived ids, mailboxes, spans, codecs, and routing. The only hand-written boundary code is `guards`, one predicate per aggregate and view label. A missing guard is a compile error.

**What it deliberately does not do.** Views are keyed by the source entity id. Cross-entity views and sagas (`Process`) are gone. There is no durable store and no envelope codec, because nothing persists envelopes yet. Policy rejections are traced on the target's span and do not fail the triggering command.

## Synthesis decision

Filled in by arena.

## Tradeoffs accepted

- We accept 19 `unsafeCoerce` calls in `Domain`, each between a spec-indexed newtype and the row its spec determines, in exchange for one-parameter types and short errors. They are sound only because `Spec`'s fundep holds and no constructor is exported.
- We accept that every `decide` handler needs a signature. Without one Iris reports `No instance found for: RowToList @Type ?131 ?134`, which is the one illegible error left. In exchange, each handler's narrow rows are the documentation and the guarantee.
- We accept dispatch through a label-keyed `Object` with an unreachable fallback, in exchange for no `Row.Cons` on unchecked labels.
- We accept that `start` reserves the label `stop` (`Row.Lacks "stop"`), in exchange for a flat `rt.account.send`.
- We accept exported type-class plumbing (`DecideTable`, `Table`, `Hosts`, `Wire`, `Routes`, `Labels`) because Iris requires classes in exported signatures to be exported. Each one has a safe, typed interface.

## Alternatives considered

- **Derive the rows from the handler records**, with no spec type. Labels would appear only in `decide` and `evolve`. This lost because event payload types would come from lambda inference, rejection rows from a `Nub` of unions, and every view and policy would need the source rows before the context exists. Errors would read as row unification.
- **Keep raw `Variant` rows with type synonyms for each role.** This needs no erasure, but signatures go back to positional rows. The error for a role mix-up becomes two full rows instead of `Command` versus `Event`.
- **Name a view's source by label (`view @"account"`).** This avoids the `FindAggregate` search, but a view could not typecheck its handlers until the context exists. Typing by spec keeps every module checkable alone.

## Implementation reconciliation

- `DecideCases` first walked the command row with `on`. The missing-handler error then sat behind three unification errors. It now walks the handler record into a table, and `Covers` proves exhaustiveness.
- `Member` (a checked `inj`/`on`) was deleted once dispatch moved to tables.
- `admitted` was written but not wired into the handles. The stop test now covers it.
- Iris reported `[UnreportedUnknownType] ... unsupported internal compiler state` instead of `NotInScope` when `Data.Maybe (Maybe(..))` was not imported in `Domain.Codec`. This is worth filing against Iris.

## Open questions and risks

- Should views take a `keyOf`, so a customer-level view can fold many accounts? Today that would need a second aggregate or a policy.
- Should a durable store sit behind `Host`, with the versioned `Codec (Event spec)` as its format? `Bank.Codec.accountEvents` already round-trips and upcasts.
- Should a policy's rejected order surface somewhere other than its span, such as a dead-letter handle on `rt`?
- Do two aggregates in one context ever share a spec? `FindAggregate` takes the first.

## Next implementation step

Put a `Store` behind `newHost` that appends envelopes encoded with `labelled`, with the in-memory map as its first implementation.

## Module map

| Module | Public exports |
|---|---|
| `Domain` | `Spec`, `Command`, `Event`, `Rejection`, `cmd`, `event`, `rejection`, `Labelled` (`toVariant`, `fromVariant`), `tagOf`, `Decision`, `emit`, `reject`, `Aggregate`, `aggregate`, `Decider`, `decider`, `commit`, `replay`, `View`, `view`, `runView`, `Policy`, `policy`, `Order`, `order`, `Addressed`, `orders`, `Envelope`, `Receipt`, `DecideTable`, `Table`, ids from `Domain.Id` |
| `Domain.Id` | `CommandId`, `StreamId`, `Version`, `Sequence`, `parseCommandId`, `commandIdText`, `streamIdText`, `parseVersion`, `versionNumber`, `sequenceNumber`, `zeroVersion`, `streamOf`, `advance`, `sequencesAfter`, `derivedCommandId` |
| `Domain.Labels` | `HasLabel`, `Covers`, `Field`, `IsVariant`, `SubRow`, `Has` |
| `Domain.Story` | `Step`, `given`, `command`, `emits`, `rejects`, `replies`, `story` |
| `Domain.Runtime` | `Options`, `Hooks`, `Span`, `defaults`, `start`, `AggregateHandle`, `ViewHandle`, `Hosts`, `Wire`, `FindAggregate` |
| `Domain.Runtime.Serial` | `Serial`, `new`, `run`, `close` |
| `Domain.Codec` | `Codec`, `codec`, `encode`, `decode`, `json`, `labelled`, `tagged`, `Upcaster`, `upcaster`, `versioned`, `EncodeCases`, `DecodeCases` |
| `Domain.Http` | `Method`, `Request`, `Response`, `serve`, `Routes` |

Deleted: `Domain.Runtime.InMemory`, `.Actor`, `.Projection`, `.Trace`, `Domain.Trace`, `Domain.Auth`, `Domain.Id.Internal`, `Bank.Runtime`, `Bank.Route`, `Bank.Trace`, `Main`, `iris-check/`, `DESIGN.md`, and the `node-http`, `routing-duplex`, `profunctor`, `lists`, `unfoldable`, `quickcheck`, and `gen` dependencies.

## Line counts (`wc -l`)

| Bank file | 2bb6630 | now |
|---|---:|---:|
| `Account` | 210 | 102 |
| `Customer` | 87 | 54 |
| `Views` | 58 | 39 |
| `Policy` | 25 | 22 |
| `Runtime` | 409 | 0 |
| `Api` | 479 | 40 |
| `Codec` | 153 | 28 |
| `Route` | 83 | 0 |
| `Trace` | 48 | 0 |
| `Bank` (context) | 0 | 13 |
| **Bank total** | **1,552** | **298** |
| Library | 1,852 | 1,818 |
| Tests | 1,665 | 210 |
