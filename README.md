# purescript-domain

purescript-domain is an event-sourcing library for PureScript, built for the Iris compiler. You write an aggregate as ordinary data types and a decider: commands, events, and rejections are ADTs, and `decide` and `evolve` are functions written with `case`. The library adds a concurrent runtime that commits each command id once per stream, read models built from folds, policies, JSON codecs, an HTTP boundary, and given/when/then tests. The compiler's exhaustiveness check finds every place a new command or event needs a decision.

[`docs/design.md`](docs/design.md) explains the design, the runtime guarantees, and the tradeoffs.

## Install

The library builds with Iris 0.1.13 and is not tested with `purs`. Add it to your `spago.yaml` as a Git package:

```yaml
package:
  name: my-app
  dependencies:
    - aff
    - domain
    - effect
    - either
    - maybe
    - newtype
    - prelude
workspace:
  packageSet:
    registry: 81.11.0
  extraPackages:
    domain:
      git: https://github.com/saiashirwad/purescript-domain.git
      ref: main
```

Then run `iris build`.

To run this repository's checks, run this command from its root:

```sh
test/check.sh
```

`test/check.sh` is the one gate. It runs `iris test` and fails unless the output has the line `All tests passed.`, because a suite that stops early can still exit 0. Then it runs `test/compile-errors.sh`, which applies common mistakes and asserts each compiler message, and `test/check-snippets.py`, which checks every code block in this README and in `docs/design.md` against the source.

## Build a bank

This tutorial builds the example bank in `src/Bank`. Every PureScript snippet is copied from `src/Bank` or `test/`, so you can read each one in context.

### Declare the types

An account has an id, a state, and three sum types: what it accepts, what it records, and why it refuses.

```purescript
newtype AccountId = AccountId String

derive instance Newtype AccountId _
derive newtype instance Eq AccountId
derive newtype instance Ord AccountId
derive newtype instance Show AccountId

data AccountState = Unopened | Open { owner :: CustomerId, balance :: Int }
```

The id must be a `Newtype` over `String`. The runtime uses it to name the stream (`account/a-1`), and the HTTP boundary reads it from the path.

```purescript
data AccountCommand
  = OpenAccount { owner :: CustomerId }
  | DepositMoney { amount :: Int }
  | WithdrawMoney { amount :: Int }

derive instance Eq AccountCommand
derive instance Generic AccountCommand _
instance Show AccountCommand where
  show = genericShow

data AccountEvent
  = AccountOpened { owner :: CustomerId }
  | MoneyDeposited Movement
  | MoneyWithdrawn Movement
```

```purescript
data AccountRejection
  = AlreadyOpen
  | NotOpen
  | NonPositiveAmount { amount :: Int }
  | InsufficientFunds Shortfall
```

Each of the three types derives `Eq` and `Generic` and has a `Show` instance, as `AccountCommand` does. `Generic` gives each constructor its label: the constructor's name with the first letter lowered. `MoneyDeposited` is labelled `moneyDeposited` in routes, span attributes, and JSON. A constructor takes no argument or one, so put several fields in a record.

### Write the aggregate

`Aggregate` takes the id, command, event, rejection, state, and reply types, in that order. Give the full type a name:

```purescript
type Account = Aggregate AccountId AccountCommand AccountEvent AccountRejection AccountState Balance
```

An aggregate is a record of four fields:

```purescript
import Domain (Aggregate(..), Change)
```

```purescript
account :: Account
account = Aggregate
  { initial: Unopened
  , decide
  , evolve
  , reply: \state -> { balance: balanceOf state }
  }
```

`decide` answers a command from the current state with `Right` and the events, or `Left` and a rejection. `evolve` folds one event into the state. `reply` computes what the sender gets back after the events are folded in.

```purescript
decide :: AccountCommand -> AccountState -> Either AccountRejection (Array AccountEvent)
decide = case _, _ of
  OpenAccount { owner }, Unopened -> Right [ AccountOpened { owner } ]
  OpenAccount _, Open _ -> Left AlreadyOpen
  _, Unopened -> Left NotOpen
  DepositMoney { amount }, Open { balance }
    | amount <= 0 -> Left (NonPositiveAmount { amount })
    | otherwise -> Right [ MoneyDeposited { amount, balanceAfter: balance + amount } ]
  WithdrawMoney { amount }, Open { balance }
    | amount <= 0 -> Left (NonPositiveAmount { amount })
    | amount > balance -> Left (InsufficientFunds { balance, requested: amount })
    | otherwise -> Right [ MoneyWithdrawn { amount, balanceAfter: balance - amount } ]

evolve :: AccountState -> AccountEvent -> AccountState
evolve state = case _ of
  AccountOpened { owner } -> Open { owner, balance: 0 }
  MoneyDeposited movement -> settle movement
  MoneyWithdrawn movement -> settle movement
  where
  settle { balanceAfter } = case state of
    Open open -> Open open { balance = balanceAfter }
    Unopened -> Unopened
```

Avoid a wildcard over the command or the event in these `case` expressions. Without one, a new command or event fails the build until you handle it. `balanceOf` is in `src/Bank/Account.purs`.

### Test it with a story

A story runs given events and a command against the pure aggregate, then checks what it emitted, rejected, or replied.

```purescript
import Domain.Story (emits, given, rejects, replies, sends, story)
```

```purescript
  expectStory "deposits into an open account and replies with the balance" $ story account
    [ given (AccountOpened { owner: CustomerId "sam" })
    , sends (DepositMoney { amount: 100 })
    , emits (MoneyDeposited { amount: 100, balanceAfter: 100 })
    , replies { balance: 100 }
    ]
  expectStory "refuses to withdraw more than the balance" $ story account
    [ given (AccountOpened { owner: CustomerId "sam" })
    , given (MoneyDeposited { amount: 100, balanceAfter: 100 })
    , sends (WithdrawMoney { amount: 150 })
    , rejects (InsufficientFunds { balance: 100, requested: 150 })
    ]
```

The `emits` steps after a command list its events exactly, in order, so a story that leaves out an emitted event fails. `story` returns `Right` with the final state, or `Left` with the first failed expectation, such as `expected events ..., got ...`. `expectStory` in `test/Test/Stories.purs` turns a `Left` into a test failure.

### Add a view

A view is a read model with one row per key. Each row is a fold over changes. A change is a committed event, the id of its stream, and the stream's state right after the event:

```purescript
type AccountChange = Change AccountId AccountState AccountEvent
```

`Fold.fold` takes an initial row and a step function. `byId` keeps one row per aggregate id.

```purescript
import Domain (View, byId, keyed)
import Domain.Fold as Fold
```

```purescript
type Summary = { owner :: Maybe CustomerId, balance :: Int }

summary :: View AccountChange AccountId Summary
summary = byId $ Fold.fold { owner: Nothing, balance: 0 } \row { event } -> case event of
  AccountOpened { owner } -> row { owner = Just owner }
  MoneyDeposited { balanceAfter } -> row { balance = balanceAfter }
  MoneyWithdrawn { balanceAfter } -> row { balance = balanceAfter }
```

A row that collects values is a `Fold.foldMap` into a monoid. The statement appends one line per movement:

```purescript
statement :: View AccountChange AccountId (Array StatementLine)
statement = byId $ Fold.foldMap \{ event } -> case event of
  AccountOpened _ -> []
  MoneyDeposited { amount, balanceAfter } -> [ { kind: "deposit", amount, balance: balanceAfter } ]
  MoneyWithdrawn { amount, balanceAfter } -> [ { kind: "withdrawal", amount, balance: balanceAfter } ]
```

`Fold` is also a `Functor`, an `Applicative`, and a `Profunctor`, so you can combine two folds with `<$>` and `<*>`, or adapt a fold's input with `Fold.premap`. `test/Test/Fold.purs` has examples.

### Add a keyed view

To fold many aggregates into one row, use `keyed` with a function from the change to the row's key. `Nothing` skips the change.

```purescript
customerOf :: AccountState -> Maybe CustomerId
customerOf Unopened = Nothing
customerOf (Open { owner }) = Just owner
```

```purescript
type Dashboard = { accounts :: Int, balance :: Int }
```

```purescript
dashboard :: View AccountChange CustomerId Dashboard
dashboard = keyed (customerOf <<< _.state) $ Fold.fold { accounts: 0, balance: 0 } \row { event } ->
  case event of
    AccountOpened _ -> row { accounts = row.accounts + 1 }
    MoneyDeposited { amount } -> row { balance = row.balance + amount }
    MoneyWithdrawn { amount } -> row { balance = row.balance - amount }
```

The `MoneyDeposited` event does not name the customer. The account's state does.

### Add a policy

A policy reacts to one aggregate's changes by ordering commands on another. The bank has a second aggregate, `customer`, in `src/Bank/Customer.purs`:

```purescript
type Customer = Aggregate CustomerId CustomerCommand CustomerEvent CustomerRejection CustomerState Unit
```

When a customer registers, `provisioning` opens their main account. An order is a record of the target id and the command.

```purescript
import Domain (Policy(..))
```

```purescript
provisioning :: Policy CustomerChange AccountId AccountCommand
provisioning = Policy \{ id, event } -> case event of
  CustomerRegistered _ -> [ { to: mainAccount id, command: OpenAccount { owner: id } } ]

mainAccount :: CustomerId -> AccountId
mainAccount (CustomerId id) = AccountId (id <> "-main")
```

You do not give an order a command id. The runtime derives one from the policy's label and the event that caused the order, so a retried trigger never opens a second account.

### Wire a context

A context is a record of aggregates, views, and policies. It needs no type signature.

```purescript
bank = { account, customer, summary, statement, dashboard, provisioning }
```

Each label becomes a runtime handle (`rt.account`), a stream category (`account/a-1`), a span name (`command.account`), and an HTTP route prefix (`/account`). A view finds its aggregate by event type. A policy finds its source by event type and its target by command type.

### Send a command and read a view

`start` returns one handle per aggregate and view, plus `stop`. These lines are from `test/Test/Runtime.purs`:

```purescript
import Domain (sequenceNumber, versionNumber)
import Domain.Runtime (Refusal(..), start)
```

```purescript
  rt <- start bank testOptions
  let
    sam = AccountId "a-1"
  before <- rt.summary.read sam
  assertEqual "a view of an unseen entity is empty" Nothing before

  _ <-
    expectRight "open" =<< rt.account.send (commandId "open") sam
      (OpenAccount { owner: CustomerId "sam" })
  afterOpen <- rt.summary.read sam
  assertEqual "a read right after send sees the open"
    (Just { owner: Just (CustomerId "sam"), balance: 0 }) afterOpen
```

`send` takes a command id, the aggregate id, and the command. It returns `Right` with a receipt that holds `commandId`, `version`, `events`, and `reply`, or `Left` with a `Refusal`. A refusal is `Rejected` with the aggregate's rejection, or `Reused` when the command id already names a different command on that stream. Views update in the same step as the commit, so the read right after `send` sees the new row.

A command id names one command to one stream. Sending the same id again returns the stored receipt and appends nothing:

```purescript
  deposited <- expectRight "deposit"
    =<< rt.account.send (commandId "deposit") sam (DepositMoney { amount: 100 })
  assertEqual "deposit replies with the balance" 100 deposited.reply.balance
  assertEqual "deposit is version 2" 2 (versionNumber deposited.version)

  retried <- expectRight "retry"
    =<< rt.account.send (commandId "deposit") sam (DepositMoney { amount: 100 })
  assertEqual "a retry returns the stored receipt" deposited retried
  history <- rt.account.history sam
  assertEqual "a retry appends nothing" [ 1, 2 ] (map (sequenceNumber <<< _.seq) history)
```

The tests use two helpers from `test/Test/Support.purs`:

```purescript
testOptions :: Options
testOptions = defaults { now = pure epoch }
```

```purescript
commandId :: String -> CommandId
commandId raw = unsafePartial (fromJust (parseCommandId raw))
```

In application code, pass `defaults`, and parse each command id with `parseCommandId`, which returns `Nothing` for a blank string. Call `rt.stop` to shut down. It refuses new commands, lets in-flight commands and their policy orders finish, and then returns. A command keeps running when its caller gives up, so a request timeout never strands a commit without its policy orders.

### Serve HTTP

`serve` derives routes, body codecs, and status codes from the context. You write the guards. An aggregate's guard takes the principal, the id from the path, and the decoded command. A view's guard takes the principal and the key from the path.

```purescript
guards =
  { account: \principal id -> case _ of
      OpenAccount _ -> isAdmin principal
      DepositMoney _ -> ownsAccount principal id
      WithdrawMoney _ -> ownsAccount principal id
  , customer: \principal _ _ -> isAdmin principal
  , summary: ownsAccount
  , statement: ownsAccount
  , dashboard: \principal id -> principal.customer == Just id || isAdmin principal
  }

ownsAccount :: Principal -> AccountId -> Boolean
ownsAccount principal id = Set.member id principal.accounts || isAdmin principal
```

The account guard matches every command without a wildcard, so adding a command fails the build until you decide who may send it. Only an admin opens an account, because `OpenAccount` names its owner. A customer who could open their own account could open it in someone else's name.

`serve bank guards rt` takes a principal and a transport-neutral request. Your server adapter fills the request:

```purescript
import Domain.Http (Method(..), serve)
```

```purescript
    call who method path key body = serve bank guards rt who
      { method, path, headers: maybe Map.empty (Map.singleton "idempotency-key") key, body }
```

An admin's `POST /account/a-1/openAccount` with an `idempotency-key` header and the body `{"owner":"sam"}` returns status 200 and the receipt:

```json
{"events":[{"type":"accountOpened","value":{"owner":"sam"}}],"reply":{"balance":0},"version":1}
```

A rejection returns 409 with `{"type":"insufficientFunds","value":{"balance":0,"requested":5}}`. A guard that refuses returns 403. `serve` decodes the command before it calls the guard, so a body that does not decode returns 400, even for a principal the guard would refuse. An `idempotency-key` reused for a different command returns 422. A failure inside the runtime, such as a request after `rt.stop`, returns 500 with `{"error":"internal error"}` and never the failure's own message. The operator still sees the cause. The runtime emits a span for every command, and a failed command's span has the outcome `failed` and the message in its `error` attribute. `GET /summary/a-1` reads the view's row. `GET /dashboard/sam` reads the keyed row for customer `sam`.

## Common mistakes and their errors

Each of these is a case in `test/compile-errors.sh`, which applies the mistake to the bank and asserts the message. The output is from `iris build`.

A `decide` without the case for opening an account that is already open:

```
Warning! · [MissingPatterns] · src/Bank/Account.purs:91:10
  •
  │
  • Pattern match is not exhaustive. Missing: OpenAccount ({ owner: _ }), Open _
  │
  │   decide = case _, _ of
  │            ╰───────────
  •

Error! · [NoInstanceFound] · src/Bank/Account.purs:91:10
  •
  │
  • No instance found for: Partial
  │
  │   decide = case _, _ of
  │            ╰───────────
  •
```

Iris reports the missing case as a warning, and the build fails because the function's signature has no `Partial` constraint. The same pair of messages appears for a missing case in `evolve`, in a view's fold, or in a policy. `guards` has no signature, so for a missing case in a guard, the error lands where you call `serve`, and the warning still names the missing command.

The other cases have one-line messages:

- A guard missing from `guards`: `serve needs a guard for statement`.
- A constructor with two arguments: `a labelled constructor takes at most one argument; put its fields in a record`.
- A context entry that is not an aggregate, a view, or a policy: `a context entry must be an Aggregate, a View, or a Policy, but provisioning is Int`.
- A view or policy whose aggregate is missing from the context: `a view or policy reads events of an aggregate this context does not contain:`, followed by the event type.

## Modules

- `Domain` holds aggregates, views, policies, changes, command ids, versions, envelopes, and receipts. It is pure.
- `Domain.Fold` builds the folds that views run. Import it qualified.
- `Domain.Story` runs given/when/then stories against an aggregate.
- `Domain.Runtime` starts a context and returns its handles.
- `Domain.Http` serves a context over a transport-neutral request type.
- `Domain.Codec` encodes commands, events, and rejections as labelled JSON, with versioned upcasting.

The modules under `Domain.Internal` are the library's own plumbing. Do not import them.
