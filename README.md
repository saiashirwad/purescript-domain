# purescript-domain

A domain modeling library for PureScript. Describe what your application can do in its own terms: commands ask for changes, aggregates decide what happens, events record those changes, views answer questions, and policies react to events with new commands.

The design takes inspiration from [Alchemy Domain](https://alchemy.run/domain/), using PureScript data types, pattern matching, and functions to express the model. The library provides an in-memory runtime, story tests, JSON codecs, and a transport-neutral HTTP boundary around it.

The runtime currently keeps its streams and views in memory; they do not survive a restart. See [the design](docs/design.md) for its guarantees and tradeoffs.

## Install

The library builds with Iris 0.1.13 and has not been tested with `purs`. Add `domain` to your dependencies and configure it as a Git package in `spago.yaml`:

```yaml
package:
  name: my-app
  dependencies:
    - domain
    - prelude
workspace:
  packageSet:
    registry: 81.11.0
  extraPackages:
    domain:
      git: https://github.com/saiashirwad/purescript-domain.git
      ref: main
```

Run `iris build`. Add the dependencies your application imports as needed.

## Model a bank account

The examples below come from the [bank model](src/Bank). An account accepts commands, records events, and can reject a request. These are ordinary sum types:

```purescript
data AccountCommand
  = OpenAccount { owner :: CustomerId }
  | DepositMoney { amount :: Int }
  | WithdrawMoney { amount :: Int }
```

```purescript
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

The account's state is also a data type:

```purescript
data AccountState = Unopened | Open { owner :: CustomerId, balance :: Int }
```

An aggregate brings those types together with four functions or values:

```purescript
type Account = Aggregate AccountId AccountCommand AccountEvent AccountRejection AccountState Balance
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

`decide` checks a command against the current state and returns events or a rejection. `evolve` applies each event to the state. `reply` computes the result returned after the events are applied.

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
```

```purescript
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

Pattern matching keeps the rules visible and lets the compiler check coverage. See [Bank.Account](src/Bank/Account.purs) for the complete definition, including the ID, instances, and helpers. IDs used by the runtime are newtypes over `String`; commands, events, and rejections derive `Generic` for their labels. Constructors used at the JSON boundary take at most one argument, so multiple fields go in a record.

## Test the rules with stories

A story tests the aggregate without starting the runtime. Given an account's history, send a command and check its events, rejection, or reply:

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

`story` returns the final state or a failed expectation; `expectStory` is a test helper in [Test.Stories](test/Test/Stories.purs). The `emits` steps must list all emitted events in order.

## Build views

A view folds committed changes into rows that can be read. Each change contains the aggregate ID, the event, and the aggregate state after that event. This view keeps a summary per account:

```purescript
type Summary = { owner :: Maybe CustomerId, balance :: Int }
```

```purescript
summary :: View AccountChange AccountId Summary
summary = byId (Fold.fold initial step)
  where
  initial = { owner: Nothing, balance: 0 }

  step row { event } = case event of
    AccountOpened { owner } -> row { owner = Just owner }
    MoneyDeposited { balanceAfter } -> row { balance = balanceAfter }
    MoneyWithdrawn { balanceAfter } -> row { balance = balanceAfter }
```

Use `byId` for one row per aggregate, or `keyed` to choose a different key. The bank's [dashboard](src/Bank/Views.purs) uses the account's state to group changes from several accounts by customer.

## React with policies

A policy turns a change into commands for another aggregate. When a customer registers, this policy opens their main account:

```purescript
provisioning :: Policy CustomerChange AccountId AccountCommand
provisioning = Policy \{ id, event } -> case event of
  CustomerRegistered _ -> [ { to: mainAccount id, command: OpenAccount { owner: id } } ]

mainAccount :: CustomerId -> AccountId
mainAccount (CustomerId id) = AccountId (id <> "-main")
```

The runtime derives command IDs for policy orders from their triggering events, so retries address the same orders. See [Bank.Policy](src/Bank/Policy.purs).

## Run the model

A context is a record of aggregates, views, and policies:

```purescript
bank = { account, customer, summary, statement, dashboard, provisioning }
```

`start` wires the entries by their types and returns handles under the same labels. In this test, a command opens an account and a view reads the result:

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

`send` takes a command ID, an aggregate ID, and a command. It returns a receipt with the events, version, and reply, or a refusal. A read after `send` sees the committed view update. Use `Domain.Runtime.defaults` for the runtime options and `parseCommandId` to validate IDs; the example's `testOptions` and `commandId` are [test helpers](test/Test/Support.purs). Call `rt.stop` when finished.

Command IDs are scoped to a stream. Reusing an ID with the same command constructor returns its stored receipt without appending events, even if the payload differs; reusing it with a different constructor returns `Reused`. Rejected commands are not stored. Commands on one stream run in order, while different streams can run concurrently.

## Expose an HTTP boundary

`Domain.Http.serve` derives routes and JSON codecs from the context. You provide guards for each aggregate and view:

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
```

A server adapter passes a principal and a request to `serve bank guards rt`. For example, `POST /account/a-1/openAccount` with an `idempotency-key` header and body `{"owner":"sam"}` returns a receipt; `GET /summary/a-1` reads the view. The HTTP boundary is transport-neutral, so the application supplies the server adapter. See [Bank.Api](src/Bank/Api.purs) and [Test.Http](test/Test/Http.purs) for the guards and requests.

`Domain.Codec` also provides labelled JSON codecs and versioned upcasting; [Bank.Codec](src/Bank/Codec.purs) shows an event codec.

## Checks

From the repository root, run:

```sh
test/check.sh
```

This runs the test suite, compiler-error checks, and documentation snippet checks. The [design document](docs/design.md) explains the runtime, limitations, and implementation decisions in more detail.
