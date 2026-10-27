# purescript-domain

purescript-domain is an event-sourcing library for PureScript, built for the Iris compiler. You declare an aggregate's commands, events, rejections, and reply once, as rows of a spec record type. The library derives the rest from that spec: typed constructors, a concurrent runtime that commits each command id once per stream, read models, policies, JSON codecs, an HTTP boundary, and given/when/then tests. A missing or misspelled handler fails the build with one error that names the label.

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
    - maybe
    - newtype
    - prelude
    - variant
workspace:
  packageSet:
    registry: 81.11.0
  extraPackages:
    domain:
      git: https://github.com/saiashirwad/purescript-domain.git
      ref: main
```

Then run `iris build`.

To run this repository's own checks, run these commands from its root:

```sh
iris test
test/compile-errors.sh
```

`iris test` ends with `All tests passed.`, and `test/compile-errors.sh` ends with `all compile-error cases match`.

## Build a bank

This tutorial builds the example bank in `src/Bank`. Every PureScript snippet is copied from `src/Bank` or `test/`, so you can read each one in context.

### Declare the spec

An aggregate's spec is a record type with six fields. `command`, `event`, and `rejection` are variant rows, and each label in them is declared here and nowhere else.

```purescript
newtype AccountId = AccountId String

derive instance Newtype AccountId _
derive newtype instance Eq AccountId
derive newtype instance Ord AccountId
derive newtype instance Show AccountId

data AccountState = Unopened | Open { owner :: CustomerId, balance :: Int }

type Movement = { amount :: Int, balanceAfter :: Int }

type Shortfall = { balance :: Int, requested :: Int }

type Account =
  { id :: AccountId
  , state :: AccountState
  , command ::
      Variant
        ( openAccount :: { owner :: CustomerId }
        , depositMoney :: { amount :: Int }
        , withdrawMoney :: { amount :: Int }
        )
  , event ::
      Variant
        ( accountOpened :: { owner :: CustomerId }
        , moneyDeposited :: Movement
        , moneyWithdrawn :: Movement
        )
  , rejection :: Variant (alreadyOpen :: {}, notOpen :: {}, insufficientFunds :: Shortfall)
  , reply :: { balance :: Int }
  }
```

The id must be a `Newtype` over `String`. The runtime uses it to name the stream (`account/a-1`), and the HTTP boundary reads it from the path.

### Write the aggregate

`aggregate` takes handler records keyed by label. `decide` needs one handler per command, and `evolve` needs one handler per event.

```purescript
import Domain (Aggregate, Decision, aggregate, emit, reject)
```

```purescript
account :: Aggregate Account
account = aggregate
  { initial: Unopened
  , decide: { openAccount, depositMoney, withdrawMoney }
  , evolve:
      { accountOpened: \_ { owner } -> Open { owner, balance: 0 }
      , moneyDeposited: settle
      , moneyWithdrawn: settle
      }
  , reply: \state -> { balance: balanceOf state }
  }
```

Each `decide` handler takes the state and the command's payload. Its `Decision` type names the rejections and events that this handler may produce. `emit` and `reject` take the label as a type argument.

```purescript
depositMoney ::
  AccountState -> { amount :: Int } -> Decision (notOpen :: {}) (moneyDeposited :: Movement)
depositMoney Unopened _ = reject @"notOpen" {}
depositMoney (Open { balance }) { amount } =
  emit @"moneyDeposited" { amount, balanceAfter: balance + amount }

withdrawMoney ::
  AccountState ->
  { amount :: Int } ->
  Decision (notOpen :: {}, insufficientFunds :: Shortfall) (moneyWithdrawn :: Movement)
withdrawMoney Unopened _ = reject @"notOpen" {}
withdrawMoney (Open { balance }) { amount }
  | amount > balance = reject @"insufficientFunds" { balance, requested: amount }
  | otherwise = emit @"moneyWithdrawn" { amount, balanceAfter: balance - amount }
```

Give every `decide` handler a signature. The library checks the narrow rows in that signature against the spec, so a deposit handler cannot return `insufficientFunds`. `openAccount`, `settle`, and `balanceOf` are in `src/Bank/Account.purs`.

### Test it with a story

A story runs given events and a command against the pure aggregate, then checks what it emitted, rejected, or replied.

```purescript
import Domain.Story (command, given, rejects, replies, emits, story)
```

```purescript
  expectStory "deposits into an open account and replies with the balance" $ story account
    [ given @"accountOpened" { owner: CustomerId "sam" }
    , command @"depositMoney" { amount: 100 }
    , emits @"moneyDeposited" { amount: 100, balanceAfter: 100 }
    , replies { balance: 100 }
    ]
  expectStory "refuses to withdraw more than the balance" $ story account
    [ given @"accountOpened" { owner: CustomerId "sam" }
    , given @"moneyDeposited" { amount: 100, balanceAfter: 100 }
    , command @"withdrawMoney" { amount: 150 }
    , rejects @"insufficientFunds" { balance: 100, requested: 150 }
    ]
```

`story` returns `Right` with the final state, or `Left` with the first failed expectation, such as `expected event ..., got ...`. `expectStory` in `test/Test/Stories.purs` turns a `Left` into a test failure.

### Add a view

A view is a read model with one row per key. Its handlers name only the events it cares about. Without a `key` field, the row key is the aggregate's id.

```purescript
import Domain (View, view)
```

```purescript
type Summary = { owner :: Maybe CustomerId, balance :: Int }

summary :: View Account AccountId Summary
summary = view
  { initial: { owner: Nothing, balance: 0 }
  , on:
      { accountOpened: \state { owner } -> state { owner = Just owner }
      , moneyDeposited: \state { balanceAfter } -> state { balance = balanceAfter }
      , moneyWithdrawn: \state { balanceAfter } -> state { balance = balanceAfter }
      }
  }
```

### Add a keyed view

To fold many aggregates into one row, add a `key`. The runtime calls `key` with the aggregate's state right after each event. The function returns the row for that event, or `Nothing` to skip the event.

```purescript
customerOf :: AccountState -> Maybe CustomerId
customerOf Unopened = Nothing
customerOf (Open { owner }) = Just owner
```

```purescript
type Dashboard = { accounts :: Int, balance :: Int }
```

```purescript
dashboard :: View Account CustomerId Dashboard
dashboard = view
  { initial: { accounts: 0, balance: 0 }
  , key: customerOf
  , on:
      { accountOpened: \row _ -> row { accounts = row.accounts + 1 }
      , moneyDeposited: \row { amount } -> row { balance = row.balance + amount }
      , moneyWithdrawn: \row { amount } -> row { balance = row.balance - amount }
      }
  }
```

The `moneyDeposited` event does not name the customer. The account's state does.

### Add a policy

A policy reacts to one aggregate's events by ordering commands on another. The bank has a second aggregate, `customer`, declared in `src/Bank/Customer.purs` the same way as `account`:

```purescript
type Customer =
  { id :: CustomerId
  , state :: CustomerState
  , command :: Variant (registerCustomer :: { name :: String })
  , event :: Variant (customerRegistered :: { name :: String })
  , rejection :: Variant (alreadyRegistered :: {})
  , reply :: Unit
  }
```

When a customer registers, `provisioning` opens their main account:

```purescript
import Domain (Policy, order, policy)
```

```purescript
provisioning :: Policy Customer Account
provisioning = policy
  { customerRegistered: \customerId _ ->
      [ order @"openAccount" (mainAccount customerId) { owner: customerId } ]
  }

mainAccount :: CustomerId -> AccountId
mainAccount (CustomerId id) = AccountId (id <> "-main")
```

You do not give an order a command id. The runtime derives one from the policy's label and the event that caused the order, so a retried trigger never opens a second account.

### Wire a context

A context is a record of aggregates, views, and policies. It needs no type signature.

```purescript
bank = { account, customer, summary, statement, dashboard, provisioning }
```

Each label becomes a runtime handle (`rt.account`), a stream category (`account/a-1`), a span name (`command.account`), and an HTTP route prefix (`/account`). Each view and policy finds its aggregates by spec type. `statement` is a third view, in `src/Bank/Views.purs`.

### Send a command and read a view

`start` returns one handle per aggregate and view, plus `stop`. These lines are from `test/Test/Runtime.purs`:

```purescript
import Domain (cmd, event, rejection, sequenceNumber, versionNumber)
import Domain.Runtime (Refusal(..), start)
```

```purescript
  rt <- start bank testOptions
  let
    sam = AccountId "a-1"
  before <- rt.summary.read sam
  assertEqual "a view of an unseen entity is empty" Nothing before

  _ <- expectRight "open"
    =<< rt.account.send (commandId "open") sam (cmd @"openAccount" { owner: CustomerId "sam" })
  afterOpen <- rt.summary.read sam
  assertEqual "a read right after send sees the open"
    (Just { owner: Just (CustomerId "sam"), balance: 0 }) afterOpen
```

`send` takes a command id, the aggregate id, and a command built with `cmd`. It returns `Right` with a receipt that holds `commandId`, `version`, `events`, and `reply`, or `Left` with a `Refusal`. A refusal is `Rejected` with the aggregate's rejection, or `Reused` when the command id already names a different command on that stream. Views update in the same step as the commit, so the read right after `send` sees the new row.

A command id names one command to one stream. Sending the same id again returns the stored receipt and appends nothing:

```purescript
  deposited <- expectRight "deposit"
    =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  assertEqual "deposit replies with the balance" 100 deposited.reply.balance
  assertEqual "deposit is version 2" 2 (versionNumber deposited.version)

  retried <- expectRight "retry"
    =<< rt.account.send (commandId "deposit") sam (cmd @"depositMoney" { amount: 100 })
  assertEqual "a retry returns the stored receipt" (show deposited) (show retried)
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

`serve` derives routes, body codecs, and status codes from the context. You write one guard per aggregate and view label. A missing guard is a compile error.

```purescript
type Guard id = Principal -> id -> Boolean

guards ::
  { account :: Guard AccountId
  , customer :: Guard CustomerId
  , summary :: Guard AccountId
  , statement :: Guard AccountId
  , dashboard :: Guard CustomerId
  }
guards =
  { account: ownsAccount
  , customer: \principal _ -> isAdmin principal
  , summary: ownsAccount
  , statement: ownsAccount
  , dashboard: \principal id -> principal.customer == Just id || isAdmin principal
  }
```

`serve bank guards rt` takes a principal and a transport-neutral request. Your server adapter fills the request:

```purescript
import Domain.Http (Method(..), serve)
```

```purescript
    call who method path key query body = serve bank guards rt who
      { method
      , path
      , query: Map.fromFoldable query
      , headers: maybe' Map.empty (Map.singleton "idempotency-key") key
      , body
      }
```

`POST /account/a-1/openAccount` with an `idempotency-key` header and the body `{"owner":"sam"}` returns status 200 and the receipt:

```json
{"events":[{"type":"accountOpened","value":{"owner":"sam"}}],"reply":{"balance":0},"version":1}
```

A rejection returns 409 with `{"type":"insufficientFunds","value":{"balance":0,"requested":5}}`. An `idempotency-key` reused for a different command returns 422. `GET /summary/a-1` reads the view's row. `GET /dashboard/sam` reads the keyed row for customer `sam`.

## Common mistakes and their errors

Each of these is a case in `test/compile-errors.sh`, which applies the mistake to the bank and asserts the message. The output is from `iris build`.

A `decide` record without a handler for `withdrawMoney`:

```
Error! · [CustomFailure] · src/Bank/Account.purs:53:11
  •
  │
  • decide is missing a handler for the command withdrawMoney
  │
  │   account = aggregate
  │             ╰────────
  •
```

A deposit handler that emits `moneyWithdrawn`, which its `Decision` row does not include:

```
Error! · [CustomFailure] · src/Bank/Account.purs:75:3
  •
  │
  • the handler's event row does not include moneyWithdrawn
  │
  │     emit @"moneyWithdrawn" { amount, balanceAfter: balance + amount }
  │     ╰────────────────────────────────────────────────────────────────
  •
```

The dashboard without its `key` field:

```
Error! · [CustomFailure] · src/Bank/Views.purs:42:13
  •
  │
  • a view with rows keyed by CustomerId needs key :: AccountState -> Maybe CustomerId
  │
  │   dashboard = view
  │               ╰───
  •
```

## Modules

- `Domain` declares specs, aggregates, views, and policies. It is pure.
- `Domain.Story` runs given/when/then stories against an aggregate.
- `Domain.Runtime` starts a context and returns its handles.
- `Domain.Http` serves a context over a transport-neutral request type.
- `Domain.Codec` derives JSON codecs for commands, events, and rejections, with versioned upcasting.
- `Domain.Stream` is one command's turn on one stream, as a pure function.
- `Domain.Id` holds command ids, stream ids, versions, and sequences.
- `Domain.Labels` holds the label checks behind the compile errors.
