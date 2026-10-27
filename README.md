# purescript-domain

A domain modeling library for PureScript, built with Iris. You declare each command, event, and rejection label once, in a spec record type. The library derives the rest from that spec: smart constructors, the runtime, span names, JSON codecs, and HTTP routes.

`design/RATIONALE.md` explains the design and its tradeoffs.

## Run

```sh
iris test
```

## Usage

Declare an aggregate's spec once:

```purescript
type Account =
  { id :: AccountId
  , state :: AccountState
  , command :: Variant (openAccount :: { owner :: String }, depositMoney :: { amount :: Int }, ...)
  , event :: Variant (accountOpened :: { owner :: String }, moneyDeposited :: Movement, ...)
  , rejection :: Variant (alreadyOpen :: {}, notOpen :: {}, ...)
  , reply :: { balance :: Int }
  }

account :: Aggregate Account
account = aggregate
  { initial: Unopened
  , decide: { openAccount, depositMoney, withdrawMoney }
  , evolve: { accountOpened: \_ { owner } -> Open { owner, balance: 0 }, ... }
  , reply: \state -> { balance: balanceOf state }
  }

depositMoney :: AccountState -> { amount :: Int } -> Decision (notOpen :: {}) (moneyDeposited :: Movement)
depositMoney Unopened _ = reject @"notOpen" {}
depositMoney (Open { balance }) { amount } = emit @"moneyDeposited" { amount, balanceAfter: balance + amount }
```

Put aggregates, views, and policies in one record. Each label becomes a handle:

```purescript
bank = { account, customer, summary, statement, provisioning }

rt <- start bank defaults
receipt <- rt.account.send commandId accountId (cmd @"depositMoney" { amount: 100 })
summary <- rt.summary.read accountId
response <- serve bank guards rt principal request
rt.stop
```

A missing handler or a misspelled label fails to compile with one error that names the label:

```
decide is missing a handler for the command withdrawMoney
```

## Layout

- `src/Domain.purs` is the pure kernel: specs, labelled values, aggregates, views, and policies.
- `src/Domain/Runtime.purs` runs a context. `src/Domain/Runtime/Serial.purs` is the per-stream executor it uses.
- `src/Domain/Story.purs` runs given/when/then stories.
- `src/Domain/Codec.purs` and `src/Domain/Http.purs` derive the wire format and the HTTP boundary.
- `src/Domain/Labels.purs` holds the label checks behind the compile errors.
- `src/Bank.purs` and `src/Bank/*.purs` are the example bank.
