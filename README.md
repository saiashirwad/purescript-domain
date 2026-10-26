# purescript-domain

A prototype of a Domain-style library for PureScript. It models the bank from Alchemy's Domain tutorial: accounts, deposits, withdrawals, summaries, statements, customers, and a provisioning policy.

`DESIGN.md` explains the design, the prior art, and the parts Alchemy has that this prototype does not.

## Run

With `purs`:

```sh
spago test
```

With Iris:

```sh
iris test
```

Do not run both compilers against the same `output` directory at the same time. Iris and `purs` generate different JavaScript, and mixing the two trees produces confusing runtime errors. Delete `output` before switching compilers.

## Layout

- `src/Domain.purs`: the kernel. `Decider`, `Step`, `View`, `Policy`, `Process`, `Envelope`, `Receipt`, `commit`, `applyView`.
- `src/Domain/Story.purs`: the given/when/then runner.
- `src/Bank/Account.purs`: the account aggregate. Commands, events, and rejections are `Variant` rows. Each handler names the narrow row it can produce, and `expand` embeds it in the aggregate's row.
- `src/Bank/Views.purs`: the summary and statement views.
- `src/Bank/Customer.purs`: the customer aggregate.
- `src/Bank/Policy.purs`: the main-account provisioning policy.
- `src/Bank/Model.purs`: the in-memory world. It commits commands, appends envelopes, applies views, and runs policies.
- `test/Test/Main.purs`: the Alchemy tutorial stories plus the replay law.

## What is not here

The prototype has no durable store, no command receipts, no per-key queues, no serialization, and no API. Those are the next pieces, and they are where the production guarantees live. See the end of `DESIGN.md`.
