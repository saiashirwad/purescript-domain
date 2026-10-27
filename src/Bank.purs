-- | The bank as one bounded context. Each label becomes a runtime handle
-- | (`rt.account.send`, `rt.summary.read`), a stream category, a span name,
-- | and an HTTP route prefix.
module Bank (bank) where

import Bank.Account (account)
import Bank.Customer (customer)
import Bank.Policy (provisioning)
import Bank.Views (dashboard, statement, summary)

-- | No signature: every entry carries its own type, and a record type here
-- | would restate each one.
bank = { account, customer, summary, statement, dashboard, provisioning }
