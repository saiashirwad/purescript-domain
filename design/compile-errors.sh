#!/bin/bash
# Apply one common mistake at a time to the bank, build, print the
# compiler's errors, and restore the file. Usage: design/compile-errors.sh
set -u
cd "$(dirname "$0")/.."
try() {
  local name="$1" file="$2" from="$3" to="$4"
  cp "$file" "$file.bak"
  python3 -c "import sys;p=sys.argv[1];s=open(p).read();a=sys.argv[2];b=sys.argv[3];assert a in s,(p,a);open(p,'w').write(s.replace(a,b,1))" "$file" "$from" "$to"
  echo "=== $name"
  iris test 2>&1 | grep -v '^\s*$' | grep -v 'Reading Spago\|Selecting package\|Downloading dep' | grep -E '•|Error|│' | grep -v '^\s*•\s*$' | head -${LINES_PER:-14}
  mv "$file.bak" "$file"
}
try "missing decide handler" src/Bank/Account.purs \
  "decide: { openAccount, depositMoney, withdrawMoney }" "decide: { openAccount, depositMoney }"
try "misspelled evolve handler" src/Bank/Account.purs \
  "moneyWithdrawn: settle" "moneyWithdrawm: settle"
try "handler rejects with undeclared label" src/Bank/Account.purs \
  "Decision (notOpen :: {}) (moneyDeposited :: Movement)" "Decision (notOpen :: {}, frozen :: {}) (moneyDeposited :: Movement)"
try "emit label not in narrow row" src/Bank/Account.purs \
  'emit @"moneyDeposited" { amount, balanceAfter: balance + amount }' 'emit @"moneyWithdrawn" { amount, balanceAfter: balance + amount }'
try "view handles undeclared event" src/Bank/Views.purs \
  "moneyWithdrawn: \\lines" "moneyWithdrew: \\lines"
try "policy orders unknown command" src/Bank/Policy.purs \
  'order @"openAccount"' 'order @"openAcount"'
try "spec missing a field" src/Bank/Customer.purs \
  "  , reply :: Unit
  }" "  }"
try "context entry of wrong kind" src/Bank.purs \
  "bank = { account, customer, summary, statement, provisioning }" "bank = { account, customer, summary, statement, provisioning: 3 }"
try "narrow payload differs from spec" src/Bank/Account.purs \
  'Decision (alreadyOpen :: {}) (accountOpened :: { owner :: String })
openAccount Unopened { owner } = emit @"accountOpened" { owner }
openAccount (Open _) _ = reject @"alreadyOpen" {}' \
  'Decision (alreadyOpen :: Unit) (accountOpened :: { owner :: String })
openAccount Unopened { owner } = emit @"accountOpened" { owner }
openAccount (Open _) _ = reject @"alreadyOpen" unit'
try "cmd with misspelled label" test/Test/Main.purs \
  'cmd @"depositMoney" { amount: 100 })' 'cmd @"depositMony" { amount: 100 })'
try "cmd with wrong payload" test/Test/Main.purs \
  'cmd @"depositMoney" { amount: 100 })' 'cmd @"depositMoney" { amount: "100" })'
try "event sent as a command" test/Test/Main.purs \
  '(cmd @"openAccount" { owner: "sam" })' '(event @"accountOpened" { owner: "sam" })'
try "missing guard" src/Bank/Api.purs \
  "  , statement: ownsAccount
  }" "  }"
try "view over an aggregate missing from the context" src/Bank.purs \
  "bank = { account, customer, summary, statement, provisioning }" "bank = { customer, summary, statement, provisioning }"
