#!/bin/bash
# Applies one common mistake at a time and asserts the compiler's message.
# Each case names a file, the text to replace, the replacement, and a
# fragment the compiler output must contain. Files are restored on exit,
# even on failure. Exit 0 only if every case matches.
#
# A case marked KNOWN pins an error that is not yet a one-line custom
# message. If its text changes, the case fails: that is a regression or an
# improvement, and the fragment should be updated deliberately.
set -u
cd "$(dirname "$0")/.."

backup="$(mktemp -d)"
touched=()
restore() {
  local f
  for f in ${touched[@]+"${touched[@]}"}; do
    cp "$backup/$(echo "$f" | tr '/' '_')" "$f"
  done
  rm -rf "$backup"
}
trap restore EXIT

failures=0

mutate() {
  python3 - "$1" "$2" "$3" <<'PY'
import sys
path, old, new = sys.argv[1:4]
text = open(path).read()
if old not in text:
    sys.exit("compile-errors.sh: %r not found in %s" % (old, path))
open(path, "w").write(text.replace(old, new, 1))
PY
}

expect() {
  local name="$1" file="$2" from="$3" to="$4" fragment="$5" output status
  touched+=("$file")
  cp "$file" "$backup/$(echo "$file" | tr '/' '_')"
  if ! mutate "$file" "$from" "$to"; then
    echo "FAIL $name: could not apply the mistake"
    failures=$((failures + 1))
    return
  fi
  output="$(iris build 2>&1)"
  status=$?
  cp "$backup/$(echo "$file" | tr '/' '_')" "$file"
  if [ "$status" -eq 0 ]; then
    echo "FAIL $name: the mistake compiled"
    failures=$((failures + 1))
  elif ! grep -qF -- "$fragment" <<<"$output"; then
    echo "FAIL $name: expected the error to contain: $fragment"
    echo "$output" | grep -E '•|Error' | head -12 | sed 's/^/    /'
    failures=$((failures + 1))
  else
    echo "ok   $name"
  fi
}

if ! iris build >/dev/null 2>&1; then
  echo "FAIL baseline: the project does not build without any mistake"
  exit 1
fi

expect "missing decide handler" src/Bank/Account.purs \
  "decide: { openAccount, depositMoney, withdrawMoney }" "decide: { openAccount, depositMoney }" \
  "decide is missing a handler for the command withdrawMoney"
expect "misspelled evolve handler" src/Bank/Account.purs \
  "moneyWithdrawn: settle" "moneyWithdrawm: settle" \
  "evolve handles an undeclared event moneyWithdrawm"
expect "handler rejects with undeclared label" src/Bank/Account.purs \
  "Decision (notOpen :: {}) (moneyDeposited :: Movement)" "Decision (notOpen :: {}, frozen :: {}) (moneyDeposited :: Movement)" \
  "a decide handler rejects with an undeclared rejection frozen"
expect "emit label not in narrow row" src/Bank/Account.purs \
  'emit @"moneyDeposited" { amount, balanceAfter: balance + amount }' 'emit @"moneyWithdrawn" { amount, balanceAfter: balance + amount }' \
  "the handler's event row does not include moneyWithdrawn"
expect "view handles undeclared event" src/Bank/Views.purs \
  "moneyWithdrawn: \\lines" "moneyWithdrew: \\lines" \
  "a view handles an undeclared event moneyWithdrew"
expect "keyed view handles undeclared event" src/Bank/Views.purs \
  "moneyDeposited: \\row" "moneyDepositd: \\row" \
  "a view handles an undeclared event moneyDepositd"
expect "keyed view without a key" src/Bank/Views.purs \
  "  , key: customerOf
" "" \
  "a view with rows keyed by CustomerId needs key :: AccountState -> Maybe CustomerId"
expect "view key of the wrong type" src/Bank/Views.purs \
  "dashboard :: View Account CustomerId Dashboard" "dashboard :: View Account AccountId Dashboard" \
  "but its key is AccountState -> Maybe CustomerId"
expect "misspelled view key field" src/Bank/Views.purs \
  "key: customerOf" "kye: customerOf" \
  "a view definition has the fields initial and on, and optionally key"
expect "policy orders unknown command" src/Bank/Policy.purs \
  'order @"openAccount"' 'order @"openAcount"' \
  "the target aggregate declares no command openAcount"
expect "spec missing a field" src/Bank/Customer.purs \
  "  , reply :: Unit
  }" "  }" \
  "an aggregate spec needs the field reply"
expect "context entry of wrong kind" src/Bank.purs \
  "bank = { account, customer, summary, statement, dashboard, provisioning }" "bank = { account, customer, summary, statement, dashboard, provisioning: 3 }" \
  "a context entry must be an Aggregate, a View, or a Policy, but provisioning is Int"
expect "narrow payload differs from spec" src/Bank/Account.purs \
  'Decision (alreadyOpen :: {}) (accountOpened :: { owner :: CustomerId })
openAccount Unopened { owner } = emit @"accountOpened" { owner }
openAccount (Open _) _ = reject @"alreadyOpen" {}' \
  'Decision (alreadyOpen :: Unit) (accountOpened :: { owner :: CustomerId })
openAccount Unopened { owner } = emit @"accountOpened" { owner }
openAccount (Open _) _ = reject @"alreadyOpen" unit' \
  "is Unit here but {} in the spec"
expect "cmd with misspelled label" test/Test/Runtime.purs \
  'cmd @"depositMoney" { amount: 100 })' 'cmd @"depositMony" { amount: 100 })' \
  "the aggregate declares no command depositMony"
expect "event sent as a command" test/Test/Runtime.purs \
  '(cmd @"openAccount" { owner: CustomerId "sam" })' '(event @"accountOpened" { owner: CustomerId "sam" })' \
  "Cannot unify 'Command' with 'Event'"
expect "missing guard" src/Bank/Api.purs \
  "  , statement: ownsAccount
" "" \
  "Missing required properties: statement"

# The erased representations are sound only while user code cannot move a
# value between specs. See "Soundness" in design/RATIONALE.md.
expect "forge an event through its constructor" test/Test/Stream.purs \
  "import Test.Support (assertEqual, commandId, epoch, fail)
" "import Test.Support (assertEqual, commandId, epoch, fail)
import Bank.Customer (Customer)
import Domain (Event(..), event)

forged :: Event Account
forged = case (event @\"customerRegistered\" { name: \"ada\" } :: Event Customer) of
  Event v -> Event v
" \
  "'Event' is not in scope"
expect "coerce an event across specs" test/Test/Stream.purs \
  "import Test.Support (assertEqual, commandId, epoch, fail)
" "import Test.Support (assertEqual, commandId, epoch, fail)
import Bank.Customer (Customer)
import Domain (Event, event)
import Safe.Coerce (coerce)

forged :: Event Account
forged = coerce (event @\"customerRegistered\" { name: \"ada\" } :: Event Customer)
" \
  "matching 'Customer' with 'Account'"
expect "event from another spec's row" test/Test/Stream.purs \
  "import Test.Support (assertEqual, commandId, epoch, fail)
" "import Test.Support (assertEqual, commandId, epoch, fail)
import Data.Variant (inj)
import Domain (Event, fromVariant)
import Type.Proxy (Proxy(..))

forged :: Event Account
forged = fromVariant (inj (Proxy :: Proxy \"customerRegistered\") { name: \"ada\" })
" \
  "Cannot unify '( customerRegistered :: { name :: String }"

# KNOWN: not a custom error. The span is the enclosing signature, not the
# call, and the message names types rather than the label.
expect "cmd with wrong payload (KNOWN: plain unification error)" test/Test/Runtime.purs \
  'cmd @"depositMoney" { amount: 100 })' 'cmd @"depositMoney" { amount: "100" })' \
  "matching '{ amount :: String }' with '{ amount :: Int }'"

# KNOWN: a custom error, but it prints the whole aggregate spec after the
# header instead of naming the aggregate.
expect "view over an aggregate missing from the context (KNOWN: spec dump)" src/Bank.purs \
  "bank = { account, customer, summary, statement, dashboard, provisioning }" "bank = { customer, summary, statement, dashboard, provisioning }" \
  "a view or policy reads an aggregate this context does not contain:"
expect "view over a missing aggregate prints the spec (KNOWN: spec dump)" src/Bank.purs \
  "bank = { account, customer, summary, statement, dashboard, provisioning }" "bank = { customer, summary, statement, dashboard, provisioning }" \
  "state :: AccountState"

if [ "$failures" -ne 0 ]; then
  echo "$failures case(s) failed"
  exit 1
fi
echo "all compile-error cases match"
