#!/bin/bash
# Applies one common mistake at a time and asserts the compiler's message.
# An `expect` case names a file, the text to replace, the replacement, and
# a fragment the compiler output must contain. Files are restored on exit,
# even on failure.
# Exit 0 only if every case matches.
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

# Asserts that the last build failed with `fragment` in its output.
check() {
  local name="$1" fragment="$2"
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

expect() {
  local name="$1" file="$2" from="$3" to="$4" fragment="$5"
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
  check "$name" "$fragment"
}

if ! iris build >/dev/null 2>&1; then
  echo "FAIL baseline: the project does not build without any mistake"
  exit 1
fi

# A missing case is the compiler's own exhaustiveness error, which names
# the missing constructor. Each handler is a `case`, so these cover every
# place a new command or event needs a decision.
expect "decide misses a case" src/Bank/Account.purs \
  "  OpenAccount _, Open _ -> Left AlreadyOpen
" "" \
  "Pattern match is not exhaustive. Missing: OpenAccount ({ owner: _ }), Open _"
expect "evolve misses an event" src/Bank/Account.purs \
  "  MoneyWithdrawn movement -> settle movement
" "" \
  "Pattern match is not exhaustive. Missing: MoneyWithdrawn _"
expect "view misses an event" src/Bank/Views.purs \
  '  MoneyWithdrawn { amount, balanceAfter } -> [ { kind: "withdrawal", amount, balance: balanceAfter } ]
' "" \
  "Pattern match is not exhaustive. Missing: MoneyWithdrawn _"
expect "guard misses a command" src/Bank/Api.purs \
  "      WithdrawMoney _ -> ownsAccount principal id
" "" \
  "Pattern match is not exhaustive. Missing: WithdrawMoney _"
expect "policy misses an event" src/Bank/Customer.purs \
  "data CustomerEvent = CustomerRegistered { name :: String }" "data CustomerEvent = CustomerRegistered { name :: String } | CustomerRenamed { name :: String }" \
  "Pattern match is not exhaustive. Missing: CustomerRenamed _"
expect "context entry of wrong kind" src/Bank.purs \
  "bank = { account, customer, summary, statement, dashboard, provisioning }" "bank = { account, customer, summary, statement, dashboard, provisioning: 3 }" \
  "a context entry must be an Aggregate, a View, or a Policy, but provisioning is Int"
expect "policy targets an aggregate missing from the context" src/Bank.purs \
  "bank = { account, customer, summary, statement, dashboard, provisioning }" "bank = { customer, provisioning }" \
  "a policy orders commands of an aggregate this context does not contain:"
expect "view over an aggregate missing from the context" src/Bank.purs \
  "bank = { account, customer, summary, statement, dashboard, provisioning }" "bank = { customer, summary }" \
  "a view or policy reads events of an aggregate this context does not contain:"
expect "missing guard" src/Bank/Api.purs \
  "  , statement: ownsAccount
" "" \
  "serve needs a guard for statement"
expect "guard for an unknown label" src/Bank/Api.purs \
  "  , statement: ownsAccount" "  , statment: ownsAccount
  , statement: ownsAccount" \
  "a guard names no aggregate or view of the context: statment"
expect "guard for a policy" src/Bank/Api.purs \
  "  , statement: ownsAccount" "  , provisioning: ownsAccount
  , statement: ownsAccount" \
  "a guard names no aggregate or view of the context: provisioning"
expect "constructor with two arguments" src/Bank/Customer.purs \
  "data CustomerRejection = AlreadyRegistered" "data CustomerRejection = AlreadyRegistered | Renamed String String" \
  "a labelled constructor takes at most one argument; put its fields in a record"

# KNOWN: plain unification errors. The span is the enclosing signature,
# not the call.
expect "event sent as a command (KNOWN: plain unification error)" test/Test/Runtime.purs \
  '(OpenAccount { owner: CustomerId "sam" })' '(AccountOpened { owner: CustomerId "sam" })' \
  "Cannot unify 'AccountEvent' with 'AccountCommand'"
expect "view keyed by the wrong type (KNOWN: plain unification error)" src/Bank/Views.purs \
  "dashboard :: View AccountChange CustomerId Dashboard" "dashboard :: View AccountChange AccountId Dashboard" \
  "Cannot unify 'CustomerId' with 'AccountId'"

if [ "$failures" -ne 0 ]; then
  echo "$failures case(s) failed"
  exit 1
fi
echo "all compile-error cases match"
