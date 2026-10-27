"""Checks that every code block in the README and docs/design.md is real.

- A purescript block is split into chunks at blank lines. Each chunk must
  appear verbatim, as consecutive lines, in one file under src/ or test/.
- A json block must appear in a file under test/.
- A compiler error block must quote the message of a case in
  test/compile-errors.sh, and its source line and caret must match that
  case's mistake at the position the error names.
"""
import pathlib
import re
import shlex
import sys

repo = pathlib.Path(__file__).resolve().parent.parent
docs = [repo / "README.md", repo / "docs" / "design.md"]
sources = {
    p: p.read_text()
    for p in sorted((repo / "src").rglob("*.purs")) + sorted((repo / "test").rglob("*.purs"))
}
script = shlex.split((repo / "test" / "compile-errors.sh").read_text(), comments=True)
script = [token for token in script if token != "\n"]
# Each `expect name file from to fragment` case, keyed by its fragment.
cases = {
    script[i + 5]: {"file": script[i + 2], "from": script[i + 3], "to": script[i + 4]}
    for i, token in enumerate(script)
    if token == "expect"
}
problems = 0


def problem(doc, text):
    global problems
    problems += 1
    print("FAIL %s: %s" % (doc.name, text))


def check_purescript(doc, body):
    for chunk in (c.rstrip("\n") for c in re.split(r"\n\s*\n", body) if c.strip()):
        if not any(chunk + "\n" in text for text in sources.values()):
            problem(doc, "purescript chunk not found in src/ or test/:\n" + chunk)


def check_json(doc, body):
    tests = (text for p, text in sources.items() if (repo / "test") in p.parents)
    if not any(body.strip() in text for text in tests):
        problem(doc, "json not found in test/: " + body.strip())


def check_error(doc, body):
    header = re.search(r"Error! · \[\w+\] · (\S+):(\d+):(\d+)", body)
    message = re.search(r"^  • (\S.*)$", body, re.M)
    code = re.search(r"^  │   (.*)\n  │   ( *)╰", body, re.M)
    if not (header and message and code):
        problem(doc, "unreadable compiler error block:\n" + body)
        return
    path, line, column = header.group(1), int(header.group(2)), int(header.group(3))
    case = cases.get(message.group(1))
    if case is None or case["file"] != path:
        problem(doc, "no case in test/compile-errors.sh for %s: %r" % (path, message.group(1)))
        return
    mistaken = (repo / path).read_text().replace(case["from"], case["to"], 1)
    lines = mistaken.splitlines()
    if line > len(lines) or lines[line - 1] != code.group(1):
        problem(doc, "%s:%d with the mistake applied is not %r" % (path, line, code.group(1)))
    if len(code.group(2)) + 1 != column:
        problem(doc, "the caret under %s:%d is not at column %d" % (path, line, column))


for doc in docs:
    for lang, body in re.findall(r"```(\w*)\n(.*?)```", doc.read_text(), re.S):
        if lang == "purescript":
            check_purescript(doc, body)
        elif lang == "json":
            check_json(doc, body)
        elif "Error! ·" in body:
            check_error(doc, body)

if problems:
    print("%d snippet problem(s)" % problems)
    sys.exit(1)
print("all snippets match")
