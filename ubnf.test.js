import { test } from "node:test"
import assert from "node:assert"
import { exec, formatExpected, parseGrammar } from "./ubnf.js"

const rules = parseGrammar(
  "digit = 0 | 1 | 2\n\ndigits* = digit digits* | digit\n\nint: = digits*"
)

test("exec", () => {
  assert.deepStrictEqual(rules, {
    digit: [["0"], ["1"], ["2"]],
    "digits*": [["digit", "digits*"], ["digit"]],
    "int:": [["digits*"]],
  })

  assert.deepStrictEqual(exec(rules, "21"), [2, [["int", 0, "21"]]])
  const r = exec(rules, "3")
  assert.deepStrictEqual(r, { EXPECTED: [new Set(["int:"])] })
})

test("formatExpected", () => {
  const r = exec(rules, "3")

  assert.deepStrictEqual(
    formatExpected(r, "3"),
    "Grammar expected 'int:' but found '3' at 0"
  )
})
