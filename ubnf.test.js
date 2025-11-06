import { equals } from "./assert.js"
import { exec, formatExpected, parseGrammar } from "./ubnf.js"

const rules = parseGrammar(
  "digit = 0 | 1 | 2\n\ndigits* = digit digits* | digit\n\nint: = digits*"
)

equals(rules, {
  digit: [["0"], ["1"], ["2"]],
  "digits*": [["digit", "digits*"], ["digit"]],
  "int:": [["digits*"]],
})

equals(exec(rules, "21"), [2, [["int", 0, "21"]]])
const r = exec(rules, "3")
equals(r, { EXPECTED: [new Set(["int:"])] })
equals(formatExpected(r, "3"), "Grammar expected 'int:' but found '3' at 0")
