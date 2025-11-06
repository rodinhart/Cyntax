import { equals } from "./assert.js"
import exec, { evalForm } from "./exec.js"

equals(evalForm("42"), 42)
equals(evalForm("[1 2 3]"), [1, 2, 3])
equals(evalForm("fred", { fred: 10 }), 10)
