import { equals } from "./assert.js"
import exec, { evalForm } from "./exec.js"

equals(evalForm("42"), 42)
equals(evalForm("[1 2 3]"), [1, 2, 3])
equals(evalForm("fred", { fred: 10 }), 10)

equals(evalForm("(fn [x] (* x x))")(3), 9)

equals(exec({})`(def bob 11)`, { bob: 11 })
