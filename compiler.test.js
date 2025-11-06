import { equals } from "./assert.js"
import compile from "./compiler.js"

equals(compile(null), "null")
equals(compile(Symbol.for("fred")), `$.resolve($, "fred")`)
equals(compile(new Number(42)), "42")
