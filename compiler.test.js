import { equals } from "./assert.js"
import compile from "./compiler.js"
import * as list from "./list.js"

equals(compile(null), "null")
equals(compile(Symbol.for("fred")), `$.resolve($,"fred")`)
equals(compile(new Number(42)), "42")

equals(
  compile(list.list(Symbol.for("f"), Symbol.for("x"))),
  `$.apply($.resolve($,"f"),[$.resolve($,"x")])`
)
