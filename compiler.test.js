import { test } from "node:test"
import assert from "node:assert"
import compile from "./compiler.js"
import * as list from "./list.js"

test("compile", () => {
  assert.deepStrictEqual(compile(null), "null")
  assert.deepStrictEqual(compile(Symbol.for("fred")), `$.resolve($,"fred")`)
  assert.deepStrictEqual(compile(new Number(42)), "42")

  assert.deepStrictEqual(
    compile(list.list(Symbol.for("f"), Symbol.for("x"))),
    `$.apply($.resolve($,"f"),[$.resolve($,"x")])`
  )
})
