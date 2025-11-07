import { test } from "node:test"
import assert from "node:assert"
import exec, { evalForm } from "./exec.js"

test("number", () => {})

test("evalForm", () => {
  assert.deepStrictEqual(evalForm("42"), 42)
  assert.deepStrictEqual(evalForm("[1 2 3]"), [1, 2, 3])
  assert.deepStrictEqual(evalForm("[1 2 3]"), [1, 2, 3])
  assert.deepStrictEqual(evalForm("fred", { fred: 10 }), 10)
  assert.deepStrictEqual(evalForm("(fn [x] (* x x))")(3), 9)
})

test("exec", () => {
  assert.deepStrictEqual(exec({})`(def bob 11)`, { bob: 11 })
})
