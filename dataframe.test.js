import { test } from "node:test"
import assert from "node:assert"
import { create, filter } from "./dataframe.js"

test("filter", () => {
  assert.deepStrictEqual(
    filter((get) => get("x") % 2, create({ x: [1, 2, 3, 4] })),
    { data: { x: [1, 2, 3, 4] }, keys: ["x"], indices: [0, 2] }
  )
})
