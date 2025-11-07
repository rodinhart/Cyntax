import { test } from "node:test"
import assert from "node:assert"
import read, { POS } from "./reader.js"
import * as list from "./list.js"

test("read", () => {
  assert.deepStrictEqual(
    read(" 42"),
    Object.assign(new Number(42), { [POS]: 1 })
  )
  assert.deepStrictEqual(read("def "), Symbol.for("def"))

  assert.deepStrictEqual(read(" ( ) "), list.list())
  assert.deepStrictEqual(
    read(" ( x  2 ( 3) ) "),
    Object.assign(
      list.list(
        Symbol.for("x"),
        Object.assign(new Number(2), { [POS]: 6 }),
        Object.assign(list.list(Object.assign(new Number(3), { [POS]: 10 })), {
          [POS]: 8,
        })
      ),
      { [POS]: 1 }
    )
  )

  assert.deepStrictEqual(
    read("[x y z]"),
    Object.assign([Symbol.for("x"), Symbol.for("y"), Symbol.for("z")], {
      [POS]: 0,
    })
  )

  assert.deepStrictEqual(
    read(`"Hello \\" World"`),
    Object.assign(new String('Hello " World'), { [POS]: 0 })
  )
})
