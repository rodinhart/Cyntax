import read from "./reader.js"
import { equals } from "./assert.js"
import * as list from "./list.js"

equals(read(" 42"), new Number(42))
equals(read("def "), Symbol.for("def"))

equals(read(" ( ) "), list.list())
equals(
  read(" ( x  2 ( 3) ) "),
  list.list(Symbol.for("x"), new Number(2), list.list(new Number(3)))
)

equals(read("[x y z]"), [Symbol.for("x"), Symbol.for("y"), Symbol.for("z")])

equals(read(`"Hello \\" World"`), new String('Hello " World'))
