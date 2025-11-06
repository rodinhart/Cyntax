import * as ubnf from "./ubnf.js"
import grammar from "./cyntax.ubnf.js"
import * as list from "./list.js"

const rules = ubnf.parseGrammar(grammar)

const POS = Symbol()

const withPos = (obj, pos) =>
  obj !== null && typeof obj === "object"
    ? Object.assign(obj, { [POS]: pos })
    : obj

export default (s) => {
  const r = ubnf.exec(rules, s)
  if (r.EXPECTED) {
    throw new Error(ubnf.formatExpected(r, s))
  }

  const dispatch = {
    number: (pos, s) => withPos(new Number(s), pos),

    symbol: (pos, s) => withPos(Symbol.for(s), pos),

    list: (pos, ...items) => withPos(list.list(...items.map(_)), pos),

    array: (pos, ...items) => withPos(items.map(_), pos),

    string: (pos, s) => withPos(new String(s.replace(/\\(.)/g, "$1")), pos),
  }

  const _ = (x) => {
    const fn = dispatch[x[0]]
    if (!fn) {
      throw new Error(`Reader has no dispatch for ${x[0]}`)
    }

    return fn(...x.slice(1))
  }

  return _(r[1][0])
}
