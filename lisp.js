// Type system

// (deftype Symbol [name])
const symbol = (name) => Symbol.for(name)
const name = (symbol) => Symbol.keyFor(symbol)

// (deftype List [car cdr])
function List(car, cdr) {
  if (!(this instanceof List)) return new List(car, cdr)

  this.car = car
  this.cdr = cdr
}
List["Type/invoke"] = ($, method, obj, args) =>
  method({ ...$, car: obj.car, cdr: obj.cdr, ...args })

List.prototype[Symbol.iterator] = function* () {
  let c = this
  while (c) {
    yield c.car
    c = c.cdr
  }
}

const cons = (car, cdr) => List(car, cdr)
const list = (...xs) => [...xs].reverse().reduce((r, x) => List(x, r), null)

// (deftype ArraySeq [arr i])
function ArraySeq(arr, i) {
  if (!(this instanceof ArraySeq)) return new ArraySeq(arr, i)

  this.arr = arr
  this.i = i
}
ArraySeq["Type/invoke"] = ($, method, obj, args) =>
  method({ ...$, arr: obj.arr, i: obj.i, ...args })

// Runtime helpers
const eqal = (a, b) => {
  const ta = a?.constructor?.name ?? "Nil"
  const tb = b?.constructor?.name ?? "Nil"

  if (ta !== tb) {
    return false
  }

  if (typeof a !== "object" && a === b) {
    return true
  }

  if (a === null && b === null) {
    return true
  }

  if (ta === "Object") {
    if (Object.keys(a).length !== Object.keys(b).length) {
      return false
    }

    return Object.entries(a).every(([key, val]) => eqal(val, b[key]))
  }

  if (ta === "Array") {
    if (a.length !== b.length) {
      return false
    }

    return a.every((x, i) => eqal(x, b[i]))
  }

  if (ta === "List") {
    return eqal([...a], [...b])
  }

  return false
}

const resolve = ($, name) => {
  if (!(name in $)) {
    throw Error(`Symbol ${name} is not defined`)
  }

  return $[name]
}

const toFn = (op) => {
  if (typeof op === "function") {
    return op
  }

  if (Array.isArray(op)) {
    return (ix) => {
      // check arity

      if (!Number.isInteger(ix)) {
        throw new Error(`Expected integer array index but found ${ix}`)
      }

      return op[ix < 0 ? op.length + ix : ix] ?? null
    }
  }

  if (op?.constructor === Object) {
    // check arity

    return (key) => op[key] ?? null
  }

  if (op === null) {
    // check arity

    return () => null
  }

  throw new Error(`Expected callable but found ${prn(op)}`)
}

// Compiler
const withType = (dispatch) => (exp) => {
  const type = exp?.constructor?.name ?? "Nil"

  if (type in dispatch) {
    return dispatch[type](exp)
  }

  if (exp?.constructor?.["Seq/first"]) {
    if ("Seq" in dispatch) {
      return dispatch["Seq"](exp)
    }

    throw new Error(`Unknown dispatch for Seq (${type})`)
  }

  if ("?" in dispatch) {
    return dispatch["?"](exp)
  }

  throw new Error(`Unknown dispatch for ${type}`)
}

export const genCode = withType({
  Nil: (exp) => "null",
  Boolean: (exp) => String(exp),
  Symbol: (exp) => `$.resolve($, ${JSON.stringify(name(exp))})`,
  Number: (exp) => String(exp),
  String: (exp) => JSON.stringify(exp),
  List: (exp) => {
    const [op, ...rands] = [...exp]

    if (op === symbol("def")) {
      // (def name val)
      const [n, val] = rands
      const symbolName = JSON.stringify(name(n))

      return `($[${symbolName}] = ${genCode(
        val ?? null
      )}, Symbol.for(${symbolName}))`
    }

    if (op === symbol("defprotocol")) {
      // (defprotocol name (method [param param]))
      const [pn, ...fns] = rands
      const protocolName = name(pn)

      const compiledMethods = fns.map((fn) => {
        const [mn] = fn
        const methodName = name(mn)

        return `$[${JSON.stringify(methodName)}] = (obj, ...args) => {
          const name = obj?.constructor?.name ?? "Nil"
          const type = $.resolve($, name)
          const fn = type["${protocolName}/${methodName}"] 
          
          if (typeof fn !== "function") {
            throw new Error(\`Type \${name} is missing implementation for ${protocolName}/${methodName}\`)
          }
          
          return fn(obj, ...args)
        }`
      })

      return `(${compiledMethods.join(",")}, Symbol.for(${JSON.stringify(
        protocolName
      )}))`
    }

    if (op === symbol("deftype")) {
      // (deftype Name [field field])
      const [tn, params] = rands

      const typeName = name(tn)

      // only works with javascript compatible names
      // could make them pojos?  const ArraySeq = (arr, i) => ({ ArraySeq: {arr, i} })
      return `($[${JSON.stringify(typeName)}] = function ${typeName}(${params
        .map(name)
        .join(",")}) {
        if (!(this instanceof ${name(tn)})) return new ${typeName}(${params
        .map(name)
        .join(", ")})
        ${params
          .map((param) => `this.${name(param)} = ${name(param)}`)
          .join("\n")}
      }, $[${JSON.stringify(
        typeName
      )}]["Type/invoke"] = ($, method, obj, args) =>
         method({
          ...$
          ${params
            .map(
              (param) =>
                `, [${JSON.stringify(name(param))}]: obj[${JSON.stringify(
                  name(param)
                )}]`
            )
            .join("")}
          , ...args}), Symbol.for(${JSON.stringify(typeName)}))`
    }

    if (op === symbol("extend")) {
      // (extend type protocol (method [param param] body))
      const [typeName, protocol, ...methods] = rands

      const type = genCode(typeName)

      const addMethods = methods.map((method) => {
        const [methodName, p, b] = [...method]

        const namespaced = JSON.stringify(
          `${name(protocol)}/${name(methodName)}`
        )

        const body = genCode(b)

        return `${type}[${namespaced}] = (obj, ...args) => ${type}["Type/invoke"](
          $,
          ($) => ${body},
          obj,
          {${p
            .map(
              (param, i) =>
                `[${JSON.stringify(name(param))}]: args[${i}] ?? null`
            )
            .join(",")}})`
      })

      return `(${addMethods.join(",")}, null)`
    }

    if (op === symbol("fn")) {
      // (fn [param param] body)
      if (Array.isArray(rands[0])) {
        // (fn [x y] (+ x y))
        return genCode(list(symbol("fn"), exp.cdr))
      } else {
        // (fn ([x] x) ([x y] (+ x y)))
        const [...overloads] = rands

        const arities = overloads.map((overload) => {
          const [p, b] = overload

          const args = []
          for (let i = 0; i < p.length; i++) {
            if (p[i] !== symbol("&")) {
              args.push(`,${JSON.stringify(name(p[i]))}:args[${i}]`)
            } else {
              args.push(`,${JSON.stringify(name(p[i + 1]))}:args.slice(${i})`)
              i++
            }
          }

          const body = genCode(b)

          return `
          ${!p.includes(symbol("&")) ? `case ${p.length}:` : "default:"}
            return (($) => ${body})({...$${args.join("")}})
          `
        })

        const arityError = "Arity ${args.length} not supported."

        return `((...args) => {
          switch (args.length) {
            ${arities.join("\n")}
          }

          throw new Error(\`${arityError}\`)
        })`
      }
    }

    if (op === symbol("if")) {
      // (if pred cons alt)
      const [pred, cons, alt] = rands

      return `((__) => __ !== false && __ !== null ? ${genCode(
        cons
      )} : ${genCode(alt ?? null)})(${genCode(pred)})`
    }

    if (op === symbol("let")) {
      const [bindings, body] = rands
      const pairs = bindings.reduce(
        (r, x, i) => (i % 2 ? r[r.length - 1].push(x) : r.push([x]), r),
        []
      )

      return `(($) => (
        ${pairs
          .map(
            ([letName, exp]) =>
              `$[${JSON.stringify(name(letName))}] = ${genCode(exp)}`
          )
          .join(",")},
        ${genCode(body)}
      ))({...$})`
    }

    if (op === symbol("loop")) {
      // (loop [name val name val] (recur newVal newVal))
      const [bindings, body] = rands

      // is there a nicer way?
      const pairs = bindings.reduce(
        (r, x, i) => (i % 2 ? r[r.length - 1].push(x) : r.push([x]), r),
        []
      )

      return `(($) => {
        $["recur"] = (...args) => {
          ${pairs
            .map(
              ([loopName], i) =>
                `$[${JSON.stringify(name(loopName))}] = args[${i}]`
            )
            .join("\n")}

          return "RECUR"
        }
        $["recur"](${pairs.map(([, init]) => genCode(init)).join(", ")})
        let r
        do {
          r = ${genCode(body)}
        } while (r === "RECUR")

        return r
      })({...$})`
    }

    if (op === symbol("macro")) {
      // (macro [param param] body)
      return `Object.assign(${genCode(
        cons(symbol("fn"), exp.cdr)
      )}, {macro:true})`
    }

    if (op === symbol("quote")) {
      // (quote exp)
      const quote = withType({
        Nil: (exp) => "null",
        Boolean: (exp) => String(exp),
        Symbol: (exp) => `Symbol.for(${JSON.stringify(name(exp))})`,
        Number: (exp) => String(exp),
        String: (exp) => JSON.stringify(exp),
        List: (exp) => {
          if (exp.car === symbol("unquote")) {
            return genCode(exp.cdr.car)
          } else if (exp.car === symbol("unquote-splice")) {
            return `...${genCode(exp.cdr.car)}`
          }

          return `$["list"](${[...exp].map(quote).join(",")})`
        },
        Array: (exp) => `[${exp.map(quote).join(",")}]`,
        Object: (exp) => `({ hashmap: ${quote(exp.hashmap)} })`,
      })

      return quote(rands[0])
    }

    return `$.toFn(${genCode(op)})(${rands
      .map((rand) => genCode(rand))
      .join(",")})`
  },
  Array: (exp) => `[${exp.map(genCode).join(",")}]`,
  Object: (exp) =>
    `({ ${exp.hashmap
      .map(([key, val]) => `[${genCode(key)}]: ${genCode(val)}`)
      .join(", ")} })`,
})

export const macroExpand = ($) =>
  withType({
    Nil: (exp) => exp,
    Boolean: (exp) => exp,
    Symbol: (exp) => exp,
    Number: (exp) => exp,
    String: (exp) => exp,
    List: (exp) => {
      const [op, ...rands] = exp
      if (typeof op === "symbol" && $[name(op)]?.macro === true) {
        return macroExpand($)($[name(op)](...rands))
      }

      if (op === symbol("quote")) {
        return exp
      }

      return list(macroExpand($)(op), ...rands.map(macroExpand($)))
    },
    Array: (exp) => exp.map(macroExpand($)),
    Object: (exp) => ({
      hashmap: exp.hashmap.map(([key, val]) => [
        macroExpand($)(key),
        macroExpand($)(val),
      ]),
    }),
  })

export const prn = withType({
  Nil: (exp) => "nil",
  Boolean: (exp) => String(exp),
  Symbol: (exp) => name(exp),
  Number: (exp) => String(exp),
  String: (exp) => JSON.stringify(exp),
  List: (exp) => `(${[...exp].map((x) => prn(x)).join(" ")})`,
  Array: (exp) => `[${exp.map((x) => prn(x)).join(" ")}]`,
  Seq: (exp) => {
    const r = []
    let c = exp
    while (c) {
      r.push(c.constructor["Seq/first"](c))
      c = c.constructor["Seq/rest"](c)
    }

    return `(${r.map(prn).join(" ")})`
  },
  Object: (exp) =>
    `{ ${(exp.hashmap ?? Object.entries(exp))
      .map(([key, val]) => `${prn(key)} ${prn(val)}`)
      .join(", ")} }`,
  "?": (exp) => `[type ${exp?.constructor?.name}]`,
})

export const read = (input) => {
  const _ = (xs) => {
    const x = xs.shift()

    if (x === "nil") {
      return null
    }

    if (x === "false") {
      return false
    }

    if (x === "true") {
      return true
    }

    if (String(Number(x)) === x) {
      return Number(x)
    }

    if (x[0] === '"') {
      return x.substring(1, x.length - 1)
    }

    if (x === "'") {
      return list(symbol("quote"), _(xs))
    }

    if (x === "~") {
      if (xs[0] !== "@") {
        return list(symbol("unquote"), _(xs))
      } else {
        xs.shift()

        return list(symbol("unquote-splice"), _(xs))
      }
    }

    if (x === "(") {
      const arr = []
      while (xs.length && xs[0] !== ")") {
        arr.push(_(xs))
      }

      if (xs.shift() !== ")") {
        throw new Error(`Expected closing )`)
      }

      return list(...arr)
    }

    if (x === "[") {
      const arr = []
      while (xs.length && xs[0] !== "]") {
        arr.push(_(xs))
      }

      if (xs.shift() !== "]") {
        throw new Error(`Expected closing ]`)
      }

      return arr
    }

    if (x === "{") {
      const hashmap = []
      while (xs.length > 1 && xs[0] !== "}") {
        hashmap.push([_(xs), _(xs)])
      }

      if (xs.shift() !== "}") {
        throw new Error(`Expected closing }`)
      }

      return {
        hashmap,
      }
    }

    return symbol(x)
  }

  return _(
    input
      .replace(/;[^\n]+\n/g, "")
      .replace(/([()\[\]\{\}'~@])/g, " $1 ")
      .trim()
      .split(/[\s,]+/)
  )
}

// native operations
export const native = {
  "+": (...xs) =>
    xs.reduce((r, x) => {
      if (!Number.isFinite(x)) {
        throw new Error(`+ expected number but found ${prn(x)}`)
      }

      return r + x
    }, 0),
  "-": (a, b) => a - b,
  "*": (...xs) => xs.reduce((r, x) => r * x, 1),
  "/": (a, b) => a / b,
  "%": (a, b) => a % b,
  "=": (a, b) => eqal(a, b),
  "<": (a, b) => a < b,
  ">": (a, b) => a > b,

  Array: {
    // (deftype Array [arr])
    "Type/invoke": ($, method, obj, args) =>
      method({ ...$, arr: obj, ...args }),
    // (extend Array Coll ...)
    "Coll/conj": (arr, item) => [...arr, item],
    "Coll/count": (arr) => arr.length,
    "Coll/seq": (arr) => ArraySeq(arr, 0),
  },
  ArraySeq,
  assoc: (map, ...xs) => {
    const result = { ...map }
    for (let i = 0; i + 1 < xs.length; i += 2) {
      result[xs[i]] = xs[i + 1]
    }

    return result
  },
  car: (exp) => exp.car,
  cdr: (exp) => exp.cdr,
  cons,
  "contains?": (map, ...keys) => keys.every((key) => key in map),
  dissoc: (map, ...keys) => {
    const set = new Set(keys)
    const result = {}
    for (const [key, val] of Object.entries(map)) {
      if (!set.has(key)) {
        result[key] = val
      }
    }

    return result
  },
  keys: (map) => Object.keys(map),
  List,
  list,
  "list?": (x) => x instanceof List,
  log: console.log,
  max: (...xs) => Math.max(...xs),
  not: (x) => x === false || x === null,
  Object: {
    // (deftype Object [obj])
    "Type/invoke": ($, method, obj, args) => method({ ...$, obj, ...args }),
    // (extend Object Coll ...)
    "Coll/conj": (obj, item) => ({ ...obj, [item[0]]: item[1] }),
    "Coll/count": (obj) => Object.keys(obj).length,
    "Coll/seq": (obj) => ArraySeq(Object.entries(obj), 0),
  },
  resolve,
  "ROUND*": (n, x) => Math.round(x * 10 ** n) / 10 ** n,
  slice: (arr, start) => arr.slice(start),
  String: {
    // (deftype String [s])
    "Type/invoke": ($, method, obj, args) => method({ ...$, s: obj, ...args }),
    // (extend String Coll ...)
    "Coll/conj": (s, item) => s + item,
    "Coll/count": (s) => s.length,
    "Coll/seq": (s) => new ArraySeq(s.split(""), 0),
  },
  string: (x) => (typeof x === "symbol" ? name(x) : ""),
  "symbol?": (x) => typeof x === "symbol",
  "test*": (description, actual, expected) => {
    if (!native["="](actual, expected)) {
      throw new Error(
        `Test ${prn(description)} failed: expected ${prn(
          expected
        )} but found ${prn(actual)}.`
      )
    }
  },
  toFn,
  "upper-case": (s) => s.toUpperCase(),
  vector: (...coll) => coll,
}

const debug = (...args) => {
  // console.log(...args)
}

export const lisp =
  ($) =>
  (strings, ...exps) => {
    const code = read(`[${strings.join("")}]`)
    debug("code", prn(code))

    const $2 = { ...$ }
    code.forEach((form) => {
      debug("---")
      debug("form", prn(form))

      const expanded = macroExpand($2)(form)
      debug("expanded", prn(expanded))

      const javascript = genCode(expanded)
      debug("javascript", javascript)

      const compiled = eval(`(($) => ${javascript})`)

      compiled($2)
    })

    return Object.fromEntries(Object.entries($2).filter(([key]) => !(key in $)))
  }
