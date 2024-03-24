// Type system

// (defprotocol Type
//   (name [])
//   (invoke [protocol method args]))
const name = (symbol) => Symbol.keyFor(symbol)

const symbol = (name) => Symbol.for(name)

// (deftype List [car cdr])
function List(car, cdr) {
  if (!(this instanceof List)) return new List(car, cdr)

  this.car = car
  this.cdr = cdr
}
List.prototype.toPojo = function () {
  return {
    car: this.car,
    cdr: this.cdr,
  }
}

List.prototype[Symbol.iterator] = function* () {
  let c = this
  while (c) {
    yield c.car
    c = c.cdr
  }
}

const cons = (car, cdr) => new List(car, cdr)

// (deftype ArraySeq [arr i])
function ArraySeq(arr, i) {
  if (!(this instanceof ArraySeq)) return new ArraySeq(arr, i)

  this.arr = arr
  this.i = i
}
ArraySeq.prototype.toPojo = function () {
  return {
    arr: this.arr,
    i: this.i,
  }
}

const getType = (exp) =>
  exp === null
    ? "null"
    : typeof exp === "boolean"
    ? "boolean"
    : typeof exp === "symbol"
    ? "symbol"
    : typeof exp === "number"
    ? "number"
    : typeof exp === "string"
    ? "string"
    : exp instanceof List
    ? "List"
    : Array.isArray(exp)
    ? "Array"
    : exp?.constructor?.["Seq/first"]
    ? "Seq"
    : exp?.constructor === Object || exp?.hashmap
    ? "Hashmap"
    : "unknown"

const list = (...xs) => [...xs].reverse().reduce((r, x) => cons(x, r), null)

// Runtime helpers
const invoke = (type, protocol, method, obj, args) => {
  try {
    return type[`${name(protocol)}/${name(method)}`](obj, ...args)
  } catch (e) {
    console.log(type, name(protocol), name(method))
    throw e
  }
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
      if (!Number.isInteger(ix)) {
        throw new Error(`Expected integer array index but found ${ix}`)
      }

      return op[ix < 0 ? op.length + ix : ix]
    }
  }

  if (op?.constructor === Object) {
    return (key) => op[key]
  }

  throw new Error(`Expected callable but found ${op}`)
}

// Compiler
const withType = (dispatch) => (exp) => {
  const type = getType(exp)
  if (!(type in dispatch)) {
    throw new Error(`Unknown dispatch for ${type}`)
  }

  return dispatch[type](exp)
}

export const genCode = withType({
  null: (exp) => "null",
  boolean: (exp) => String(exp),
  symbol: (exp) => `$.resolve($, ${JSON.stringify(name(exp))})`,
  number: (exp) => String(exp),
  string: (exp) => JSON.stringify(exp),
  List: (exp) => {
    const [op, ...rands] = [...exp]

    if (op === symbol("def")) {
      const [n, val] = rands
      const targetName = JSON.stringify(name(n))

      return `($[${targetName}] = ${genCode(
        val ?? null
      )},Symbol.for(${targetName}))`
    }

    if (op === symbol("defprotocol")) {
      const [pn, ...fns] = rands
      const protocolName = JSON.stringify(name(pn))

      const compiledMethods = fns.map((fn) => {
        const [method] = fn
        const compiledName = name(method)

        return `$[${JSON.stringify(
          compiledName
        )}]=(obj, ...args) => $.invoke($[obj?.constructor?.name ?? "Nil"] ?? obj, Symbol.for(${protocolName}), Symbol.for(${JSON.stringify(
          name(method)
        )}), obj, args)`
      })

      // use compile (quote name)
      return `(${compiledMethods.join(",")},Symbol.for(${protocolName}))`
    }

    if (op === symbol("deftype")) {
      const [typeName, params] = rands

      // only works with javascript compatible names
      // could make them pojos?  const ArraySeq = (arr, i) => ({ ArraySeq: {arr, i} })
      return `($[${JSON.stringify(name(typeName))}] = function ${name(
        typeName
      )}(${params.map(name).join(",")}) {
        if (!(this instanceof ${name(typeName)})) return new ${name(
        typeName
      )}(${params.map(name).join(", ")})
        ${params
          .map((param) => `this.${name(param)} = ${name(param)}`)
          .join("\n")}
      }, $[${JSON.stringify(name(typeName))}].prototype.toPojo = function() {
        return {${params
          .map((param) => `${name(param)}: this.${name(param)}`)
          .join(",")}}
      }, Symbol.for(${JSON.stringify(name(typeName))}))`
    }

    if (op === symbol("extend")) {
      const [typeName, protocol, ...methods] = rands

      const addMethods = methods.map((method) => {
        const [methodName, p, b] = [...method]

        const namespaced = JSON.stringify(
          `${name(protocol)}/${name(methodName)}`
        )
        const params = p.map(
          (param, i) => `,${JSON.stringify(name(param))}: args[${i}] ?? null`
        )
        const body = genCode(b)

        return `${genCode(typeName)}[${namespaced}] = (obj, ...args) => {
          return (($) => ${body})({...$,...(obj?.toPojo?.()??{})${params.join(
          ""
        )}})
        }`
      })

      return `(
        ${addMethods.join(",")}
        ,null
      )`
    }

    if (op === symbol("fn")) {
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
      return `Object.assign(${genCode(
        cons(symbol("fn"), exp.cdr)
      )}, {macro:true})`
    }

    if (op === symbol("quote")) {
      const quote = withType({
        null: (exp) => "null",
        boolean: (exp) => String(exp),
        symbol: (exp) => `Symbol.for(${JSON.stringify(name(exp))})`,
        number: (exp) => String(exp),
        string: (exp) => JSON.stringify(exp),
        List: (exp) => {
          if (exp.car === symbol("unquote")) {
            return genCode(exp.cdr.car)
          } else if (exp.car === symbol("unquote-splice")) {
            return `...${genCode(exp.cdr.car)}`
          }

          return `$["list"](${[...exp].map(quote).join(",")})`
        },
        Array: (exp) => `[${exp.map(quote).join(",")}]`,
        Hashmap: (exp) => `({ hashmap: ${quote(exp.hashmap)} })`,
      })

      return quote(rands[0])
    }

    return `$.toFn(${genCode(op)})(${rands
      .map((rand) => genCode(rand))
      .join(",")})`
  },
  Array: (exp) => `[${exp.map(genCode).join(",")}]`,
  Hashmap: (exp) =>
    `({ ${exp.hashmap
      .map(([key, val]) => `[${genCode(key)}]: ${genCode(val)}`)
      .join(", ")} })`,
})

export const macroExpand = ($) =>
  withType({
    null: (exp) => exp,
    boolean: (exp) => exp,
    symbol: (exp) => exp,
    number: (exp) => exp,
    string: (exp) => exp,
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
    Hashmap: (exp) => ({
      hashmap: exp.hashmap.map(([key, val]) => [
        macroExpand($)(key),
        macroExpand($)(val),
      ]),
    }),
  })

export const prn = withType({
  null: (exp) => "nil",
  boolean: (exp) => String(exp),
  symbol: (exp) => name(exp),
  number: (exp) => String(exp),
  string: (exp) => JSON.stringify(exp),
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
  Hashmap: (exp) =>
    `{ ${(exp.hashmap ?? Object.entries(exp))
      .map(([key, val]) => `${prn(key)} ${prn(val)}`)
      .join(", ")} }`,
  unknown: (exp) => `[type ${exp?.constructor?.name}]`,
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
  "+": (...xs) => xs.reduce((r, x) => r + x, 0),
  "-": (a, b) => a - b,
  "*": (...xs) => xs.reduce((r, x) => r * x, 1),
  "/": (a, b) => a / b,
  "%": (a, b) => a % b,
  "=": (a, b) => a === b, // need eqal
  "<": (a, b) => a < b,
  ">": (a, b) => a > b,

  Array: {
    "Coll/conj": (arr, item) => [...arr, item],
    "Coll/count": (arr) => arr.length,
    "Coll/seq": (arr) => new ArraySeq(arr, 0),
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
  invoke,
  keys: (map) => Object.keys(map),
  List,
  list,
  "list?": (x) => x instanceof List,
  log: console.log,
  max: (...xs) => Math.max(...xs),
  not: (x) => x === false || x === null,
  Object: {
    "Coll/conj": (obj, item) => ({ ...obj, [item[0]]: item[1] }),
    "Coll/count": (obj) => Object.keys(obj).length,
    "Coll/seq": (obj) => new ArraySeq(Object.entries(obj), 0),
  },
  resolve,
  "ROUND*": (n, x) => Math.round(x * 10 ** n) / 10 ** n,
  slice: (arr, start) => arr.slice(start),
  String: {
    "Coll/conj": (s, item) => s + item,
    "Coll/count": (s) => s.length,
    "Coll/seq": (s) => new ArraySeq(s.split(""), 0),
  },
  string: (x) => (typeof x === "symbol" ? name(x) : ""),
  "symbol?": (x) => typeof x === "symbol",
  toFn,
  "upper-case": (s) => s.toUpperCase(),
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
