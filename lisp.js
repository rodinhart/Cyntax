// Type system

// (deftype Null [])
const Null = {}
Null.params = []
Null.prototype = Null

const S = (name) => Symbol.for(name)
const Sn = (symbol) => Symbol.keyFor(symbol)

// (deftype List [car cdr])
function List(car, cdr) {
  if (!(this instanceof List)) return new List(car, cdr)

  this.car = car
  this.cdr = cdr
}
List.params = [S("car"), S("cdr")]
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
ArraySeq.params = [S("arr"), S("i")]

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
    : exp?.["Seq/first"]
    ? "Seq"
    : exp?.constructor === Object || exp?.hashmap
    ? "Hashmap"
    : "unknown"

const list = (...xs) => [...xs].reverse().reduce((r, x) => cons(x, r), null)

// Runtime helpers
const invoke = (obj, protocol, method, args) => {
  try {
    return (obj === null ? Null : obj)[`${Sn(protocol)}/${Sn(method)}`](...args)
  } catch (e) {
    console.log(obj, Sn(protocol), Sn(method))
    throw e
  }
}

const resolve = ($, name) => {
  if (!(name in $)) {
    throw Error(`Symbol ${name} is not defined`)
  }

  return $[name]
}

// check for array and pojo
const toFn = (op) => (typeof op === "function" ? op : (ix) => op[ix] ?? "FUBAR")

// Compiler
const withType =
  (dispatch) =>
  (exp, ...rest) => {
    const type = getType(exp)
    if (!(type in dispatch)) {
      throw new Error(`Unknown dispatch for ${type}`)
    }

    return dispatch[type](exp, ...rest)
  }

export const genCode = withType({
  null: (exp) => "null",
  boolean: (exp) => String(exp),
  symbol: (exp) => `$.resolve($, ${JSON.stringify(Sn(exp))})`,
  number: (exp) => String(exp),
  string: (exp) => JSON.stringify(exp),
  List: (exp, $) => {
    const [op, ...rands] = [...exp]

    if (op === S("def")) {
      const [name, val] = rands

      return `($[${JSON.stringify(Sn(name))}] = ${genCode(
        val ?? null
      )},Symbol.for(${JSON.stringify(Sn(name))}))`
    }

    if (op === S("defprotocol")) {
      const [name, ...fns] = rands
      const compiledMethods = fns.map((fn) => {
        const [method] = fn
        const compiledName = Sn(method)

        return `$[${JSON.stringify(
          compiledName
        )}]=(obj, ...args) => $.invoke(obj, Symbol.for(${JSON.stringify(
          Sn(name)
        )}), Symbol.for(${JSON.stringify(Sn(method))}), args)`
      })

      // use compile (quote name)
      return `(${compiledMethods.join(",")},Symbol.for(${JSON.stringify(
        Sn(name)
      )}))`
    }

    if (op === S("deftype")) {
      const [name, params] = rands

      // only works with javascript compatible names
      return `($[${JSON.stringify(Sn(name))}] = function ${Sn(name)}(${params
        .map(Sn)
        .join(",")}) {
        if (!(this instanceof ${Sn(name)})) return new ${Sn(name)}(${params
        .map(Sn)
        .join(", ")})
        ${params.map((param) => `this.${Sn(param)} = ${Sn(param)}`).join("\n")}
      }, $[${JSON.stringify(Sn(name))}].params = ${genCode(
        cons(S("quote"), cons(params, null))
      )}, Symbol.for(${JSON.stringify(Sn(name))}))`
    }

    if (op === S("extend")) {
      const [typeName, protocol, ...methods] = rands

      const type = resolve($, Sn(typeName))

      const typeParams = [...type.params].map(
        (param) =>
          `,${JSON.stringify(Sn(param))}: this[${JSON.stringify(Sn(param))}]`
      )

      const addMethods = methods.map((method) => {
        const [name, p, b] = [...method]

        const namespaced = JSON.stringify(`${Sn(protocol)}/${Sn(name)}`)
        const params = p.map(
          (param, i) => `,${JSON.stringify(Sn(param))}: args[${i}] ?? null`
        )
        const body = genCode(b)

        return `${genCode(
          typeName
        )}.prototype[${namespaced}] = function(...args) {
          return (($) => ${body})({...$${typeParams.join("")}${params.join(
          ""
        )}})
        }`
      })

      return `(
        ${addMethods.join(",")}
        ,null
      )`
    }

    if (op === S("fn")) {
      if (Array.isArray(rands[0])) {
        // (fn [x y] (+ x y))
        return genCode(list(S("fn"), exp.cdr))
      } else {
        // (fn ([x] x) ([x y] (+ x y)))
        const [...overloads] = rands

        const arities = overloads.map((overload) => {
          const [p, b] = overload

          const args = []
          for (let i = 0; i < p.length; i++) {
            if (p[i] !== S("&")) {
              args.push(`,${JSON.stringify(Sn(p[i]))}:args[${i}]`)
            } else {
              args.push(`,${JSON.stringify(Sn(p[i + 1]))}:args.slice(${i})`)
              i++
            }
          }

          const body = genCode(b)

          return `
          ${!p.includes(S("&")) ? `case ${p.length}:` : "default:"}
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

    if (op === S("if")) {
      const [pred, cons, alt] = rands

      return `((__) => __ !== false && __ !== null ? ${genCode(
        cons
      )} : ${genCode(alt ?? null)})(${genCode(pred)})`
    }

    if (op === S("let")) {
      const [bindings, body] = rands
      const pairs = bindings.reduce(
        (r, x, i) => (i % 2 ? r[r.length - 1].push(x) : r.push([x]), r),
        []
      )

      return `(($) => (
        ${pairs
          .map(
            ([name, exp]) => `$[${JSON.stringify(Sn(name))}] = ${genCode(exp)}`
          )
          .join(",")},
        ${genCode(body)}
      ))({...$})`
    }

    if (op === S("loop")) {
      const [bindings, body] = rands

      // is there a nicer way?
      const pairs = bindings.reduce(
        (r, x, i) => (i % 2 ? r[r.length - 1].push(x) : r.push([x]), r),
        []
      )

      return `(($) => {
        $["recur"] = (...args) => {
          ${pairs
            .map(([name], i) => `$[${JSON.stringify(Sn(name))}] = args[${i}]`)
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

    if (op === S("macro")) {
      return `Object.assign(${genCode(cons(S("fn"), exp.cdr))}, {macro:true})`
    }

    if (op === S("quote")) {
      const quote = withType({
        null: (exp) => "null",
        boolean: (exp) => String(exp),
        symbol: (exp) => `Symbol.for(${JSON.stringify(Sn(exp))})`,
        number: (exp) => String(exp),
        string: (exp) => JSON.stringify(exp),
        List: (exp) => {
          return `$["list"](${[...exp]
            .map((x) => {
              if (x instanceof List && x.car === S("unquote")) {
                return genCode(x.cdr.car)
              } else if (x instanceof List && x.car === S("unquote-splice")) {
                return `...${genCode(x.cdr.car)}`
              } else {
                return quote(x)
              }
            })
            .join(",")})`
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
      if (typeof op === "symbol" && $[Sn(op)]?.macro === true) {
        return macroExpand($)(toFn($[Sn(op)])(...rands))
      }

      if (op === S("quote")) {
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
  symbol: (exp) => Sn(exp),
  number: (exp) => String(exp),
  string: (exp) => JSON.stringify(exp),
  List: (exp) => `(${[...exp].map((x) => prn(x)).join(" ")})`,
  Array: (exp) => `[${exp.map((x) => prn(x)).join(" ")}]`,
  Seq: (exp) => {
    const r = []
    let c = exp
    while (c) {
      r.push(c["Seq/first"]())
      c = c["Seq/rest"]()
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
      return list(S("quote"), _(xs))
    }

    if (x === "~") {
      if (xs[0] !== "@") {
        return list(S("unquote"), _(xs))
      } else {
        xs.shift()

        return list(S("unquote-splice"), _(xs))
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

    return S(x)
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

  // define macro in core?
  and: (...args) => {
    switch (args.length) {
      case 0:
        return true

      case 1:
        return args[0]

      default:
        return args[0] !== false && args[0] !== null
          ? native.and(...args.slice(1))
          : args[0]
    }
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

  // define as macro in core?
  comp:
    (g, f) =>
    (...args) =>
      toFn(g)(toFn(f)(...args)),
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
  max: (...xs) => Math.max(...xs),
  macroexpand: (form) => macroExpand($)(form),
  not: (x) => x === false || x === null,
  Null,
  resolve,
  "ROUND*": (n, x) => Math.round(x * 10 ** n) / 10 ** n,
  slice: (arr, start) => arr.slice(start),
  string: (x) => (typeof x === "symbol" ? Sn(x) : ""),
  "symbol?": (x) => typeof x === "symbol",
  toFn,
  "upper-case": (s) => s.toUpperCase(),

  // (defprotocol Coll (conj [item]) (count []) (seq []))
  conj: (coll, item) => {
    if (typeof coll === "string") {
      return coll + item
    }

    if (Array.isArray(coll)) {
      return [...coll, item]
    }

    if (coll?.constructor === Object) {
      return {
        ...coll,
        [item[0]]: item[1],
      }
    }

    return invoke(coll, S("Coll"), S("conj"), [item])
  },
  count: (coll) => {
    if (typeof coll === "string" || Array.isArray(coll)) {
      return coll.length
    }

    if (coll?.constructor === Object) {
      return Object.keys(coll).length
    }

    return invoke(coll, S("Coll"), S("count"), [])
  },
  seq: (coll) => {
    if (typeof coll === "string") {
      return $["ArraySeq"](coll.split(""), 0)
    }

    if (Array.isArray(coll)) {
      return coll.length ? ArraySeq(coll, 0) : null
    }
    if (coll?.constructor === Object) {
      return ArraySeq(Object.entries(coll), 0)
    }

    return invoke(coll, S("Coll"), S("seq"), [])
  },

  tap: (x) => console.log("tap", x) || x,
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

      const javascript = genCode(expanded, $2)
      debug("javascript", javascript)

      const compiled = eval(`(($) => ${javascript})`)

      compiled($2)
    })

    return Object.fromEntries(Object.entries($2).filter(([key]) => !(key in $)))
  }
