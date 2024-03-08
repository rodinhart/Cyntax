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

const toFn = (op) =>
  typeof op === "function"
    ? op
    : op?.arity
    ? (...args) => (op.arity[args.length] ?? op.arity[-1])(...args)
    : (ix) => op[ix] ?? "FUBAR"

// Compiler
const withType = (dispatch) => (exp) => {
  const type = getType(exp)
  if (!(type in dispatch)) {
    throw new Error(`Unknown dispatch for ${type}`)
  }

  return dispatch[type](exp)
}

const generateCode = withType({
  null: (exp) => "null",
  boolean: (exp) => String(exp),
  symbol: (exp) => `resolve($, ${JSON.stringify(Sn(exp))})`,
  number: (exp) => String(exp),
  string: (exp) => JSON.stringify(exp),
  List: (exp) => {
    const [op, ...rands] = [...exp]

    if (op === S("def")) {
      const [name, val] = rands

      return `($[${JSON.stringify(Sn(name))}] = ${generateCode(
        val ?? null
      )},S(${JSON.stringify(Sn(name))}))`
    }

    if (op === S("defprotocol")) {
      const [name, ...fns] = rands
      const compiledMethods = fns.map((fn) => {
        const [method] = fn
        const compiledName = Sn(method)

        return `$[${JSON.stringify(
          compiledName
        )}]=(obj, ...args) => invoke(obj, S(${JSON.stringify(
          Sn(name)
        )}), S(${JSON.stringify(Sn(method))}), args)`
      })

      // use compile (quote name)
      return `(${compiledMethods.join(",")},S(${JSON.stringify(Sn(name))}))`
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
      }, $[${JSON.stringify(Sn(name))}].params = ${generateCode(
        cons(S("quote"), cons(params, null))
      )}, S(${JSON.stringify(Sn(name))}))`
    }

    if (op === S("extend")) {
      const [type, protocol, ...methods] = rands

      const typeParams = [...$[Sn(type)].params].map(
        (param) =>
          `,${JSON.stringify(Sn(param))}: this[${JSON.stringify(Sn(param))}]`
      )

      const addMethods = methods.map((method) => {
        const [name, p, b] = [...method]

        const namespaced = JSON.stringify(`${Sn(protocol)}/${Sn(name)}`)
        const params = p.map(
          (param, i) => `,${JSON.stringify(Sn(param))}: args[${i}] ?? null`
        )
        const body = generateCode(b)

        return `${generateCode(
          type
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
        return generateCode(list(S("fn"), exp.cdr))
      } else {
        // (fn ([x] x) ([x y] (+ x y)))
        const [...overloads] = rands

        return `({
          arity: {
            ${overloads
              .map((overload) => {
                const [p, b] = overload

                const params = []
                for (let i = 0; i < p.length; i++) {
                  if (p[i] !== S("&")) {
                    params.push(`$${i}`)
                  } else {
                    params.push(`...$${i}`)
                    i++
                  }
                }

                const args = []
                for (let i = 0; i < p.length; i++) {
                  if (p[i] !== S("&")) {
                    args.push(`,${JSON.stringify(Sn(p[i]))}:$${i}`)
                  } else {
                    args.push(`,${JSON.stringify(Sn(p[i + 1]))}:$${i}`)
                    i++
                  }
                }

                const body = generateCode(b)

                return `[${
                  !p.includes(S("&")) ? p.length : -1
                }]: ((${params.join(",")}) => (($) => ${body})({...$${args.join(
                  ""
                )}}))`
              })
              .join(", ")}
            }
        })`
      }
    }

    if (op === S("if")) {
      const [pred, cons, alt] = rands

      return `((__) => __ !== false && __ !== null ? ${generateCode(
        cons
      )} : ${generateCode(alt ?? null)})(${generateCode(pred)})`
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
            ([name, exp]) =>
              `$[${JSON.stringify(Sn(name))}] = ${generateCode(exp)}`
          )
          .join(",")},
        ${generateCode(body)}
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
        $["recur"](${pairs.map(([, init]) => generateCode(init)).join(", ")})
        let r
        do {
          r = ${generateCode(body)}
        } while (r === "RECUR")

        return r
      })({...$})`
    }

    if (op === S("macro")) {
      return `Object.assign(${generateCode(
        cons(S("fn"), exp.cdr)
      )}, {macro:true})`
    }

    if (op === S("quote")) {
      const quote = withType({
        null: (exp) => "null",
        boolean: (exp) => String(exp),
        symbol: (exp) => `S(${JSON.stringify(Sn(exp))})`,
        number: (exp) => String(exp),
        string: (exp) => JSON.stringify(exp),
        List: (exp) => {
          return `$["list"](${[...exp]
            .map((x) => {
              if (x instanceof List && x.car === S("unquote")) {
                return generateCode(x.cdr.car)
              } else if (x instanceof List && x.car === S("unquote-splice")) {
                return `...${generateCode(x.cdr.car)}`
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

    return `toFn(${generateCode(op)})(${rands
      .map((rand) => generateCode(rand))
      .join(",")})`
  },
  Array: (exp) => `[${exp.map(generateCode).join(",")}]`,
  Hashmap: (exp) =>
    `({ ${exp.hashmap
      .map(([key, val]) => `[${generateCode(key)}]: ${generateCode(val)}`)
      .join(", ")} })`,
})

const macroExpand = ($) =>
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

const prn = withType({
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

const read = (input) => {
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
const $ = {
  "+": (...xs) => xs.reduce((r, x) => r + x, 0),
  "-": (a, b) => a - b,
  "*": (...xs) => xs.reduce((r, x) => r * x, 1),
  "%": (a, b) => a % b,
  "=": (a, b) => a === b, // need eqal
  "<": (a, b) => a < b,
  ">": (a, b) => a > b,

  and: {
    arity: {
      0: () => true,
      1: (x) => x,
      "-1": (x, ...next) =>
        x !== false && x !== null ? toFn($.and)(...next) : x,
    },
  },
  assoc: (map, ...xs) => {
    const result = { ...map }
    for (let i = 0; i + 1 < xs.length; i += 2) {
      result[xs[i]] = xs[i + 1]
    }

    return result
  },
  car: (exp) => exp.car,
  cdr: (exp) => exp.cdr,
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
  List,
  list,
  "list?": (x) => x instanceof List,
  max: (...xs) => Math.max(...xs),
  macroexpand: (form) => macroExpand($)(form),
  not: (x) => x === false || x === null,
  Null,
  slice: (arr, start) => arr.slice(start),
  string: (x) => (typeof x === "symbol" ? Sn(x) : ""),
  "symbol?": (x) => typeof x === "symbol",
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
      return coll.length ? $["ArraySeq"](coll, 0) : null
    }
    if (coll?.constructor === Object) {
      return $["ArraySeq"](Object.entries(coll), 0)
    }

    return invoke(coll, S("Coll"), S("seq"), [])
  },

  tap: (x) => console.log("tap", x) || x,
}

const input = await fetch("./core.clj").then((r) => r.text())

const code = read(`[${input}]`)

const debug = (...args) => {
  // console.log(...args)
}
const result = code.reduce((_, form) => {
  let expanded
  let javascript
  let compiled
  try {
    debug("--")
    debug("form", prn(form))

    expanded = macroExpand($)(form)
    debug("expanded", prn(expanded))

    javascript = generateCode(expanded)
    debug("javascript", javascript)

    compiled = eval(`(($) => ${javascript})`)
    const r = compiled($)
    debug("r", r)

    return r
  } catch (e) {
    console.error(e)
    console.log(prn(form))
    console.log(prn(expanded))
    console.log(javascript)
    throw e
  }
}, null)

// console.log(result)

document.getElementById("output").innerText = prn(result)
if (result?.self?.data) {
  const df = result.self
  document.getElementById("table").innerHTML = `
    <table>
      <tr>${df.keys.map((key) => `<th>${key}</th>`).join("")}</tr>
      ${df.indices
        .map(
          (index) => `
        <tr>${df.keys
          .map((key) => `<td>${df.data[key][index]}</td>`)
          .join("")}</tr>
      `
        )
        .join("\n")}
    </table>
  `
}

window.l = (strings, ...exps) => {
  const tokens = read(strings.join(""))
  debug("tokens", prn(tokens))
  const expanded = macroExpand($)(tokens)
  debug("expanded", prn(expanded))
  const javascript = generateCode(expanded)
  debug("javascript", javascript)
  const compiled = eval(`(($) => ${javascript})`)

  return prn(compiled($))
}
