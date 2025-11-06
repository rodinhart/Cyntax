import * as list from "./list.js"

export const withType = (name, dispatch) => (exp) => {
  const type = exp?.constructor?.name ?? "Nil"

  if (type in dispatch) {
    return dispatch[type](exp)
  }

  // Make sequences first class
  if (exp?.constructor?.["Seq/first"]) {
    if ("Seq" in dispatch) {
      return dispatch["Seq"](exp)
    }

    throw new Error(`${name} has no dispatch for Seq (${type})`)
  }

  if ("?" in dispatch) {
    return dispatch["?"](exp)
  }

  throw new Error(`${name} has no dispatch for ${type}`)
}

const compile = withType("Compiler", {
  Nil: () => "null",

  Symbol: (exp) => `$.resolve($,${JSON.stringify(Symbol.keyFor(exp))})`,

  Number: (exp) => String(exp),

  Array: (exp) => `[${exp.map(compile).join(",")}]`,

  List: (exp) => {
    const [op, ...rands] = [...exp]

    switch (op) {
      case Symbol.for("def"): {
        // (def name val)
        const name = JSON.stringify(Symbol.keyFor(rands[0]))
        const val = compile(rands[1] ?? null)

        return `($[${name}]=${val},Symbol.for(${name}))`
      }

      case Symbol.for("fn"): {
        // (fn [param param] body)
        if (Array.isArray(rands[0])) {
          // (fn [x y] (+ x y))
          return compile(list.list(op, exp.cdr))
        } else {
          // (fn ([x] x) ([x y] (+ x y)))
          const [...overloads] = rands

          const arities = overloads.map((overload) => {
            const [p, b] = overload

            const args = []
            for (let i = 0; i < p.length; i++) {
              if (p[i] !== Symbol.for("&")) {
                args.push(`,${JSON.stringify(Symbol.keyFor(p[i]))}:args[${i}]`)
              } else {
                args.push(
                  `,${JSON.stringify(Symbol.keyFor(p[i + 1]))}:args.slice(${i})`
                )
                i++
              }
            }

            const body = compile(b)

            return `
                 ${
                   !p.includes(Symbol.for("&"))
                     ? `case ${p.length}:`
                     : "default:"
                 }
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
    }

    return `$.apply(${compile(op)},[${rands
      .map((rand) => compile(rand))
      .join(",")}])`
  },
})

export default compile
