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

  Symbol: (exp) => `$.resolve($, ${JSON.stringify(Symbol.keyFor(exp))})`,

  Number: (exp) => String(exp),

  Array: (exp) => `[${exp.map(compile).join(",")}]`,
})

export default compile
