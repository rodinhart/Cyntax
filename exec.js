import read from "./reader.js"
import compile from "./compiler.js"

// Symbol lookup
const resolve = ($, name) => {
  if (!(name in $)) {
    throw Error(`Symbol ${name} is not defined`)
  }

  return $[name]
}

const native = {
  resolve,
  apply: (fn, args) => fn(...args), // temp protocal for apply
  "*": (...xs) => xs.reduce((r, x) => r * x, 1),
}

export default ($) => (strings) => {
  const code = read(`[${strings.join("")}]`)
  const $2 = { ...native, ...$ }
  const keys = new Set(Object.keys($2))
  for (const form of code) {
    const js = compile(form)
    eval(`(($) => ${js})`)($2)
  }

  return Object.fromEntries(
    Object.entries($2).filter(([key]) => !keys.has(key))
  )
}

export const evalForm = (s, $ = {}) => {
  const form = read(s)
  const js = compile(form)

  return eval(`(($) => ${js})`)({ ...native, ...$ })
}
