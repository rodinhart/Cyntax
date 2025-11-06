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
}

export default ($) => (strings) => {
  const code = read(`[${strings.join("")}]`)
  const $2 = { ...native, ...$ }
  for (const form of code) {
    const js = compile(form)
    eval(`(($) => ${js})`)($2)
  }

  return Object.fromEntries(Object.entries($2).filter(([key]) => !(key in $)))
}

export const evalForm = (s, $ = {}) => {
  const form = read(s)
  const js = compile(form)

  return eval(`(($) => ${js})`)({ ...native, ...$ })
}
