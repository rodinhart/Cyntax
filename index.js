import { genCode, macroExpand, native, prn, read } from "./lisp.js"
import pql from "./pql.js"

const debug = (...args) => {
  // console.log(...args)
}

const $ = { ...native }

const input = await fetch("./core.clj").then((r) => r.text())

const code = read(`[${input}]`)

const result = code.reduce((_, form) => {
  let expanded
  let javascript
  let compiled
  try {
    debug("--")
    debug("form", prn(form))

    expanded = macroExpand($)(form)
    debug("expanded", prn(expanded))

    javascript = genCode(expanded, $)
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
  const javascript = genCode(expanded)
  debug("javascript", javascript)
  const compiled = eval(`(($) => ${javascript})`)

  return prn(compiled($))
}

console.log(pql.result)
