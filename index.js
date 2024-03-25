import cyntax from "./cyntax.js"
import { prn } from "./lisp.js"
import pql from "./pql.js"

const result = pql.result

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

window.c = (t) => prn(cyntax(pql)([`(def __ ${t})`]).__)
