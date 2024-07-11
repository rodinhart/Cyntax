import cyntax from "./cyntax.js"
import { prn } from "./lisp.js"
import pql from "./pql.js"

const result = pql.main({ data: { x: [2, 3, 5, 7, 11] } })

document.getElementById("output").innerText = prn(result)
if (result?.SELF?.data) {
  const df = result.SELF
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
