import cyntax from "./cyntax.js"
import { prn } from "./lisp.js"

const result = null

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

window.c = (t) => prn(cyntax({})([`(def __ ${t})`]).__)
