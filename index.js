import cyntax from "./cyntax.js"
import { prn } from "./lisp.js"
import pql from "./pql.js"

const result = pql.main()
// console.log(result)
document.getElementById("output").innerText = prn(result)
if (result?.has?.("keys") && result?.has?.("indices")) {
  const df = result
  document.getElementById("table").innerHTML = `
    <table>
      <tr>${df
        .get("keys")
        .map((key) => `<th>${key}</th>`)
        .join("")}</tr>
      ${df
        .get("indices")
        .map(
          (index) => `
        <tr>${df
          .get("keys")
          .map((key) => `<td>${df.get("data").get(key)[index]}</td>`)
          .join("")}</tr>
      `
        )
        .join("\n")}
    </table>
  `
}

window.c = (t) => prn(cyntax({})([`(def __ ${t})`]).__)
