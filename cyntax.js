import core from "./core.js"
import { lisp, macroExpand, native } from "./lisp.js"

export default ($) =>
  lisp({
    ...native,
    macroexpand: macroExpand({ ...native, ...core, ...$ }),
    ...core,
    ...$,
  })
