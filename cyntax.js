import core from "./core.test.js"
import { lisp, macroExpand, native } from "./lisp.js"

export default ($) =>
  lisp({
    ...native,
    macroexpand: macroExpand({ ...native, ...core, ...$ }),
    ...core,
    ...$,
  })
