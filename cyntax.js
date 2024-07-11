import core from "./core.test.js"
import { lisp, native } from "./lisp.js"

export default ($) =>
  lisp({
    ...native,
    ...core,
    ...$,
  })
