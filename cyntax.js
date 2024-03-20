import core from "./core.js"
import { lisp, native } from "./lisp.js"

export default ($) => lisp({ ...native, ...core, ...$ })
