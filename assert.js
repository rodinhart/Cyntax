import * as kernel from "./kernel.js"

export const equals = (a, b) => {
  if (!kernel.egal(a, b)) {
    console.error("Test failed, found", a, "but expected", b, new Error().stack)
    console.error()
  }
}
