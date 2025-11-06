export const egal = (a, b) => {
  if (a === b) {
    return true
  }

  // treats undefined and null both as Nil
  const ta = a?.constructor?.name ?? "Nil"
  const tb = b?.constructor?.name ?? "Nil"

  if (ta !== tb) {
    return false
  }

  if (a?.valueOf?.() === b?.valueOf?.()) {
    return true
  }

  // could be dispatch
  if (ta === "List") {
    return egal([...a], [...b])
  }

  if (ta === "Array") {
    return a.length === b.length && a.every((x, i) => egal(x, b[i]))
  }

  if (ta === "Map") {
    return (
      a.size === b.size &&
      [...a].every(([key, val]) => b.has(key) && egal(val, b.get(key)))
    )
  }

  if (ta === "Object") {
    return (
      Object.keys(a).length === Object.keys(b).length &&
      Object.entries(a).every(([key, val]) => egal(val, b[key]))
    )
  }

  if (ta === "Set") {
    return a.size === b.size && a.isSubsetOf(b)
  }

  return false
}
