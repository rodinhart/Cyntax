export const parseGrammar = (grammar) =>
  Object.fromEntries(
    grammar
      .trim()
      .split(/\n\n+/)
      .map((rule) => [
        rule.split("=")[0].trim(),
        rule
          .split("=")[1]
          .trim()
          .split("|")
          .map((alt) =>
            alt
              .trim()
              .split(/\s+/)
              .map((term) =>
                term[0] === "/" ? String.fromCharCode(term.slice(1)) : term
              )
          ),
      ])
  )

export const exec = (rules, s, path = [Object.keys(rules).at(-1)], start) => {
  const rule = path.at(-1)
  const expected = []
  for (const alt of rules[rule]) {
    let i = start ?? 0
    const r = []
    for (const term of alt) {
      if (term === "#") {
        continue
      }

      if (term.length === 1) {
        if (i < s.length && term === s[i]) {
          r.push(s[i])
          i++
        } else {
          expected[i] ??= new Set()
          expected[i].add(path.findLast((r) => r.at(-1) === ":") ?? term)
          i = -1
          break
        }
      } else if (term[0] === "^") {
        const chars = new Set()
        for (let i = 1; i < term.length; ) {
          if (term[i] !== "/") {
            chars.add(term[i])
            i++
          } else {
            let j = i + 1
            while (j < term.length && term[j] >= "0" && term[j] <= "9") {
              j++
            }

            chars.add(String.fromCharCode(term.slice(i + 1, j)))
            i = j
          }
        }

        if (i < s.length && !chars.has(s[i])) {
          r.push(s[i])
          i++
        } else {
          i = -1
          expected[i] ??= new Set()
          expected[i].add(
            path.findLast((r) => r.at(-1) === ":") ??
              `not ${[...chars].join(" ")}`
          )

          break
        }
      } else {
        const t = exec(rules, s, [...path, term], i)
        if (!t?.EXPECTED) {
          r.push(...t[1])
          i = t[0]
        } else {
          i = -1
          for (let i = 0; i < t.EXPECTED.length; i++) {
            if (t.EXPECTED[i]) {
              expected[i] = new Set([...(expected[i] ?? []), ...t.EXPECTED[i]])
            }
          }
          break
        }
      }
    }

    if (i !== -1 && (start !== undefined || i >= s.length)) {
      return [
        i,
        rule.at(-1) === "#"
          ? []
          : rule.at(-1) === "*"
          ? [r.join("")]
          : rule.at(-1) === ":"
          ? [[rule.slice(0, -1), start ?? 0, ...r]]
          : r,
      ]
    }

    if (i !== -1) {
      expected[i] ??= new Set()
      expected[i].add("END")
    }
  }

  return { EXPECTED: expected }
}

export const formatExpected = (r, input) =>
  ((i) =>
    `Grammar expected ${[...r.EXPECTED[i]]
      .map((t) => `'${t}'`)
      .join(", ")} but found '${input[i] ?? "END"}' at ${i}`)(
    r.EXPECTED.findLastIndex((set) => set)
  )
