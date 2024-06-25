# Cyntax

JavaScript syntax can be clumsy at times. For example, building a histogram of characters in a string using standard library functions:

```js
const histogram = Object.entries(Object.groupBy(text.split(""), (x) => x)).map(
  ([key, lst]) => [key, lst.length]
)
```

Because JavaScript is a mix of static functions and object methods, half of the above reads inside-out, the other left-to-right. One way to improve on this is to introduce a function `thread`:

```js
const thread = (x, ...fns) => fns.reduce((r, fn) => fn(r), x)

const histogram = thread(
  text,
  (s) => s.split(""),
  (xs) => Object.groupBy(xs, (x) => x),
  (groups) => Object.entries(groups),
  (entries) => entries.map(([key, lst]) => [key, lst.length])
)
```

This would be improved if all operations were functions with the collection as the last parameter:

```js
const histogram = thread(
  text,
  (s) => split("", s),
  (xs) => groupBy((x) => x, xs),
  (entries) => map(([key, lst]) => [key, lst.length], entries)
)
```

`Object.entries` has been eliminated by assuming `map` also operates on plain objects. But I still have write a lambda for each step and name the intermediate results. With the abstraction powers of LISP macros, we can write the following Cyntax:

```clj
(def histogram (->> text
  (split "")
  (groupBy (fn [x] x))
  (map (fn [[key lst]] [key (length lst)]))))
```

Other examples of syntax abstraction using macros are lazy sequences, conditionals, pattern matching and for comprehension. Partial solutions exists in JavaScript, but usually require nullary lambdas to delay evaluation. This is of course half of what a macro does, the other half being altering code before evaluation. This is not possible in JavaScript without the use of eval, at which point you are defining at least a DSL anyway.

## Conditional clauses

Coding for mutiple conditional clauses in JavaScript can become messy. For example:

```js
return a === 1
  ? "one"
  : b === 2 && c !== 3
  ? "two"
  : d > 4
  ? "three"
  : d <= 10
  ? "four"
  : "five"
```

Even with better formatting (this is how prettier does it), it is not very readable. Better is using the switch statement. Switch only allows value comparison, not predicates, but we can turn it around:

```js
;(() => {
  switch (true) {
    case a === 1:
      return "one"

    case b === 2 && c !== 3:
      return "two"

    case d > 4:
      return "three"

    case d <= 10:
      return "four"

    default:
      return "five"
  }
})()
```

Much more readable, but notice we have to work around the fact that switch is a statement, not an expression. With Cyntax:

```clj
(cond
  (= a 1) "one"
  (and (= b 2) (not= c 3)) "two"
  (> d 4) "three"
  (<= d 10) "four"
  true "five")
```

# How to use

Cyntax follows JavaScript as closely as possible. This includes data structures, calling conventions and modules. The library that comes with Cyntax makes some different choices however, mostly in term of values, value equality and immutability.

Inclusion of Cyntax is done through modules:

```js
// file: extraMaths.js

import cyntax from "./cyntax.js"
import { square } from "./baseMaths.js"

export default cyntax({ square })`
 
(defn cube [x] (* x (square x)))
 
`
```

`cyntax` takes a scope extension, `square` in this case, and a template literal containing Cyntax. The return an object with any key/values defined in Cyntax. This in turn could be used in another JavaScript or Cyntax module:

```js
import extraMaths from "./extraMaths.js"

console.log(extraMath.cube(3)) // 27
```

Exports from Cyntax have to be in object form for two reasons: Module `import` and `export` are static statements, and can therefor not be generated by Cyntax without a project compilation pipeline. And Cyntax symbols can contain characters not allowed by JavaScript (e.g. `null?`).

# Reference

**+**

**-**

**\***

**/**

**%**

**=**

**<**

**>**

**->**

**->>**

**|**

**and**

**apply**

**assoc**

**assoc-in**

**car**

**cdr**

**comp**

**cond**

**conj**

**cons**

**constantly**

**contains?**

**count**

**cycle**

**def**

**defn**

**defmacro**

**dissoc**

**drop**

**filter**

**filter-array**

**first**

**fold**

**get**

**get-in**

**identity**

**inc**

**into**

**keys**

**lazy-cons**

**list**

**list?**

**log**

**map**

**map-array**

**map-list**

**max**

**name**

**nil**

**nil?**

**not**

**not=**

**odd?**

**range**

**rest**

**seq**

**slice**

**string**

**symbol?**

**take**

**tap**

**test**

**update**

**update-in**

**upper-case**

**vals**

**vector**

# TODO

- fix defining something already in scope
- multi arity in protocol ?
  - (count nil 2) doesn't throw error
  - protocol Blah (foo []) (foo [x]) ?
- destructering
- allow spaces in strings - proper lexer
- and#
- tests
