```
           prn (str)       codeGen (js)    quote (data)     macroExpand (data)
null       `nil`           `null`          null             null
boolean    `true`          `true`          true             true
symbol     `foo`           `$["foo"]`      foo              foo
number     `3.14`          `3.14`          3.14             3.14
string     `"Hello"`       `"Hello"`       "Hello"          "Hello"
List       `(f x y)`       ***             (f x y)          ***
Array      `[1 2 3]`       `[1, 2, 3]`     [1 2 3]          [1 2 3]
Seq        `(1 2 3)`
Hashmap
unknown    [type User]
```

## TODO

- make macro a fn with meta data
- destructering
- ffi
- fix arity
- review native functions (specially invoke, resolve, toFn)
