const lisp = `

quoted = ws ~ @ literal  (_,q,s,[l])=>({car:"unquote-splice",cdr:({car:l,cdr:null})})
       | ws ~ literal    (_,q,[l])=>({car:"unquote",cdr:({car:l,cdr:null})})
       | ws ' literal    (_,q,[l])=>({car:"quote",cdr:({car:l,cdr:null})})
       | literal         ([l])=>l

literal = nil | bool | number | string | symbol
        | list | array | hashmap | set

set = ws # { items ws }  (_,__,o,e)=>newSet(e)

hashmap = ws { items ws }  (_,o,e)=>({hashmap:e.reduce((r,x,i)=>i%2?[...r,[e[i-1],x]]:r,[])})

array = ws [ items ws ]  (_,o,i)=>i

list = ws ( items ws )  (_,o,i)=>i.reverse().reduce((cdr,car)=>({car,cdr}),null)

items = quoted sep items  (x,_,xs)=>[x,...xs]
      | quoted          (x)=>[x]
      |                 ()=>[]

sep = _ | \\n | ,

symbol = symbol*              (s)=>Symbol.for(s)
symbol* = nonnumeric symbol*  (c,s)=>c+s
        | nonnumeric          (c)=>c

string = ws " string* "  (_,q,c)=>c
string* = \\ " string*   (_,__,s)=>'"'+s
        | char string*   ([c],s)=>c+s
        |                ()=>""

char = nonnumeric  ([c])=>c
     | digit       ([c])=>c
     | _ | \\n | ,

nonnumeric = letter   ([c])=>c
           | special  ([c])=>c

special = * | + | ! | - | \\_ | ? | < | > | = | & | \\|

letter = a | b | c | d | e | f | g | h | i | j | k | l | m
       | n | o | p | q | r | s | t | u | v | w | x | y | z
       | A | B | C | D | E | F | G | H | I | J | K | L | M
       | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

number = ws - digits . digits  (_,...p)=>Number(p.join(""))
       | ws digits . digits    (_,...p)=>Number(p.join(""))
       | ws - digits           (_,...p)=>Number(p.join(""))
       | ws digits             (_,d)=>Number(d)

digits = digit digits  ([d],i)=>d+i
       | digit         ([d])=>d

digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

bool = ws t r u e    (...a)=>true
     | ws f a l s e  (...a)=>false

nil = ws n i l  ()=>null

ws = comment ws
   | _ ws
   | \\n ws
   |             ()=>{}

comment   = ; comment* \\n
comment*  = comment** comment*
          |  ()=>{}
comment** = nonnumeric | digit | _ | , | ;

end = end
]`

// prettier-ignore
const s = `[

;; lang
(def defmacro (macro [name & forms]
  '(def ~name (macro ~@forms))))

(defmacro defn [name & forms]
  '(def ~name (fn ~@forms)))

(defmacro ->
  ([x] x)
  ([x form] (if (list? form)
    '(~(car form) ~x ~@(cdr form))
    '(~form ~x)))
  ([x form & rest] '(-> (-> ~x ~form) ~@rest)))

(defmacro ->>
  ([x] x)
  ([x form] (if (list? form)
    '(~@form ~x)
    '(~form ~x)))
  ([x form & rest] '(->> (->> ~x ~form) ~@rest)))

(defn | [val & pipes]
  (loop [cur val i 0]
    (if (< i (count pipes))
      cur)))


]`

const escape = {
  _: " ",
  "\\_": "_",
  "\\n": "\n",
  "\\|": "|",
}
const compile = (s) => {
  const terms = s.trim().split(/\s+/)
  const r = {}
  while (terms.length) {
    const key = terms.shift()
    r[key] = []
    terms.shift() === "=" // skip =

    while (terms.length && terms[1] !== "=") {
      const seq = []
      while (terms.length && terms[0] !== "|" && terms[1] !== "=") {
        const t = terms.shift()
        seq.push(escape[t] ?? t)
      }

      if (seq[seq.length - 1].includes("=>")) {
        r[key].push({
          seq: seq.slice(0, seq.length - 1),
          gencode: eval(seq[seq.length - 1].replace(/(new)/g, "$1 ")),
        })
      } else {
        r[key].push({ seq })
      }

      if (terms[0] === "|") {
        terms.shift() // skip |
      }
    }
  }

  return r
}

const interpret = (syntax, source, rule, start) =>
  rule.reduce((r, { seq, gencode }) => {
    if (!r) {
      const result = seq.reduce(
        (r, term) => {
          if (r) {
            if (term.length === 1) {
              if (source[r.index] === term) {
                return { matches: [...r.matches, term], index: r.index + 1 }
              }
            } else {
              const sub = interpret(syntax, source, syntax[term], r.index)
              if (sub) {
                return {
                  ...sub,
                  matches: [...r.matches, sub.matches],
                }
              }
            }
          }

          return null
        },
        { matches: [], index: start }
      )

      if (result) {
        return gencode
          ? {
              ...result,
              matches: gencode(...result.matches),
            }
          : result
      }
    }

    return r
  }, null)

const compiled = compile(lisp)
// console.log(compiled)
console.log(interpret(compiled, s, Object.values(compiled)[0], 0))

export const lispReader = (s) =>
  interpret(compiled, s, Object.values(compiled)[0], 0)
