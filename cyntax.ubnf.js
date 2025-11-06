export default `
ws = /32 | /10 | ,

ws# = ws ws# | #

digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

digits* = digit digits* | digit

number: = digits*

letter = a | b | c | d | e | f | g | h | i | j | k | l | m | n | o | p | q | r | s | t | u | v | w | x | y | z

alpha = letter | _

alphanumeric* = alpha alphanumeric* | digit alphanumeric* | #

symbol* = alpha alphanumeric*

symbol: = symbol*

(# = (

)# = )

items = ws# expr items | #

list: = (# items ws# )#

[# = [

]# = ]

array: = [# items ws# ]#

chars* = /92 " chars* | ^" chars* | #

"# = "

string: = "# chars* "#

expr = number: | symbol: | list: | array: | string:

cyntax = ws# expr ws#
`
