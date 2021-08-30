# tupi
Tupi - A lingua franca for computer science

( ; ; )                     
= /= >= <= > < + - * / ^    default left-associative infix identifier (binop) with conventional precedence
{=}  {/=}  {>=} ...         binop as prefix
{_ + 7}  {_3 + _1 / _2}     annonymous arg
x ← 3                       assignment
{a b: a + b}                lambda function
{a:n b:n "a / b":n}         typed native code
b   ↓ ↑                     boolean
c   't'                     char
s   "text"                  string
n   32                      number
[]                          empty list is a value of type, e.g., [n]
[1,2]                       syntax sugar for [] « 1 « 2
"key"→value                 tuple
·                           composition


# Remarks

Based on

2013 Functional language parser/interpreter https://github.com/davips/lamdheal-j


