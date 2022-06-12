# Tupi Unique Program Identification - "A lingua franca de novo"
This motto is a wordplay meaning "A link language from the beginning" if read as an English sentence, 
but also "The link language is back" if read as written in Portuguese.
Neither of these meanings are particularly true, as the _tupi_ programming language doesn't represent the 
Tupian languages in any way, and the effort needed for a language to be an interface for other or even to 
simply become seriously used by third parties requires time, money and people, something that won't happen.
However, the pun - and the fun of language design - are still worth it.

**This is an ongoing work, not ready for use right now.**
```
( ; ; )                     sequence of expressions
= /= >= <= > < + - * / ^    default left-associative infix identifier (binop) with conventional precedence
{=}  {/=}  {>=} ...         binop as prefix
{_ + 7}  {_3 + _1 / _2}     annonymous arg
x ← 3                       assignment (AltGr+y)
{a b: a + b}                lambda function
{a:n b:n "a/b":n}           typed native code
b   ↓ ↑                     boolean (AltGr+u, AltGr+Shift+u)
c   't'                     char
t   "some text"             text
n   32                      number
[]                          empty list is a value of type, e.g., [n]
[1,2]                       syntax sugar for [] « 1 « 2
"key"→value                 tuple
·                           composition (AltGr+.)
```


# Remarks

Based on

2013 Functional language parser/interpreter https://github.com/davips/lamdheal-j


