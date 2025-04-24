# scheme-tokenizer
The tokenizer takes a sequence of characters as input
and returns a sequence of tokens.

Example of an expression and the stream of tokens:
```
'(+ 4 -5)

Quote OpenParen Symbol(+) Const(4) Const(-5) CloseParen
```

Types of tokens:
  - Number
  - Bracket ( or )
  - Quote `'`
  - Dot `.`
  - Symbols, for example, a variable `x` or a function `+`. A symbol starts with characters `[a-z<=>*#]`
    and may additionally contain the characters `-`, `+`, `?`. Additionally,
    there are exception symbols `+`, `-` and `*`.