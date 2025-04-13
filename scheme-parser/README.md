## Scheme Syntax

In the simple case, language expressions consist of a single number or identifier.
In this case, the parse tree consists of a single node - either a number or an identifier.
```
5
+
foo-bar
```

An expression can be a pair. A pair is denoted by round brackets with a dot
between two elements.

An expression can also be a list.
A list is one of two entities:
1. An empty list. Denoted by empty parentheses - ().
2. A pair, in which the second element is a list. In this case, the first element of the pair is called the head of the list, and the second - the tail.

From this definition, a list of three elements can be written as:
`(1 . (2 . (3 . ())))`
```
pair => pair => pair => ()
  |       |       |
  1       2       3
```

Abbreviated notation:
```
(1 2 3)
```


This is how a "proper list" is written.

The language also supports "improper lists".
In such a list, the second element of the most nested pair is not an empty list.
```
(1 . (2 . 3))
pair => pair => 3
  |       |
  1       2
```


There is also an abbreviated notation for such lists:
```
(1 2 . 3)
```

In the above examples, the elements of lists can be any valid expressions
```
((1 . 2) (3 4) 5)
(1 () (2 3 4) 5)
```


## Recursive Descent

The grammar of the `scheme` language falls into the `LL(1)` class. This means that it is possible
to write a recursive descent parsing algorithm that looks ahead by only one token.

## Error Handling

In case the stream of tokens does not correspond to a correct expression, a `SyntaxError` exception is thrown.
