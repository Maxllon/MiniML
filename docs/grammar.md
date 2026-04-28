# Grammar

## Tokens

```
IDENT    ::= [a-zA-Z_][a-zA-Z0-9_]*
INT      ::= [0-9]+
BOOLEAN  ::= "true" | "false"
```

## Keywords

```
let, rec, if, then, else, in, \ (->)
```

## Operators

```
Arithmetic:  + - * /
Comparison: = != < <= > >=
Logical:    && (and) || (or) ! (not) ^ (xor)
```

## Grammar

```
<program> ::= <expr>

<expr> ::= <let-expr>

<let-expr> ::= "let" <ident> "=" <expr> "in" <expr>
             | "let" "rec" <ident> "=" <expr> "in" <expr>
             | <fun-expr>

<fun-expr> ::= "\" <ident>+ "->" <expr>
              | <if-expr>

<if-expr> ::= "if" <expr> "then" <expr> "else" <expr>
             | <eq-expr>

<eq-expr> ::= <or-expr>
            | <or-expr> "=" <eq-expr>
            | <or-expr> "!=" <eq-expr>

<or-expr> ::= <xor-expr>
            | <xor-expr> "||" <or-expr>

<xor-expr> ::= <and-expr>
             | <and-expr> "^" <xor-expr>

<and-expr> ::= <comp-expr>
             | <comp-expr> "&&" <and-expr>

<comp-expr> ::= <add-expr>
              | <add-expr> "<" <comp-expr>
              | <add-expr> "<=" <comp-expr>
              | <add-expr> ">" <comp-expr>
              | <add-expr> ">=" <comp-expr>

<add-expr> ::= <mul-expr>
            | <mul-expr> "+" <add-expr>
            | <mul-expr> "-" <add-expr>

<mul-expr> ::= <unary-expr>
             | <unary-expr> "*" <mul-expr>
             | <unary-expr> "/" <mul-expr>

<unary-expr> ::= "-" <unary-expr>
               | "!" <unary-expr>
               | <app-expr>

<app-expr> ::= <atom> <app-expr>
             | <atom>

<atom> ::= <int>
         | <ident>
         | "true"
         | "false"
         | "(" <expr> ")"
```