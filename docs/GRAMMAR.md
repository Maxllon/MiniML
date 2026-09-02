# MiniML — BNF Grammar

## Notation

- `::=` defines a production
- `|` separates alternatives
- `{ X }` means zero or more repetitions of X
- `[ X ]` means X is optional
- Lowercase identifiers are non-terminals
- Uppercase identifiers are tokens (terminals)
- `(X)` groups alternatives

---

## 1. Program

```bnf
<program> ::= <expr> EOF
```

## 2. Expressions (precedence: lowest → highest)

```bnf
<expr> ::= <type_decl>
         | <let_expr> | <lambda> | <try_expr> | <if_expr> | <case_expr>
         | <tuple>
```

### Type declarations

```bnf
<type_decl> ::= "type" IDENT "=" <type_body> "in" <expr>
```

A type declaration introduces a set of data constructors in scope for the
remainder of `<expr>`. See [Section 3](#3-type-expressions-used-in-type-declarations).

### Let bindings

```bnf
<let_expr> ::= "let" "rec" IDENT { IDENT } "=" <expr> "in" <expr>
             | "let" IDENT { IDENT } "=" <expr> "in" <expr>
```

Multi-argument let desugars to nested lambdas:
`let f a b = e in body` ≡ `let f = \a. \b. e in body`

### Lambda abstraction

```bnf
<lambda>   ::= "\" { IDENT } "." <expr>
```

Multi-argument lambda desugars to nested lambdas:
`\x y. e` ≡ `\x. \y. e`

### Try / With

```bnf
<try_expr> ::= "try" <expr> "with" <expr>
```

### Conditional

```bnf
<if_expr>  ::= ("if" | "lif") <expr> "then" <expr> "else" <expr>
```

`lif` wraps both branches in lambdas and applies to a dummy argument (lazy evaluation).

### Pattern matching

```bnf
<case_expr>   ::= "case" <expr> "of" <case_branch> { "|" <case_branch> }
<case_branch> ::= IDENT IDENT "=>" <expr>
```

The first `IDENT` names a constructor of the scrutinee's type, the second is
the variable bound to the constructor's payload in the branch body.
All constructors of the type must be present (exhaustiveness check at parse time).

### Tuple

```bnf
<tuple> ::= <eq_expr> "," <eq_expr> { "," <eq_expr> }
```

The head of the top-level precedence chain is `parse_seq`, whose elements are
`<eq_expr>` (see below). For a single-element tuple the bare expression is used;
`(e)` is a parenthesized expression, not a 1-tuple.

### Binary operators (precedence: lowest → highest, all right-associative)

```bnf
<binop_expr> ::= <eq_expr>

<eq_expr>    ::= <or_expr>  { ("=" | "!=") <eq_expr> }

<or_expr>    ::= <xor_expr>  { ("||"  | "or")  <or_expr>  }
<xor_expr>   ::= <and_expr>  { ("^"   | "xor") <xor_expr> }
<and_expr>   ::= <comp_expr> { ("&&"  | "and") <and_expr> }
<comp_expr>  ::= <add_expr>  { ("<" | "<=" | ">" | ">=") <comp_expr> }
<add_expr>   ::= <mul_expr>  { ("+" | "-") <add_expr> }
<mul_expr>   ::= <unary_expr> { ("*" | "/") <mul_expr> }
```

Equality (`=`, `!=`) binds looser than the logical operators but tighter than
tuples, i.e. `a = b or c` parses as `a = (b or c)`.

### Unary operators

```bnf
<unary_expr> ::= ("-" | "not" | "!") <atom>
               | <app_expr>
```

Note that `("-" | "not" | "!")` applies to a *single atom* (not a full unary
expression), so parenthesise to negate a compound expression: `- (a + b)`.

### Function application (left-associative)

```bnf
<app_expr>   ::= <app_expr> <atom>
               | <atom>
```

In recursive descent this is implemented iteratively:
parse an atom, then keep consuming atoms and wrapping in `App(...)`.

### Atoms (highest precedence)

```bnf
<atom> ::= INT_LITERAL
         | "true" | "false"
         | IDENT
         | "unit"
         | "(" <expr> ")"
```

---

## 3. Type Expressions (used in type declarations)

```bnf
<type> ::= <type_arrow>

<type_arrow>  ::= <type_tuple> { "->" <type_arrow> }
<type_tuple>  ::= <type_atom>  { "*" <type_atom> }
<type_atom>   ::= "Int" | "Bool" | "Unit"
                | IDENT                      -- self-reference to declared type
                | "(" <type> ")"
```

Tuple types have higher precedence than arrow: `Int * Bool -> Unit`
parses as `(Int * Bool) -> Unit`.

Arrow is right-associative: `Int -> Bool -> Unit`
parses as `Int -> (Bool -> Unit)`.

### Type declaration construction

```bnf
<type_decl>   ::= "type" IDENT "=" <type_body> "in" <expr>
<type_body>   ::= <type_constr> { "|" <type_constr> }
<type_constr> ::= IDENT "of" <type>
```

The IDENT immediately after `"type"` is the name being declared; a `type_constr`
constructor may `"of"` `IDENT` only when that IDENT is the self-reference to the
type currently being declared.

---

## 4. Tokens

```
IDENT          [a-zA-Z_][a-zA-Z0-9_]*
INT_LITERAL    [0-9]+
"true"         true
"false"        false
"unit"         unit
"let"          let
"rec"          rec
"in"           in
"if"           if
"lif"          lif
"then"         then
"else"         else
"try"          try
"with"         with
"type"         type
"of"           of
"case"         case
"\"            \
"."            .
","            ,
"->"           ->
"=>"           =>
"|"            |
"Int"          Int
"Bool"         Bool
"Unit"         Unit
"+"  "-"  "*"  "/"
"="  "!="
"<"  "<="  ">"  ">="
"&&"  "||"  "^"
"and" "or" "not" "xor"
"!"            !
"("  ")"
```

---

## 5. Full grammar summary (single-rule-per-level, RD-friendly)

```bnf
<program>     ::= <expr> EOF

<expr>        ::= "type" IDENT "=" <type_body> "in" <expr>
                | "let" "rec"? IDENT { IDENT } "=" <expr> "in" <expr>
                | "\" { IDENT } "." <expr>
                | "try" <expr> "with" <expr>
                | ("if" | "lif") <expr> "then" <expr> "else" <expr>
                | "case" <expr> "of" <branch> { "|" <branch> }
                | <tuple>

<tuple>       ::= <eq> { "," <eq> }

<eq>          ::= <or>  { ("=" | "!=") <eq> }

<or>          ::= <xor>  { ("||"  | "or")  <or>  }
<xor>         ::= <and>  { ("^"   | "xor") <xor> }
<and>         ::= <comp> { ("&&"  | "and") <and> }
<comp>        ::= <add>  { ("<" | "<=" | ">" | ">=") <comp> }
<add>         ::= <mul>  { ("+" | "-") <add> }
<mul>         ::= <unary> { ("*" | "/") <mul> }

<unary>       ::= ("-" | "not" | "!") <atom>
                | <app>

<app>         ::= <app> <atom>       -- left-recursive, impl. iteratively
                | <atom>

<atom>        ::= INT_LITERAL
                | ("true" | "false")
                | IDENT
                | "unit"
                | "(" <expr> ")"

<branch>      ::= IDENT IDENT "=>" <expr>

<type_body>   ::= <type_constr> { "|" <type_constr> }
<type_constr> ::= IDENT "of" <type>

<type>        ::= <type_arrow>
<type_arrow>  ::= <type_tuple> { "->" <type_arrow> }
<type_tuple>  ::= <type_atom> { "*" <type_atom> }
<type_atom>   ::= "Int" | "Bool" | "Unit" | IDENT | "(" <type> ")"
```
