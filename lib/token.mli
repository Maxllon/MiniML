type keyword =
  | LET
  | REC
  | IF
  | THEN
  | ELSE
  | IN
  | LAMBD
  | ARROW

type boolean =
  | TRUE
  | FALSE

type bracket =
  | L_PAREN
  | R_PAREN

type operator =
  | PLUS
  | MINUS
  | DIV
  | MULT
  | EQ
  | NEQ
  | LT
  | LE
  | GT
  | GE
  | AND
  | OR
  | NOT
  | XOR

type token =
  | VAR of string
  | INT of int
  | BRACKET of bracket
  | KEYWORD of keyword
  | BOOLEAN of boolean
  | OPERATOR of operator
  | EOF
  | INVALID

val parseToken : string -> int * token
val tokenToString: token -> string
