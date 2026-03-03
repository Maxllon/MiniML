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

let isWhitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let isDigit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let isOp c = String.contains "\\+-/*=!><&|^" c
let isBracket c = String.contains "()" c
let isWordPart c = not (isBracket c || isWhitespace c || isOp c)

let kwFromString = function
  | "let" -> Some LET
  | "rec" -> Some REC
  | "if" -> Some IF
  | "then" -> Some THEN
  | "else" -> Some ELSE
  | "in" -> Some IN
  | "\\" -> Some LAMBD
  | "->" -> Some ARROW
  | _ -> None
;;

let boolFromString = function
  | "true" -> Some TRUE
  | "false" -> Some FALSE
  | _ -> None
;;

let opFromString = function
  | "+" -> Some PLUS
  | "-" -> Some MINUS
  | "/" -> Some DIV
  | "*" -> Some MULT
  | "=" -> Some EQ
  | "!=" -> Some NEQ
  | "<" -> Some LT
  | "<=" -> Some LE
  | ">" -> Some GT
  | ">=" -> Some GE
  | "&&" | "and" -> Some AND
  | "||" | "or" -> Some OR
  | "!" | "not" -> Some NOT
  | "^" | "xor" -> Some XOR
  | _ -> None
;;

let intFromString s =
  try Some (int_of_string s) with
  | _ -> None
;;

let parseNumber s =
  let len = String.length s in
  let rec findEnd pos = if pos < len && isDigit s.[pos] then findEnd (pos + 1) else pos in
  let res = findEnd 0 in
  let sNum = String.sub s 0 res in
  match intFromString sNum with
  | Some n -> res, INT n
  | None -> res, INVALID
;;

let parseBracket s =
  match s.[0] with
  | '(' -> 1, BRACKET L_PAREN
  | ')' -> 1, BRACKET R_PAREN
  | _ -> 1, INVALID
;;

let parseOp s =
  let len = String.length s in
  let rec findEnd pos = if pos < len && isOp s.[pos] then findEnd (pos + 1) else pos in
  let res = findEnd 0 in
  let sOp = String.sub s 0 res in
  match kwFromString sOp with
  | Some kw -> res, KEYWORD kw
  | None ->
    (match opFromString sOp with
     | Some op -> res, OPERATOR op
     | None -> res, INVALID)
;;

let parseWord s =
  let len = String.length s in
  let rec findEnd pos =
    if pos < len && isWordPart s.[pos] then findEnd (pos + 1) else pos
  in
  let res = findEnd 0 in
  let word = String.sub s 0 res in
  match kwFromString word with
  | Some kw -> res, KEYWORD kw
  | None ->
    (match opFromString word with
     | Some op -> res, OPERATOR op
     | None ->
       (match boolFromString word with
        | Some bool -> res, BOOLEAN bool
        | None -> res, VAR word))
;;

let rec parseToken s =
  let len = String.length s in
  if len = 0
  then 0, EOF
  else (
    match s.[0] with
    | c when isWhitespace c ->
      let res, token = parseToken (String.sub s 1 (len - 1)) in
      res + 1, token
    | c when isDigit c -> parseNumber s
    | c when isBracket c -> parseBracket s
    | c when isOp c -> parseOp s
    | _ -> parseWord s)
;;

let tokenToString = function
  | VAR s -> "VAR(" ^ s ^ ")"
  | INT n -> "INT(" ^ string_of_int n ^ ")"
  | BRACKET L_PAREN -> "L_PAREN"
  | BRACKET R_PAREN -> "R_PAREN"
  | KEYWORD LET -> "LET"
  | KEYWORD REC -> "REC"
  | KEYWORD IF -> "IF"
  | KEYWORD THEN -> "THEN"
  | KEYWORD ELSE -> "ELSE"
  | KEYWORD IN -> "IN"
  | KEYWORD LAMBD -> "LAMBD"
  | KEYWORD ARROW -> "ARROW"
  | BOOLEAN TRUE -> "TRUE"
  | BOOLEAN FALSE -> "FALSE"
  | OPERATOR PLUS -> "PLUS"
  | OPERATOR MINUS -> "MINUS"
  | OPERATOR DIV -> "DIV"
  | OPERATOR MULT -> "MULT"
  | OPERATOR EQ -> "EQ"
  | OPERATOR NEQ -> "NEQ"
  | OPERATOR LT -> "LT"
  | OPERATOR LE -> "LE"
  | OPERATOR GT -> "GT"
  | OPERATOR GE -> "GE"
  | OPERATOR AND -> "AND"
  | OPERATOR OR -> "OR"
  | OPERATOR NOT -> "NOT"
  | OPERATOR XOR -> "XOR"
  | EOF -> "EOF"
  | INVALID -> "INVALID"
;;
