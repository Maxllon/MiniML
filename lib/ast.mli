type bin_op =
  | Add
  | Sub
  | Mult
  | Div
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | Xor

type un_op =
  | Not
  | Neg

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Let of string * expr * expr
  | Let_rec of string * expr * expr
  | Lambd of string * expr
  | App of expr * expr
  | If of expr * expr * expr
  | Bin_op of bin_op * expr * expr
  | Un_op of un_op * expr

val expr_to_string : expr -> string
