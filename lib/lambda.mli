open Ast

type var_type =
  | Idx of int
  | Name of string

type term =
  | Var of var_type
  | Fun of string * term
  | App of term * term
  | Int of int

val ltrue : term
val lfalse : term
val ast_to_term : expr -> term
val term_to_string : term -> string
