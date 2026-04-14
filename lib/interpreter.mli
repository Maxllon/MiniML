open Ast

type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * expr * env
  | VRecClosure of string * string * expr * env

and env = (string * value) list

val eval : env -> expr -> (value, string) result
val value_to_string : value -> string
