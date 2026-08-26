open Ast

type ml_type = 
  | TInt
  | TBool
  | TArrow of ml_type * ml_type
  | TVar of int

val get_type: expr -> (ml_type, string) result
val ml_type_to_string: ml_type -> string
