open Ast

val get_type : expr -> (ml_type, string) result
val ml_type_to_string : ml_type -> string
