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

type ml_type =
  | TExc
  | TUnit
  | TInt
  | TBool
  | TArrow of ml_type * ml_type
  | TVar of int
  | TVarS of string
  | TTuple of ml_type list
  | RecT of string * ml_type

type expr =
  | Unit
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
  | Tuple of expr list
  | Constr of string * int * int * ml_type
  | Case of expr * (expr * string * expr) list
  | Try of expr * expr

let rec expr_to_string = function
  | Unit -> "unit"
  | Var s -> s
  | Int n -> string_of_int n
  | Bool x -> string_of_bool x
  | Let (name, value, body) ->
    "("
    ^ "let "
    ^ name
    ^ " = "
    ^ expr_to_string value
    ^ " in "
    ^ expr_to_string body
    ^ ")"
  | Let_rec (name, value, body) ->
    "("
    ^ "let rec "
    ^ name
    ^ " = "
    ^ expr_to_string value
    ^ " in "
    ^ expr_to_string body
    ^ ")"
  | Lambd (arg, body) -> "(" ^ "\\" ^ arg ^ "." ^ expr_to_string body ^ ")"
  | App (f, arg) -> "(" ^ expr_to_string f ^ " " ^ expr_to_string arg ^ ")"
  | If (cond, then_body, else_body) ->
    "("
    ^ "if "
    ^ expr_to_string cond
    ^ " then "
    ^ expr_to_string then_body
    ^ " else "
    ^ expr_to_string else_body
    ^ ")"
  | Bin_op (op, left, right) ->
    let string_of_op = function
      | Add -> "+"
      | Sub -> "-"
      | Div -> "/"
      | Mult -> "*"
      | Eq -> "="
      | Neq -> "!="
      | Lt -> "<"
      | Le -> "<="
      | Gt -> ">"
      | Ge -> ">="
      | And -> "and"
      | Or -> "or"
      | Xor -> "xor"
    in
    "(" ^ expr_to_string left ^ " " ^ string_of_op op ^ " " ^ expr_to_string right ^ ")"
  | Un_op (op, expr) ->
    let string_of_op = function
      | Not -> "not"
      | Neg -> "-"
    in
    "(" ^ string_of_op op ^ "(" ^ expr_to_string expr ^ "))"
  | Tuple tuple ->
    let rec helper = function
      | first :: second :: rest -> expr_to_string first ^ ", " ^ helper (second :: rest)
      | expr :: [] -> expr_to_string expr
      | _ -> ""
    in
    "(" ^ helper tuple ^ ")"
  | Constr (name, _, _, _) -> name
  | Case (scrutinee, branches) ->
    let rec string_of_branches = function
      | (cname, var, body) :: rest ->
        expr_to_string cname
        ^ " "
        ^ var
        ^ " => "
        ^ expr_to_string body
        ^ if rest = [] then "" else " | " ^ string_of_branches rest
      | [] -> ""
    in
    "(case " ^ expr_to_string scrutinee ^ " of " ^ string_of_branches branches ^ ")"
  | Try (try_val, catch) ->
    "(try " ^ expr_to_string try_val ^ " with " ^ expr_to_string catch ^ ")"
;;
