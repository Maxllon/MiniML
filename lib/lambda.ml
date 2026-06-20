open Ast

type term =
  | Var of string
  | Fun of string * term
  | App of term * term
  | Int of int

let ltrue = Fun ("x", Fun ("y", Var "x"))
let lfalse = Fun ("x", Fun ("y", Var "y"))

let rec ast_to_term : expr -> term = function
  | Var s -> Var s
  | Int v -> Int v
  | Bool v ->
    (match v with
     | true -> ltrue
     | false -> lfalse)
  | Let (name, value, body) -> App (Fun (name, ast_to_term body), ast_to_term value)
  | Let_rec (name, value, body) ->
    let helper =
      Fun ("h", App (Var "g", Fun ("x", App (App (Var "h", Var "h"), Var "x"))))
    in
    let z = Fun ("g", App (helper, helper)) in
    App (Fun (name, ast_to_term body), App (z, Fun (name, ast_to_term value)))
  | Lambd (name, expr) -> Fun (name, ast_to_term expr)
  | App (expr, expr') -> App (ast_to_term expr, ast_to_term expr')
  | If (cond, th, els) -> App (App (ast_to_term cond, ast_to_term th), ast_to_term els)
  | Bin_op (op, a, b) -> bin_to_term op (ast_to_term a) (ast_to_term b)
  | Un_op (op, expr) -> un_to_term op (ast_to_term expr)

and bin_to_term op a b =
  let builder op' a' b' = App (App (Var op', a'), b') in
  match op with
  | Add -> builder "+" a b
  | Sub -> builder "-" a b
  | Mult -> builder "*" a b
  | Div -> builder "/" a b
  | Eq -> builder "=" a b
  | Neq -> un_to_term Not (builder "=" a b)
  | Lt -> builder "<" a b
  | Le -> builder "<=" a b
  | Gt -> builder ">" a b
  | Ge -> builder ">=" a b
  | And -> App (App (a, b), lfalse)
  | Or -> App (App (a, ltrue), b)
  | Xor -> App (App (a, un_to_term Not b), b)

and un_to_term op term =
  match op with
  | Not -> App (App (term, lfalse), ltrue)
  | Neg -> bin_to_term Sub (Int 0) term
;;

let rec term_to_string = function
  | Var s -> s
  | Int v -> string_of_int v
  | Fun (name, body) -> "(λ" ^ name ^ "." ^ term_to_string body ^ ")"
  | App (term, term') -> "(" ^ term_to_string term ^ " " ^ term_to_string term' ^ ")"
;;
