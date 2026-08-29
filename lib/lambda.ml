open Ast

exception FreeVar

type var_type =
  | Idx of int
  | Name of string

type term =
  | Var of var_type
  | Fun of term
  | App of term * term
  | Int of int

let ch_true = Lambd ("x", Lambd ("y", Var "x"))
let ch_false = Lambd ("x", Lambd ("y", Var "y"))
let ltrue = Fun (Fun (Var (Idx 1)))
let lfalse = Fun (Fun (Var (Idx 0)))

let rec find_pos n name = function
  | name' :: _ when name' = name -> n
  | _ :: rest -> find_pos (n + 1) name rest
  | _ -> raise FreeVar
;;

let try_std (e : expr) : expr =
  let first = Lambd ("f", App (Var "f", ch_true)) in
  let second = Lambd ("f", App (Var "f", ch_false)) in
  match e with
  | App (Var "nth", Int 0) -> Lambd ("g", App (first, Var "g"))
  | App (Var "nth", Int i) ->
    let rec helper n : expr = if n = 0 then Var "g" else App (second, helper (n - 1)) in
    Lambd ("g", helper i)
  | _ -> e
;;

let rec compile (ctx : string list) (e : expr) : term =
  match try_std e with
  | Unit -> compile ctx (Lambd ("x", Var "x"))
  | Var s ->
    (try
       let i = find_pos 0 s ctx in
       Var (Idx i)
     with
     | FreeVar -> Var (Name s))
  | Int v -> Int v
  | Bool v ->
    (match v with
     | true -> ltrue
     | false -> lfalse)
  | Let (name, value, body) -> compile ctx (App (Lambd (name, body), value))
  | Let_rec (name, value, body) ->
    let z_expr =
      let helper_expr : expr =
        Lambd ("x", App (Var "f", Lambd ("y", App (App (Var "x", Var "x"), Var "y"))))
      in
      Lambd ("f", App (helper_expr, helper_expr))
    in
    compile ctx (App (Lambd (name, body), App (z_expr, Lambd (name, value))))
  | Lambd (name, expr) -> Fun (compile (name :: ctx) expr)
  | App (expr, expr') -> App (compile ctx expr, compile ctx expr')
  | If (cond, th, els) -> App (App (compile ctx cond, compile ctx th), compile ctx els)
  | Bin_op (op, a, b) -> bin_to_term op (compile ctx a) (compile ctx b)
  | Un_op (op, expr) -> un_to_term op (compile ctx expr)
  | Tuple tuple -> compile ctx (compile_tuple tuple)
  | Constr (_, idx, total, _) ->
    let rec helper i : expr =
      if i = total
      then App (Var ("a" ^ string_of_int idx), Var "x")
      else Lambd ("a" ^ string_of_int i, helper (i + 1))
    in
    compile ctx (Lambd ("x", helper 0))
  | Case (a, case_body) ->
    let rec helper acc case_body : expr =
      match case_body with
      | [] -> acc
      | (_, name, body) :: rest -> helper (App (acc, Lambd (name, body))) rest
    in
    compile ctx (helper a case_body)

and compile_tuple = function
  | [ expr ] -> expr
  | expr :: rest -> Lambd ("f", App (App (Var "f", expr), compile_tuple rest))
  | _ -> failwith "(Compiler): Should never reach here!"

and bin_to_term op a b =
  let builder op' a' b' = App (App (Var (Name op'), a'), b') in
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

let ast_to_term (e : expr) = compile [] e

let rec term_to_string = function
  | Var (Name s) -> s
  | Var (Idx i) -> "i" ^ string_of_int i
  | Int v -> string_of_int v
  | Fun body -> "(λ" ^ "." ^ term_to_string body ^ ")"
  | App (term, term') -> "(" ^ term_to_string term ^ " " ^ term_to_string term' ^ ")"
;;
