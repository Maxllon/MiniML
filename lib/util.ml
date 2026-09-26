open Ast
open Lambda
open Interpreter

let rec type_term_to_string term tp =
  match tp with
  | TExc -> "exception"
  | TUnit -> "unit"
  | TInt ->
    (match term with
     | Int i -> string_of_int i
     | _ -> failwith "Util: Should never reach here!")
  | TBool ->
    (match term with
     | Fun (Fun (Var (Idx i))) when i = 0 || i = 1 -> if i = 1 then "True" else "False"
     | _ -> failwith "Util: Should never reach here")
  | TArrow _ -> "λχ.τ"
  | TTuple tuple ->
    let rec helper i =
      (function
        | first :: second :: rest ->
          let nth = ast_to_term (App (Var "nth", Int i)) in
          let el = eval (App (nth, term)) in
          type_term_to_string el first ^ ", " ^ helper (i + 1) (second :: rest)
        | first :: [] ->
          let nth = ast_to_term (App (Var "nth", Int i)) in
          let el = eval (App (nth, term)) in
          type_term_to_string el first
        | _ -> "")
    in
    "(" ^ helper 0 tuple ^ ")"
  | RecT _ -> "μχ.τ"
  | Scheme _ -> "∀χ.τ"
  | _ -> failwith "Util: Should never reach here!"
;;
