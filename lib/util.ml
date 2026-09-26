open Ast
open Lambda
open Interpreter

let nth i = ast_to_term (App (Var "nth", Int i))

let bit term i =
  match eval (App (nth i, term)) with
  | Fun (Fun (Var (Idx 1))) -> 1
  | _ -> 0
;;

let rec type_term_to_string term tp =
  match tp with
  | TExc -> "exception"
  | TUnit -> "unit"
  | TInt ->
    string_of_int
      (List.fold_left (fun acc i -> acc lor (bit term i lsl i)) 0 (List.init 32 Fun.id))
  | TBool ->
    (match term with
     | Fun (Fun (Var (Idx i))) when i = 0 || i = 1 -> if i = 1 then "True" else "False"
     | _ -> failwith "Util: Should never reach here")
  | TArrow _ -> "λχ.τ"
  | TTuple tuple ->
    let rec helper i =
      (function
        | first :: second :: rest ->
          let el = eval (App (nth i, term)) in
          type_term_to_string el first ^ ", " ^ helper (i + 1) (second :: rest)
        | first :: [] ->
          let el = eval (App (nth i, term)) in
          type_term_to_string el first
        | _ -> "")
    in
    "(" ^ helper 0 tuple ^ ")"
  | RecT _ -> "μχ.τ"
  | Scheme _ -> "∀χ.τ"
  | _ -> failwith "Util: Should never reach here!"
;;
