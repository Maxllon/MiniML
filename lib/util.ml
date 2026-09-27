open Ast
open Lambda
open Interpreter

let nth i = ast_to_term (App (Var "nth", Int i))

let apply fn arg =
  match fn with
  | VClosure (body, env) -> eval (arg :: env) body
  | _ -> VError
;;

let bit term i =
  match apply (eval [] (nth i)) term with
  | VClosure (Fun (Var (Idx 1)), _) -> 1
  | VBool true -> 1
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
     | VClosure (Fun (Var (Idx i)), _) when i = 0 || i = 1 ->
       if i = 1 then "True" else "False"
     | VBool b -> if b then "True" else "False"
     | _ -> failwith "Util: Should never reach here")
  | TArrow _ -> "λχ.τ"
  | TTuple tuple ->
    let rec helper i =
      (function
        | first :: second :: rest ->
          let el = apply (eval [] (nth i)) term in
          type_term_to_string el first ^ ", " ^ helper (i + 1) (second :: rest)
        | first :: [] ->
          let el = apply (eval [] (nth i)) term in
          type_term_to_string el first
        | _ -> "")
    in
    "(" ^ helper 0 tuple ^ ")"
  | RecT _ -> "μχ.τ"
  | Scheme _ -> "∀χ.τ"
  | _ -> failwith "Util: Should never reach here!"
;;
