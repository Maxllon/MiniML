open Ast

type ml_type =
  | TInt
  | TBool
  | TArrow of ml_type * ml_type
  | TVar of int

let counter = ref 0

let fresh_var =
  counter := !counter + 1;
  !counter
;;

let rec subst_c name n_type tp =
  match tp with
  | TVar i when i = name -> n_type
  | TArrow (l, r) -> TArrow (subst_c name n_type l, subst_c name n_type r)
  | some -> some
;;

let rec unify = function
  | (ltype, _) :: [] -> ltype
  | (ltype, rtype) :: rest when ltype = rtype -> unify rest
  | (TVar name, tp) :: rest | (tp, TVar name) :: rest ->
    unify (List.map (fun (a, b) -> subst_c name tp a, subst_c name tp b) rest)
  | (TArrow (l1, l2), TArrow (r1, r2)) :: rest -> unify ((l1, r1) :: (l2, r2) :: rest)
  | _ -> failwith "Should never reach here"
;;

let rec set_equations (term : expr) (ctx : (string * ml_type) list)
  : ml_type * (ml_type * ml_type) list
  =
  match term with
  | Var name ->
    (match List.assoc_opt name ctx with
     | Some t -> t, []
     | _ -> failwith "Unbound variable")
  | Int _ -> TInt, []
  | Bool _ -> TBool, []
  | Lambd (name, body) ->
    let arg_type = TVar fresh_var in
    let body_type, c = set_equations body ((name, arg_type) :: ctx) in
    TArrow (arg_type, body_type), c
  | App (left, right) ->
    let ltype, lc = set_equations left ctx in
    let rtype, rc = set_equations right ctx in
    let exprT = TVar fresh_var in
    exprT, (ltype, TArrow (rtype, exprT)) :: (lc @ rc)
  | If (cnd, th, els) ->
    let cnd_type, cnd_c = set_equations cnd ctx in
    let th_type, th_c = set_equations th ctx in
    let els_type, els_c = set_equations els ctx in
    let new_c = [ cnd_type, TBool; th_type, els_type ] in
    th_type, new_c @ cnd_c @ th_c @ els_c
  | Bin_op _ -> failwith "Bin op not supported"
  | Un_op _ -> failwith "Un op not supported"
  | Let_rec (name, value, body) ->
    let f_type = TVar fresh_var in
    let value_type, value_c = set_equations value ((name, f_type) :: ctx) in
    let body_type, body_c = set_equations body ((name, f_type) :: ctx) in
    body_type, (f_type, value_type) :: (value_c @ body_c)
  | Let (name, value, body) ->
    let value_type, value_c = set_equations value ctx in
    let body_type, body_c = set_equations body ((name, value_type) :: ctx) in
    body_type, value_c @ body_c
;;

let get_type ast =
  try
    let res_type, c = set_equations ast [] in
    Ok (unify (List.rev ((res_type, res_type) :: c)))
  with
  | Failure s -> Error s
;;

let rec ml_type_to_string = function
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (l, r) -> "(" ^ ml_type_to_string l ^ " -> " ^ ml_type_to_string r ^ ")"
  | TVar i -> "t" ^ string_of_int i
;;
