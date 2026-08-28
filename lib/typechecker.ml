open Ast

let get_bin_type = function
  | Add | Sub | Mult | Div -> TInt, TInt, TInt
  | Eq | Neq | Lt | Le | Gt | Ge -> TInt, TInt, TBool
  | And | Or | Xor -> TBool, TBool, TBool
;;

let get_un_type = function
  | Not -> TBool, TBool
  | Neg -> TInt, TInt
;;

let counter = ref (-1)

let fresh_var () =
  counter := !counter + 1;
  !counter
;;

let rec subst_c name n_type = function
  | TVar i when i = name -> n_type
  | TArrow (l, r) -> TArrow (subst_c name n_type l, subst_c name n_type r)
  | some -> some
;;

let rec occurs name = function
  | TVar i when i = name -> true
  | TArrow (l, r) -> occurs name l || occurs name r
  | _ -> false
;;

let rec unify = function
  | (ltype, _) :: [] -> ltype
  | (ltype, rtype) :: rest when ltype = rtype -> unify rest
  | (TVar name, tp) :: rest | (tp, TVar name) :: rest ->
    if occurs name tp
    then failwith "Occur check error"
    else unify (List.map (fun (a, b) -> subst_c name tp a, subst_c name tp b) rest)
  | (TArrow (l1, l2), TArrow (r1, r2)) :: rest -> unify ((l1, r1) :: (l2, r2) :: rest)
  | (RecT (l, _), RecT (r, _)) :: rest when l = r -> unify rest
  (*danger*)
  | (TTuple l, TTuple r) :: rest ->
    let rec check_size ((l, r) : ml_type list * ml_type list) : (ml_type * ml_type) list =
      match l, r with
      | l :: lrest, r :: rrest -> (l, r) :: check_size (lrest, rrest)
      | [], _ -> []
      | _ :: _, [] -> failwith "Tuple size match error"
    in
    unify (check_size (l, r) @ rest)
  (*danger*)
  | (ltype, rtype) :: _ ->
    failwith
      ("Cannot equalise:\n" ^ ml_type_to_string ltype ^ "\n" ^ ml_type_to_string rtype)
  | _ -> failwith "Should never reach here"

and ml_type_to_string = function
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (l, r) -> "(" ^ ml_type_to_string l ^ " -> " ^ ml_type_to_string r ^ ")"
  | TVar i -> "t" ^ string_of_int i
  | TVarS s -> "s: " ^ s
  | RecT (name, t) -> "rec " ^ name ^ "." ^ ml_type_to_string t
  | TTuple tuple ->
    let rec helper = function
      | first :: second :: rest ->
        ml_type_to_string first ^ " * " ^ helper (second :: rest)
      | tp :: [] -> ml_type_to_string tp
      | _ -> ""
    in
    "(" ^ helper tuple ^ ")"
;;

let rec set_equations (term : expr) (ctx : (string * ml_type) list)
  : ml_type * (ml_type * ml_type) list
  =
  match term with
  (*std*)
  | App (Var "nth", Int n) ->
    ( TArrow
        ( TTuple (List.init (n + 1) (fun _ -> TVar (fresh_var ())))
        , TVar (fresh_var () + n + 1) )
    , [] )
  (*std*)
  | Var name ->
    (match List.assoc_opt name ctx with
     | Some t -> t, []
     | _ -> failwith ("Unbound variable: " ^ "\"" ^ name ^ "\""))
  | Int _ -> TInt, []
  | Bool _ -> TBool, []
  | Lambd (name, body) ->
    let arg_type = TVar (fresh_var ()) in
    let body_type, c = set_equations body ((name, arg_type) :: ctx) in
    TArrow (arg_type, body_type), c
  | App (left, right) ->
    let ltype, lc = set_equations left ctx in
    let rtype, rc = set_equations right ctx in
    let exprT = TVar (fresh_var ()) in
    exprT, (ltype, TArrow (rtype, exprT)) :: (lc @ rc)
  | If (cnd, th, els) ->
    let cnd_type, cnd_c = set_equations cnd ctx in
    let th_type, th_c = set_equations th ctx in
    let els_type, els_c = set_equations els ctx in
    let new_c = [ cnd_type, TBool; th_type, els_type ] in
    th_type, new_c @ cnd_c @ th_c @ els_c
  | Bin_op (op, left, right) ->
    let exc_l, exc_r, exc_res = get_bin_type op in
    let ltype, lc = set_equations left ctx in
    let rtype, rc = set_equations right ctx in
    exc_res, (exc_l, ltype) :: (exc_r, rtype) :: (lc @ rc)
  | Un_op (op, value) ->
    let exc_t, exc_res = get_un_type op in
    let vtype, vc = set_equations value ctx in
    exc_res, (exc_t, vtype) :: vc
  | Let_rec (name, value, body) ->
    let f_type = TVar (fresh_var ()) in
    let value_type, value_c = set_equations value ((name, f_type) :: ctx) in
    let body_type, body_c = set_equations body ((name, f_type) :: ctx) in
    body_type, (f_type, value_type) :: (value_c @ body_c)
  | Let (name, value, body) ->
    let value_type, value_c = set_equations value ctx in
    let body_type, body_c = set_equations body ((name, value_type) :: ctx) in
    body_type, value_c @ body_c
  | Tuple tuple ->
    let rec helper = function
      | value :: rest when rest != [] ->
        let value_type, value_c = set_equations value ctx in
        let rest_type, rest_c = helper rest in
        value_type :: rest_type, value_c @ rest_c
      | value :: [] ->
        let value_type, value_c = set_equations value ctx in
        [ value_type ], value_c
      | _ -> failwith "(Typecheker): Should never reach here!"
    in
    let type_list, c = helper tuple in
    TTuple type_list, c
  | Constr (_, tp) -> tp, []
  | Case (folded, cases) ->
    let folded_type, folded_c = set_equations folded ctx in
    let rec helper cases pref_type c : ml_type * (ml_type * ml_type) list =
      match cases with
      | [] -> pref_type, c
      | (f, name, body) :: rest ->
        let arg_type = TVar (fresh_var ()) in
        let body_type, body_c = set_equations body ((name, arg_type) :: ctx) in
        let f_type, f_c = set_equations f ctx in
        let new_c = [ f_type, TArrow (arg_type, folded_type); body_type, pref_type ] in
        helper rest body_type (body_c @ f_c @ new_c @ c)
    in
    helper cases (TVar (fresh_var ())) folded_c
;;

let get_type ast =
  try
    let res_type, c = set_equations ast [] in
    Ok (unify (List.rev ((res_type, res_type) :: c)))
  with
  | Failure s -> Error s
;;
