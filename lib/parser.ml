open Ast
open Token

type constr_reg = (string * (int * int)) list

let sort_and_check (reg : constr_reg) (branches : (expr * string * expr) list)
  : (expr * string * expr) list
  =
  let name_of = function
    | Var n -> n
    | _ -> failwith "case branch must reference a constructor by name"
  in
  let indexed =
    List.map
      (fun (e, var, body) ->
         let name = name_of e in
         match List.assoc_opt name reg with
         | Some (idx, total) -> idx, total, (e, var, body)
         | None -> failwith ("unknown constructor: " ^ name))
      branches
  in
  let total =
    match indexed with
    | (_, total, _) :: _ -> total
    | [] -> failwith "case has no branches"
  in
  if List.length indexed <> total
  then
    failwith
      ("case is not exhaustive: "
       ^ string_of_int (List.length indexed)
       ^ " branch(es), expected "
       ^ string_of_int total)
  else (
    let sorted = List.sort (fun (i1, _, _) (i2, _, _) -> Int.compare i1 i2) indexed in
    let sorted_indices = List.map (fun (i, _, _) -> i) sorted in
    if sorted_indices = List.init total (fun i -> i)
    then List.map (fun (_, _, branch) -> branch) sorted
    else failwith "case constructors are duplicated or not contiguous")
;;

let rec parse_with_reg reg tk_list =
  try
    match parse_expr reg tk_list with
    | ast, [] -> Ok ast
    | ast, [ EOF ] -> Ok ast
    | _, _ -> Error "Extra tokens"
  with
  | Failure s -> Error s

and parse_expr reg tk_list = parse_let reg tk_list

and parse_let reg tk_list =
  match tk_list with
  | KEYWORD TYPE :: VAR type_name :: OPERATOR EQ :: rest ->
    let constrs, rest' = parse_type_body type_name rest in
    (match rest' with
     | KEYWORD IN :: rest'' ->
       let reg' = List.mapi (fun i (name, _) -> name, (i, List.length constrs)) constrs in
       let in_expr, rest''' = parse_expr reg' rest'' in
       let rec unfold t =
         match t with
         | RecT (x, TVarS y) when x = y -> TVarS y
         | RecT (x, body) -> RecT (x, unfold body)
         | TArrow (a, b) -> TArrow (unfold a, unfold b)
         | TTuple items -> TTuple (List.map unfold items)
         | other -> other
       in
       let build constrs body =
         List.fold_right
           (fun (name, payload) acc ->
              let idx, total = List.assoc name reg' in
              Let
                ( name
                , Constr
                    (name, idx, total, TArrow (payload, RecT (type_name, unfold payload)))
                , acc ))
           constrs
           body
       in
       build constrs in_expr, rest'''
     | _ -> failwith "expected in after type declaration")
  | KEYWORD LET :: KEYWORD REC :: VAR name :: rest ->
    parse_let_body
      reg
      []
      (fun (name', expr', in_expr') -> Let_rec (name', expr', in_expr'))
      name
      rest
  | KEYWORD LET :: VAR name :: rest ->
    parse_let_body
      reg
      []
      (fun (name', expr', in_expr') -> Let (name', expr', in_expr'))
      name
      rest
  | _ -> parse_fun reg tk_list

and parse_let_body reg args constructor name tk_list =
  match tk_list with
  | VAR n :: r -> parse_let_body reg (n :: args) constructor name r
  | OPERATOR EQ :: r ->
    let value, rest = parse_expr reg r in
    let rec build args' expr' =
      match args' with
      | [] -> expr'
      | s :: args'' -> Lambd (s, build args'' expr')
    in
    let value = build args value in
    (match rest with
     | KEYWORD IN :: rest' ->
       let body, rest'' = parse_expr reg rest' in
       constructor (name, value, body), rest''
     | _ -> failwith "Expected in after let/let rec")
  | _ -> failwith "Expected \"=\" after let/let rec"

and parse_fun reg tk_list =
  match tk_list with
  | KEYWORD LAMBD :: rest -> parse_args reg [] rest
  | _ -> parse_if reg tk_list

and parse_args reg args tk_list =
  match tk_list with
  | VAR name :: rest -> parse_args reg (name :: args) rest
  | KEYWORD DOT :: rest ->
    let expr, rest' = parse_expr reg rest in
    let rec build args' expr' =
      match args' with
      | [] -> expr'
      | s :: args'' -> Lambd (s, build args'' expr')
    in
    build (List.rev args) expr, rest'
  | _ -> failwith "Expected args or DOT keyword"

and parse_if reg tk_list =
  match tk_list with
  | KEYWORD n :: rest when n = IF || n = LIF ->
    let cond, rest' = parse_expr reg rest in
    (match rest' with
     | KEYWORD THEN :: rest'' ->
       let then_expr, rest''' = parse_expr reg rest'' in
       (match rest''' with
        | KEYWORD ELSE :: rest'''' ->
          let else_expr, rest''''' = parse_expr reg rest'''' in
          if n = LIF
          then (
            let then_expr = Lambd ("x", then_expr) in
            let else_expr = Lambd ("x", else_expr) in
            App (If (cond, then_expr, else_expr), Int 0), rest''''')
          else If (cond, then_expr, else_expr), rest'''''
        | _ -> failwith "expected else keyword")
     | _ -> failwith "expected then keyword")
  | _ -> parse_case reg tk_list

and parse_case reg tk_list =
  match tk_list with
  | KEYWORD CASE :: rest ->
    let scrutinee, rest' = parse_expr reg rest in
    (match rest' with
     | KEYWORD OF :: rest'' ->
       let branches, rest''' = parse_case_body reg rest'' in
       Case (scrutinee, sort_and_check reg branches), rest'''
     | _ -> failwith "expected of keyword")
  | _ -> parse_seq reg tk_list

and parse_case_body reg tk_list =
  let constr, var, rest =
    match parse_expr reg tk_list with
    | App (e, Var v), rest -> e, v, rest
    | _, _ -> failwith "expected constructor applied to a variable in case body"
  in
  match rest with
  | KEYWORD BIGARROW :: rest' ->
    let body, rest'' = parse_expr reg rest' in
    (match rest'' with
     | KEYWORD PIPE :: rest''' ->
       let branches, rest'''' = parse_case_body reg rest''' in
       (constr, var, body) :: branches, rest''''
     | _ -> (constr, var, body) :: [], rest'')
  | _ -> failwith "expected => after case branch pattern"

and parse_type type_name tk_list = parse_type_arrow type_name tk_list

and parse_type_arrow type_name tk_list =
  let left, rest = parse_type_tuple type_name tk_list in
  match rest with
  | KEYWORD ARROW :: rest' ->
    let right, rest'' = parse_type_arrow type_name rest' in
    TArrow (left, right), rest''
  | _ -> left, rest

and parse_type_tuple type_name tk_list =
  let first, rest = parse_type_atom type_name tk_list in
  let rec helper (acc : ml_type list) tk_list =
    match tk_list with
    | OPERATOR MULT :: rest' ->
      let next, rest'' = parse_type_atom type_name rest' in
      helper (next :: acc) rest''
    | _ -> List.rev acc, tk_list
  in
  match helper [ first ] rest with
  | [ single ], rest' -> single, rest'
  | l, rest' -> TTuple l, rest'

and parse_type_atom type_name tk_list =
  match tk_list with
  | KEYWORD BTInt :: rest -> TInt, rest
  | KEYWORD BTBool :: rest -> TBool, rest
  | VAR y :: rest ->
    if y = type_name then RecT (y, TVarS y), rest else failwith "unknown type"
  | BRACKET L_PAREN :: rest ->
    let t, rest' = parse_type type_name rest in
    (match rest' with
     | BRACKET R_PAREN :: rest'' -> t, rest''
     | _ -> failwith "missing closing paren in type")
  | _ -> failwith "error in parse_type_atom"

and parse_type_body type_name tk_list =
  match tk_list with
  | VAR name :: KEYWORD OF :: rest ->
    let payload, rest' = parse_type type_name rest in
    (match rest' with
     | KEYWORD PIPE :: rest'' ->
       let branches, rest''' = parse_type_body type_name rest'' in
       (name, payload) :: branches, rest'''
     | _ -> (name, payload) :: [], rest')
  | _ -> failwith "expected constructor of type in type body"

and parse_seq reg tk_list =
  let left, rest = parse_eq reg tk_list in
  let rec helper (acc : expr) tk_list =
    match tk_list with
    | KEYWORD COMMA :: rest ->
      let next, rest' = parse_eq reg rest in
      let res, rest'' = helper next rest' in
      (match res with
       | Tuple l -> Tuple (acc :: l), rest''
       | _ -> failwith "Should never reach here!")
    | _ -> Tuple [ acc ], tk_list
  in
  let res, rest = helper left rest in
  match res with
  | Tuple (expr :: []) -> expr, rest
  | _ -> res, rest

and parse_eq reg tk_list =
  let left, rest = parse_or reg tk_list in
  match rest with
  | OPERATOR EQ :: rest' ->
    let right, rest'' = parse_eq reg rest' in
    Bin_op (Eq, left, right), rest''
  | OPERATOR NEQ :: rest' ->
    let right, rest'' = parse_eq reg rest' in
    Bin_op (Neq, left, right), rest''
  | _ -> left, rest

and parse_or reg tk_list =
  let left, rest = parse_xor reg tk_list in
  match rest with
  | OPERATOR OR :: rest' ->
    let right, rest'' = parse_or reg rest' in
    Bin_op (Or, left, right), rest''
  | _ -> left, rest

and parse_xor reg tk_list =
  let left, rest = parse_and reg tk_list in
  match rest with
  | OPERATOR XOR :: rest' ->
    let right, rest'' = parse_xor reg rest' in
    Bin_op (Xor, left, right), rest''
  | _ -> left, rest

and parse_and reg tk_list =
  let left, rest = parse_comp reg tk_list in
  match rest with
  | OPERATOR AND :: rest' ->
    let right, rest'' = parse_and reg rest' in
    Bin_op (And, left, right), rest''
  | _ -> left, rest

and parse_comp reg tk_list =
  let left, rest = parse_add reg tk_list in
  match rest with
  | OPERATOR LE :: rest' ->
    let right, rest'' = parse_comp reg rest' in
    Bin_op (Le, left, right), rest''
  | OPERATOR LT :: rest' ->
    let right, rest'' = parse_comp reg rest' in
    Bin_op (Lt, left, right), rest''
  | OPERATOR GE :: rest' ->
    let right, rest'' = parse_comp reg rest' in
    Bin_op (Ge, left, right), rest''
  | OPERATOR GT :: rest' ->
    let right, rest'' = parse_comp reg rest' in
    Bin_op (Gt, left, right), rest''
  | _ -> left, rest

and parse_add reg tk_list =
  let left, rest = parse_mult reg tk_list in
  match rest with
  | OPERATOR PLUS :: rest' ->
    let right, rest'' = parse_add reg rest' in
    Bin_op (Add, left, right), rest''
  | OPERATOR MINUS :: rest' ->
    let right, rest'' = parse_add reg rest' in
    Bin_op (Sub, left, right), rest''
  | _ -> left, rest

and parse_mult reg tk_list =
  let left, rest = parse_un reg tk_list in
  match rest with
  | OPERATOR MULT :: rest' ->
    let right, rest'' = parse_mult reg rest' in
    Bin_op (Mult, left, right), rest''
  | OPERATOR DIV :: rest' ->
    let right, rest'' = parse_mult reg rest' in
    Bin_op (Div, left, right), rest''
  | _ -> left, rest

and parse_un reg tk_list =
  match tk_list with
  | OPERATOR NOT :: rest ->
    let expr, rest' = parse_atom reg rest in
    Un_op (Not, expr), rest'
  | OPERATOR MINUS :: rest ->
    let expr, rest' = parse_atom reg rest in
    Un_op (Neg, expr), rest'
  | _ -> parse_app reg tk_list

and parse_app reg tk_list =
  let expr, rest = parse_atom reg tk_list in
  let rec build_app expr' rest' =
    try
      let atom, rest'' = parse_atom reg rest' in
      build_app (App (expr', atom)) rest''
    with
    | Failure _ -> expr', rest'
  in
  build_app expr rest

and parse_atom reg = function
  | INT n :: rest -> Int n, rest
  | VAR s :: rest -> Var s, rest
  | BOOLEAN a :: rest -> Bool a, rest
  | BRACKET L_PAREN :: rest ->
    (match parse_expr reg rest with
     | expr, BRACKET R_PAREN :: rest' -> expr, rest'
     | _, _ -> failwith "Missing closing parenthesis")
  | _ -> failwith "Error in parse_atom"
;;

let parse tk_list = parse_with_reg [] tk_list
