open Ast
open Token

let rec parse tk_list =
  try
    match parse_expr tk_list with
    | ast, [] -> Ok ast
    | ast, [ EOF ] -> Ok ast
    | _, _ -> Error "Extra tokens"
  with
  | Failure s -> Error s

and parse_expr tk_list = parse_let tk_list

and parse_let tk_list =
  match tk_list with
  | KEYWORD LET :: KEYWORD REC :: VAR name :: OPERATOR EQ :: rest ->
    parse_let_body
      (fun (name', expr', in_expr') -> Let_rec (name', expr', in_expr'))
      name
      rest
  | KEYWORD LET :: VAR name :: OPERATOR EQ :: rest ->
    parse_let_body
      (fun (name', expr', in_expr') -> Let (name', expr', in_expr'))
      name
      rest
  | _ -> parse_fun tk_list

and parse_let_body constructor name tk_list =
  let value, rest = parse_expr tk_list in
  match rest with
  | KEYWORD IN :: rest' ->
    let body, rest'' = parse_expr rest' in
    constructor (name, value, body), rest''
  | _ -> failwith "Expected in after let/let rec"

and parse_fun tk_list =
  match tk_list with
  | KEYWORD LAMBD :: rest -> parse_args [] rest
  | _ -> parse_if tk_list

and parse_args args tk_list =
  match tk_list with
  | VAR name :: rest -> parse_args (name :: args) rest
  | KEYWORD ARROW :: rest ->
    let expr, rest' = parse_expr rest in
    let rec build args' expr' =
      match args' with
      | [] -> expr'
      | s :: args'' -> Lambd (s, build args'' expr')
    in
    build (List.rev args) expr, rest'
  | _ -> failwith "Expected args or ARROW keyword"

and parse_if tk_list =
  match tk_list with
  | KEYWORD IF :: rest ->
    let cond, rest' = parse_expr rest in
    (match rest' with
     | KEYWORD THEN :: rest'' ->
       let then_expr, rest''' = parse_expr rest'' in
       (match rest''' with
        | KEYWORD ELSE :: rest'''' ->
          let else_expr, rest''''' = parse_expr rest'''' in
          If (cond, then_expr, else_expr), rest'''''
        | _ -> failwith "expected else keyword")
     | _ -> failwith "expected then keyword")
  | _ -> parse_eq tk_list

and parse_eq tk_list =
  parse_bin
    parse_or
    [ EQ; NEQ ]
    (fun op left right ->
       match op with
       | EQ -> Bin_op (Eq, left, right)
       | NEQ -> Bin_op (Neq, left, right)
       | _ -> failwith "impossible op in parse_eq")
    tk_list

and parse_or tk_list =
  parse_bin
    parse_xor
    [ OR ]
    (fun op left right ->
       match op with
       | OR -> Bin_op (Or, left, right)
       | _ -> failwith "impossible op in parse_or")
    tk_list

and parse_xor tk_list =
  parse_bin
    parse_and
    [ XOR ]
    (fun op left right ->
       match op with
       | XOR -> Bin_op (Xor, left, right)
       | _ -> failwith "impossible op in parse_xor")
    tk_list

and parse_and tk_list =
  parse_bin
    parse_comp
    [ AND ]
    (fun op left right ->
       match op with
       | AND -> Bin_op (And, left, right)
       | _ -> failwith "impossible op in parse_and")
    tk_list

and parse_comp tk_list =
  parse_bin
    parse_add
    [ LE; LT; GE; GT ]
    (fun op left right ->
       match op with
       | LE -> Bin_op (Le, left, right)
       | LT -> Bin_op (Lt, left, right)
       | GE -> Bin_op (Ge, left, right)
       | GT -> Bin_op (Gt, left, right)
       | _ -> failwith "impossible op in parse_comp")
    tk_list

and parse_add tk_list =
  parse_bin
    parse_mult
    [ PLUS; MINUS ]
    (fun op left right ->
       match op with
       | PLUS -> Bin_op (Add, left, right)
       | MINUS -> Bin_op (Sub, left, right)
       | _ -> failwith "impossible op in parse_add")
    tk_list

and parse_mult tk_list =
  parse_bin
    parse_un
    [ MULT; DIV ]
    (fun op left right ->
       match op with
       | MULT -> Bin_op (Mult, left, right)
       | DIV -> Bin_op (Div, left, right)
       | _ -> failwith "impossible op in parse_mult")
    tk_list

and parse_un tk_list =
  match tk_list with
  | OPERATOR NOT :: rest ->
    let expr, rest' = parse_atom rest in
    Un_op (Not, expr), rest'
  | OPERATOR MINUS :: rest ->
    let expr, rest' = parse_atom rest in
    Un_op (Neg, expr), rest'
  | _ -> parse_app tk_list

and parse_app tk_list =
  let expr, rest = parse_atom tk_list in
  let rec build_app expr' rest' =
    try
      let atom, rest'' = parse_atom rest' in
      build_app (App (expr', atom)) rest''
    with
    | Failure _ -> expr', rest'
  in
  build_app expr rest

and parse_atom = function
  | INT n :: rest -> Int n, rest
  | VAR s :: rest -> Var s, rest
  | BOOLEAN a :: rest -> Bool a, rest
  | BRACKET L_PAREN :: rest ->
    (match parse_expr rest with
     | expr, BRACKET R_PAREN :: rest' -> expr, rest'
     | _, _ -> failwith "Missing closing parenthesis")
  | _ -> failwith "Error in parse_atom"

and parse_bin next operators f tk_list =
  let left, rest = next tk_list in
  match rest with
  | OPERATOR op :: rest' when List.mem op operators ->
    let right, rest'' = parse_bin next operators f rest' in
    f op left right, rest''
  | _ -> left, rest
;;
