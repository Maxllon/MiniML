open Ast
open Token

let rec parse tk_list =
  match parse_expr tk_list with
  | Ok (ast, []) -> Ok ast
  | Ok (ast, [ EOF ]) -> Ok ast
  | Ok (_, _) -> Error "Extra tokens"
  | Error e -> Error e

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
  match parse_expr tk_list with
  | Error e -> Error e
  | Ok (expr, rest) ->
    (match rest with
     | KEYWORD IN :: rest' ->
       (match parse_expr rest' with
        | Error e -> Error e
        | Ok (in_expr, rest'') -> Ok (constructor (name, expr, in_expr), rest''))
     | _ -> Error "Expected in after let/let rec")

and parse_fun tk_list =
  match tk_list with
  | KEYWORD LAMBD :: rest -> parse_args [] rest
  | _ -> parse_if tk_list

and parse_args args tk_list =
  match tk_list with
  | VAR name :: rest -> parse_args (name :: args) rest
  | KEYWORD ARROW :: rest ->
    (match parse_expr rest with
     | Error e -> Error e
     | Ok (expr, rest') ->
       let rec build args' expr' =
         match args' with
         | [] -> expr'
         | s :: args'' -> Lambd (s, build args'' expr')
       in
       Ok (build (List.rev args) expr, rest'))
  | _ -> Error "Expected args or ARROW keyword"

and parse_if tk_list =
  match tk_list with
  | KEYWORD IF :: rest ->
    (match parse_expr rest with
     | Error e -> Error e
     | Ok (cond, rest') ->
       (match rest' with
        | KEYWORD THEN :: rest'' ->
          (match parse_expr rest'' with
           | Error e -> Error e
           | Ok (then_expr, rest''') ->
             (match rest''' with
              | KEYWORD ELSE :: rest'''' ->
                (match parse_expr rest'''' with
                 | Error e -> Error e
                 | Ok (else_expr, rest''''') ->
                   Ok (If (cond, then_expr, else_expr), rest'''''))
              | _ -> Error "expected else keyword"))
        | _ -> Error "expected then keyword"))
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
    (match parse_atom rest with
     | Error e -> Error e
     | Ok (expr, rest') -> Ok (Un_op (Not, expr), rest'))
  | OPERATOR MINUS :: rest ->
    (match parse_atom rest with
     | Error e -> Error e
     | Ok (expr, rest') -> Ok (Un_op (Neg, expr), rest'))
  | _ -> parse_app tk_list

and parse_app tk_list =
  match parse_atom tk_list with
  | Error e -> Error e
  | Ok (expr, rest) ->
    let rec build_app expr' rest' =
      match parse_atom rest' with
      | Error _ -> Ok (expr', rest')
      | Ok (atom, rest'') -> build_app (App (expr', atom)) rest''
    in
    build_app expr rest

and parse_atom = function
  | INT n :: rest -> Ok (Int n, rest)
  | VAR s :: rest -> Ok (Var s, rest)
  | BOOLEAN a :: rest -> Ok (Bool a, rest)
  | BRACKET L_PAREN :: rest ->
    (match parse_expr rest with
     | Error e -> Error e
     | Ok (expr, BRACKET R_PAREN :: rest') -> Ok (expr, rest')
     | Ok (_, _) -> Error "Missing closing parenthesis")
  | _ -> Error "Error in parse_atom"

and parse_bin next operators f tk_list =
  match next tk_list with
  | Error e -> Error e
  | Ok (left, rest) ->
    (match rest with
     | OPERATOR op :: rest' when List.mem op operators ->
       (match parse_bin next operators f rest' with
        | Error e -> Error e
        | Ok (right, rest'') -> Ok (f op left right, rest''))
     | _ -> Ok (left, rest))
;;
