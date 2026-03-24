open MiniML
open QCheck

let bin_op_expr_gen sub =
  let bin_ops =
    [ Ast.Add
    ; Ast.Sub
    ; Ast.Mult
    ; Ast.Div
    ; Ast.Eq
    ; Ast.Neq
    ; Ast.Lt
    ; Ast.Le
    ; Ast.Gt
    ; Ast.Ge
    ; Ast.And
    ; Ast.Or
    ; Ast.Xor
    ]
  in
  let bin_op_gen = Gen.oneof (List.map Gen.return bin_ops) in
  Gen.map3 (fun op left right -> Ast.Bin_op (op, left, right)) bin_op_gen sub sub
;;

let is_keyword s =
  List.mem
    (String.lowercase_ascii s)
    [ "let"; "rec"; "if"; "then"; "else"; "in"; "or"; "and"; "xor"; "not" ]
;;

let gen_identifier =
  let char_gen = Gen.oneof [ Gen.char_range 'a' 'z'; Gen.char_range 'A' 'Z' ] in
  let rec gen () =
    let base = Gen.string_size ~gen:char_gen (Gen.int_range 1 8) in
    Gen.bind base (fun s -> if is_keyword s then gen () else Gen.return s)
  in
  gen ()
;;

let gen_var = gen_identifier |> Gen.map (fun s -> Ast.Var s)

let atom_expr_gen =
  Gen.oneof
    [ Gen.map (fun n -> Ast.Int n) Gen.nat_small
    ; Gen.map (fun var -> Ast.Var var) gen_identifier
    ; Gen.map (fun _bool -> Ast.Bool _bool) Gen.bool
    ]
;;

let un_op_expr_gen sub =
  let un_ops = [ Ast.Neg; Ast.Not ] in
  let un_op_gen = Gen.oneof (List.map Gen.return un_ops) in
  Gen.map2 (fun op _expr -> Ast.Un_op (op, _expr)) un_op_gen sub
;;

let let_expr_gen sub =
  Gen.map3 (fun name expr expr' -> Ast.Let (name, expr, expr')) gen_identifier sub sub
;;

let let_rec_expr_gen sub =
  Gen.map3 (fun name expr expr' -> Ast.Let_rec (name, expr, expr')) gen_identifier sub sub
;;

let lambd_expr_gen sub =
  Gen.map2 (fun name expr -> Ast.Lambd (name, expr)) gen_identifier sub
;;

let app_expr_gen sub = Gen.map2 (fun expr expr' -> Ast.App (expr, expr')) gen_var sub

let if_expr_gen sub =
  Gen.map3 (fun expr expr' expr'' -> Ast.If (expr, expr', expr'')) sub sub sub
;;

let rec expr_gen depth =
  if depth <= 1
  then atom_expr_gen
  else (
    let sub = expr_gen (depth - 1) in
    Gen.oneof
      [ bin_op_expr_gen sub
      ; un_op_expr_gen sub
      ; let_expr_gen sub
      ; let_rec_expr_gen sub
      ; lambd_expr_gen sub
      ; app_expr_gen sub
      ; if_expr_gen sub
      ])
;;

let lex_and_parse s =
  match Lexer.tokenize s with
  | Ok tokens -> Parser.parse tokens
  | Error _ -> Error "lexer error"
;;

let pp_expr e = Ast.expr_to_string e
let gen_expr depth = make ~print:pp_expr (expr_gen depth)

let test_roundtrip_simple =
  Test.make ~name:"roundtrip simple" ~count:10000 (gen_expr 3) (fun e ->
    match lex_and_parse (pp_expr e) with
    | Ok _ -> true
    | Error msg ->
      prerr_endline ("FAIL: " ^ pp_expr e ^ " -> " ^ msg);
      false)
;;

let test_roundtrip_medium =
  Test.make ~name:"roundtrip medium" ~count:10000 (gen_expr 5) (fun e ->
    match lex_and_parse (pp_expr e) with
    | Ok _ -> true
    | Error msg ->
      prerr_endline ("FAIL: " ^ pp_expr e ^ " -> " ^ msg);
      false)
;;

let test_roundtrip_hard =
  Test.make ~name:"roundtrip hard" ~count:10000 (gen_expr 10) (fun e ->
    match lex_and_parse (pp_expr e) with
    | Ok _ -> true
    | Error msg ->
      prerr_endline ("FAIL: " ^ pp_expr e ^ " -> " ^ msg);
      false)
;;

let suite = [ test_roundtrip_simple; test_roundtrip_medium; test_roundtrip_hard ]
let () = exit (QCheck_runner.run_tests suite)
