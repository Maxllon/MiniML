open MiniML
open MiniML.Ast
open QCheck

let lex_and_parse s =
  match Lexer.tokenize s with
  | Ok tokens -> Parser.parse tokens
  | Error _ -> Error "lexer error"
;;

let pp_expr e = Ast.expr_to_string e

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

let gen_var = gen_identifier |> Gen.map (fun s -> Var s)
let gen_int = Gen.int_range 0 100 |> Gen.map (fun n -> Int n)

let gen_bool =
  Gen.oneof [ Gen.return true; Gen.return false ] |> Gen.map (fun b -> Bool b)
;;

let gen_atom = Gen.oneof [ gen_int; gen_bool; gen_var ]

let rec gen_expr depth =
  if depth <= 0
  then gen_atom
  else
    Gen.oneof_weighted
      [ 4, gen_atom
      ; 2, Gen.bind (gen_expr (depth / 2)) (fun e -> Gen.return (Un_op (Neg, e)))
      ; 2, Gen.bind (gen_expr (depth / 2)) (fun e -> Gen.return (Un_op (Not, e)))
      ; ( 1
        , Gen.bind gen_identifier (fun n ->
            Gen.bind
              (gen_expr (depth / 2))
              (fun v ->
                 Gen.bind (gen_expr (depth / 2)) (fun b -> Gen.return (Let (n, v, b))))) )
      ; ( 1
        , Gen.bind gen_identifier (fun x ->
            Gen.bind (gen_expr (depth / 2)) (fun b -> Gen.return (Lambd (x, b)))) )
      ; ( 1
        , Gen.bind
            (gen_expr (depth / 2))
            (fun c ->
               Gen.bind
                 (gen_expr (depth / 2))
                 (fun t ->
                    Gen.bind (gen_expr (depth / 2)) (fun e -> Gen.return (If (c, t, e)))))
        )
      ; ( 3
        , Gen.bind
            (gen_expr (depth / 2))
            (fun l ->
               Gen.bind
                 (gen_expr (depth / 2))
                 (fun r ->
                    Gen.oneof
                      [ Gen.return (Bin_op (Add, l, r))
                      ; Gen.return (Bin_op (Sub, l, r))
                      ; Gen.return (Bin_op (Mult, l, r))
                      ; Gen.return (Bin_op (Div, l, r))
                      ; Gen.return (Bin_op (Eq, l, r))
                      ; Gen.return (Bin_op (Neq, l, r))
                      ; Gen.return (Bin_op (Lt, l, r))
                      ; Gen.return (Bin_op (Le, l, r))
                      ; Gen.return (Bin_op (Gt, l, r))
                      ; Gen.return (Bin_op (Ge, l, r))
                      ; Gen.return (Bin_op (And, l, r))
                      ; Gen.return (Bin_op (Or, l, r))
                      ; Gen.return (Bin_op (Xor, l, r))
                      ])) )
      ; ( 2
        , Gen.bind gen_var (fun f -> Gen.bind gen_atom (fun a -> Gen.return (App (f, a))))
        )
      ]
;;

let expr_gen depth = make ~print:pp_expr (gen_expr depth)
let small_expr_gen = expr_gen 5
let medium_expr_gen = expr_gen 10

let test_roundtrip_simple =
  Test.make ~name:"roundtrip simple" small_expr_gen (fun e ->
    match lex_and_parse (pp_expr e) with
    | Ok _ -> true
    | Error msg ->
      prerr_endline ("FAIL: " ^ pp_expr e ^ " -> " ^ msg);
      false)
;;

let test_roundtrip_medium =
  Test.make ~name:"roundtrip medium" medium_expr_gen (fun e ->
    match lex_and_parse (pp_expr e) with
    | Ok _ -> true
    | Error msg ->
      prerr_endline ("FAIL: " ^ pp_expr e ^ " -> " ^ msg);
      false)
;;

let suite = [ test_roundtrip_simple; test_roundtrip_medium ]
let () = exit (QCheck_runner.run_tests suite)
