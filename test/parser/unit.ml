open MiniML
open Alcotest

let pp_ast fmt ast = Format.pp_print_string fmt (Ast.expr_to_string ast)
let ast_testable = testable pp_ast ( = )

let test_parse name expected input =
  match Lexer.tokenize input with
  | Error e -> fail (Printf.sprintf "Lexer error at pos %d" e.pos)
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error msg -> fail ("Parser error: " ^ msg)
     | Ok ast -> check ast_testable name expected ast)
;;

let test_parse_error name input =
  match Lexer.tokenize input with
  | Error _ -> ()
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error _ -> ()
     | Ok _ -> fail (name ^ ": Expected error but got success"))
;;

let test_precedence () =
  test_parse "classic" (Bin_op (Add, Int 2, Bin_op (Mult, Int 2, Int 2))) "2 + 2 * 2";
  test_parse
    "mult before add"
    (Bin_op (Add, Bin_op (Mult, Int 2, Int 3), Int 4))
    "2 * 3 + 4";
  test_parse
    "add before mult"
    (Bin_op (Add, Int 2, Bin_op (Mult, Int 3, Int 4)))
    "2 + 3 * 4";
  test_parse
    "parens change order"
    (Bin_op (Mult, Bin_op (Add, Int 2, Int 3), Int 4))
    "(2 + 3) * 4";
  test_parse
    "left assoc add"
    (Bin_op (Add, Int 2, Bin_op (Add, Int 3, Int 4)))
    "2 + 3 + 4";
  test_parse
    "basic compare"
    (Bin_op
       ( Eq
       , Bin_op (Ge, Int 2, Un_op (Neg, Int 3))
       , Bin_op
           (Or, Bin_op (And, Bool true, Bool false), Bin_op (Xor, Bool false, Bool false))
       ))
    "2 >= -3 = true and false or false xor false";
  ()
;;

let test_un () =
  test_parse "minus 1" (Un_op (Neg, Int 1)) "-1";
  test_parse "double minus" (Un_op (Neg, Un_op (Neg, Int 1))) "-(-1)";
  test_parse
    "simple not and boolean (with not operator)"
    (Un_op (Not, Bool true))
    "not true";
  test_parse "simple not and boolean (with !)" (Un_op (Not, Bool true)) "!true";
  ()
;;

let test_if () =
  test_parse
    "simple condition"
    (If (Bin_op (Gt, Var "a", Int 10), Int 20, Un_op (Neg, Int 1)))
    "if a>10 then 20 else -1";
  test_parse_error "without else" "if a>10 then a";
  ()
;;

let test_let () =
  test_parse "basic let expression" (Let ("x", Int 10, Var "x")) "let x=10 in x";
  test_parse
    "let encapsulation"
    (Let
       ( "x"
       , Int 5
       , Let
           ( "y"
           , Let ("z", Int 10, Bin_op (Add, Var "x", Var "z"))
           , Bin_op (Add, Var "x", Var "y") ) ))
    "let x = 5 in\n  let y =\n    let z = 10 in\n    x + z\n  in\n  x + y";
  test_parse_error "without in" "let x = 5";
  test_parse_error "without in body" "let x = 5 in";
  test_parse_error "without name" "let = 5 in 1";
  test_parse_error "without expr" "let x = in 1";
  test_parse
    "let rec inf loop"
    (Let_rec ("x", Bin_op (Add, Var "x", Int 1), Var "x"))
    "let rec x = x + 1 in x";
  ()
;;

let test_fun () =
  test_parse "basic fun expr" (Lambd ("x", Var "x")) "\\x -> x";
  test_parse
    "multy vars"
    (Lambd ("x", Lambd ("y", Bin_op (Add, Var "x", Var "y"))))
    "\\x y -> (x+y)";
  ()
;;

let test_atom () =
  test_parse_error "nothing" "";
  test_parse "zero" (Int 0) "0";
  test_parse "var" (Var "aboba") "aboba";
  ()
;;

let test_app () =
  test_parse "basic app" (App (Var "f", Var "x")) "f x";
  test_parse "mult args" (App (App (App (Var "f", Var "x"), Var "y"), Var "z")) "f x y z";
  ()
;;

let suite =
  [ "precedence", `Quick, test_precedence
  ; "unary", `Quick, test_un
  ; "conditions", `Quick, test_if
  ; "let", `Quick, test_let
  ; "fun", `Quick, test_fun
  ; "atom", `Quick, test_atom
  ; "app", `Quick, test_app
  ]
;;

let () = Alcotest.run "Parser tests" [ "parser", suite ]
