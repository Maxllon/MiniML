open MiniML
open Alcotest

let test_parse name expected_str input =
  match Lexer.tokenize input with
  | Error e -> fail (Printf.sprintf "Lexer error at pos %d" e.pos)
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error msg -> fail ("Parser error: " ^ msg)
     | Ok ast ->
       let actual_str = Ast.expr_to_string ast in
       check string name expected_str actual_str)
;;

let test_parse_error name input =
  match Lexer.tokenize input with
  | Error _ -> () (* ожидаем ошибку лексера - ок *)
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error _ -> () (* ожидаем ошибку парсера - ок *)
     | Ok _ -> fail (name ^ ": Expected error but got success"))
;;

let test_precedence () =
  test_parse "classic" "(2 + (2 * 2))" "2 + 2 * 2";
  test_parse "mult before add" "((2 * 3) + 4)" "2 * 3 + 4";
  test_parse "add before mult" "(2 + (3 * 4))" "2 + 3 * 4";
  test_parse "parens change order" "((2 + 3) * 4)" "(2 + 3) * 4";
  test_parse "left assoc add" "(2 + (3 + 4))" "2 + 3 + 4";
  test_parse
    "basic compare"
    "((((2 >= -(3)) = true) and false) or (false xor false))"
    "2>=(-3) = true and false or false xor false";
  ()
;;

let test_un () =
  test_parse "minus 1" "-(1)" "-1";
  test_parse "double minus" "-(-(1))" "-(-1)";
  test_parse "simple not and boolean (with not operator)" "not(true)" "not true";
  test_parse "simple not and boolean (with !)" "not(true)" "!true";
  ()
;;

let test_if () =
  test_parse "simple condition" "if (a > 10) then 20 else -(1)" "if a>10 then 20 else -1";
  test_parse_error "without else" "if a>10 then a";
  ()
;;

let test_let () =
  test_parse "basic let expression" "let x = 10 in x" "let x=10 in x";
  test_parse
    "let encapsulation"
    "let x = 5 in let y = let z = 10 in (x + z) in (x + y)"
    "let x = 5 in\n  let y =\n    let z = 10 in\n    x + z\n  in\n  x + y";
  test_parse_error "without in" "let x = 5";
  test_parse_error "without in body" "let x = 5 in";
  test_parse_error "without name" "let = 5 in 1";
  test_parse_error "without expr" "let x = in 1";
  test_parse "let rec inf loop" "let rec x = (x + 1) in x" "let rec x = x + 1 in x";
  ()
;;

let test_fun () =
  test_parse "basic fun expr" "\\x -> x" "\\x -> x";
  test_parse "multy vars" "\\x -> \\y -> (x + y)" "\\x y -> (x+y)";
  ()
;;

let test_atom () =
  test_parse "nothing" "" "";
  test_parse "zero" "0" "0";
  test_parse "var" "aboba" "aboba";
  ()
;;

let test_app () =
  test_parse "basic app" "(f x)" "f x";
  test_parse "mult args" "(((f x) y) z)" "f x y z";
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
