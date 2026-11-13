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
  test_parse
    "let with arg sugar"
    (Let ("f", Lambd ("x", Bin_op (Add, Var "x", Int 1)), App (Var "f", Int 5)))
    "let f x = x+1 in f 5";
  test_parse
    "let rec with arg sugar"
    (Let_rec
       ( "fact"
       , Lambd
           ( "n"
           , If
               ( Bin_op (Eq, Var "n", Int 0)
               , Int 1
               , Bin_op (Mult, Var "n", App (Var "fact", Bin_op (Sub, Var "n", Int 1))) )
           )
       , Var "fact" ))
    "let rec fact n = if n=0 then 1 else n*fact(n-1) in fact";
  ()
;;

let test_fun () =
  test_parse "basic fun expr" (Lambd ("x", Var "x")) "\\x.x";
  test_parse
    "multy vars"
    (Lambd ("x", Lambd ("y", Bin_op (Add, Var "x", Var "y"))))
    "\\x y.x+y";
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

let test_case () =
  test_parse
    "simple case"
    (Let
       ( "t1"
       , Constr ("t1", 0, 2, TArrow (TInt, RecT ("X", TInt)))
       , Let
           ( "t2"
           , Constr ("t2", 1, 2, TArrow (TInt, RecT ("X", TInt)))
           , Case (Var "a", [ Var "t1", "x", Int 1; Var "t2", "y", Int 2 ]) ) ))
    "type X = t1 of Int | t2 of Int in case a of t1 x => 1 | t2 y => 2";
  test_parse
    "case branches sorted to canonical order"
    (Let
       ( "t1"
       , Constr ("t1", 0, 2, TArrow (TInt, RecT ("X", TInt)))
       , Let
           ( "t2"
           , Constr ("t2", 1, 2, TArrow (TInt, RecT ("X", TInt)))
           , Case (Var "a", [ Var "t1", "x", Int 1; Var "t2", "y", Int 2 ]) ) ))
    "type X = t1 of Int | t2 of Int in case a of t2 y => 2 | t1 x => 1";
  test_parse_error
    "case not exhaustive"
    "type X = t1 of Int | t2 of Int in case a of t1 x => 1";
  test_parse_error
    "case unknown constructor"
    "type X = t1 of Int in case a of t1 x => 1 | foo y => 2";
  test_parse_error "case without type decl" "case a of t1 x => 1";
  test_parse_error "case without of" "type X = t1 of Int in case a t1 x => 1";
  test_parse_error "branch without var" "type X = t1 of Int in case a of t1 => 1";
  test_parse_error "branch without arrow" "type X = t1 of Int in case a of t1 x 1";
  ()
;;

let test_typedecl () =
  test_parse
    "single constructor"
    (Let ("t1", Constr ("t1", 0, 1, TArrow (TInt, RecT ("X", TInt))), Var "t1"))
    "type X = t1 of Int in t1";
  test_parse
    "multiple constructors"
    (Let
       ( "t1"
       , Constr ("t1", 0, 2, TArrow (TInt, RecT ("X", TInt)))
       , Let ("t2", Constr ("t2", 1, 2, TArrow (TBool, RecT ("X", TBool))), Var "t1") ))
    "type X = t1 of Int | t2 of Bool in t1";
  test_parse
    "tuple and arrow precedence"
    (Let
       ( "t1"
       , Constr
           ( "t1"
           , 0
           , 1
           , TArrow
               ( TArrow (TTuple [ TInt; TBool ], RecT ("X", TVarS "X"))
               , RecT ("X", TArrow (TTuple [ TInt; TBool ], TVarS "X")) ) )
       , Var "t1" ))
    "type X = t1 of Int * Bool -> X in t1";
  test_parse
    "recursive reference in payload"
    (Let
       ( "t1"
       , Constr ("t1", 0, 1, TArrow (RecT ("X", TVarS "X"), RecT ("X", TVarS "X")))
       , Var "t1" ))
    "type X = t1 of X in t1";
  test_parse
    "parens in type"
    (Let
       ( "t1"
       , Constr
           ("t1", 0, 1, TArrow (TArrow (TInt, TBool), RecT ("X", TArrow (TInt, TBool))))
       , Var "t1" ))
    "type X = t1 of (Int -> Bool) in t1";
  test_parse
    "recursive tuple payload"
    (Let
       ( "val"
       , Constr
           ( "val"
           , 0
           , 1
           , TArrow
               ( TTuple [ TInt; RecT ("a", TVarS "a") ]
               , RecT ("a", TTuple [ TInt; TVarS "a" ]) ) )
       , Var "val" ))
    "type a = val of Int * a in val";
  test_parse_error "type without in" "type X = t1 of Int t1";
  test_parse_error "unknown type in payload" "type X = t1 of Y in t1";
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
  ; "case", `Quick, test_case
  ; "typedecl", `Quick, test_typedecl
  ]
;;

let () = Alcotest.run "Parser tests" [ "parser", suite ]
