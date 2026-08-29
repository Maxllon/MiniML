open MiniML
open Ast
open Alcotest

let rec alpha_eq acc t1 t2 =
  match t1, t2 with
  | TInt, TInt | TBool, TBool -> Some acc
  | TVar i1, TVar i2 ->
    (match List.assoc_opt i1 acc with
     | Some i2' -> if i2 = i2' then Some acc else None
     | None ->
       if List.exists (fun (_, k) -> k = i2) acc
       then None
       else Some ((i1, i2) :: acc))
  | TArrow (l1, r1), TArrow (l2, r2) ->
    (match alpha_eq acc l1 l2 with
     | Some acc' -> alpha_eq acc' r1 r2
     | None -> None)
  | TTuple ts1, TTuple ts2 when List.length ts1 = List.length ts2 ->
    List.fold_left2
      (fun acc_opt t1 t2 ->
        match acc_opt with
        | None -> None
        | Some acc' -> alpha_eq acc' t1 t2)
      (Some acc) ts1 ts2
  | RecT (n1, t1), RecT (n2, t2) when String.equal n1 n2 -> alpha_eq acc t1 t2
  | TVarS s1, TVarS s2 -> if String.equal s1 s2 then Some acc else None
  | _ -> None
;;

let types_equal t1 t2 = match alpha_eq [] t1 t2 with Some _ -> true | None -> false

let pp_type_result fmt = function
  | Ok t -> Format.fprintf fmt "Ok %s" (Typechecker.ml_type_to_string t)
  | Error e -> Format.fprintf fmt "Error %s" e
;;

let type_result_testable =
  testable
    pp_type_result
    (fun a b ->
      match a, b with
      | Ok t1, Ok t2 -> types_equal t1 t2
      | Error e1, Error e2 -> String.equal e1 e2
      | _ -> false)
;;

let typecheck src =
  match Lexer.tokenize src with
  | Error _ -> Error "lexer error"
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error e -> Error ("parser error: " ^ e)
     | Ok ast -> Typechecker.get_type ast)
;;

let check_ok name expected src = check type_result_testable name (Ok expected) (typecheck src)

let check_error name src =
  match typecheck src with
  | Error _ -> ()
  | Ok t ->
    fail
      (Printf.sprintf "%s: expected a type error but got %s" name (Typechecker.ml_type_to_string t))
;;

let check_error_msg name msg src = check type_result_testable name (Error msg) (typecheck src)

let test_constants () =
  check_ok "int literal" TInt "5";
  check_ok "zero" TInt "0";
  check_ok "true literal" TBool "true";
  check_ok "false literal" TBool "false";
  ()
;;

let test_arithmetic () =
  check_ok "addition" TInt "1+2";
  check_ok "subtraction" TInt "1-2";
  check_ok "multiplication" TInt "2*3";
  check_ok "division" TInt "7/2";
  check_ok "precedence" TInt "2+3*4";
  check_ok "parens change type result" TInt "(2+3)*4";
  check_ok "unary minus" TInt "-5";
  check_ok "double negation" TInt "-(-5)";
  check_error "bool in arithmetic" "1+true";
  check_error "int under unary not" "not 1";
  check_error "bool under unary minus" "-true";
  ()
;;

let test_logic () =
  check_ok "and" TBool "true and false";
  check_ok "or" TBool "false or true";
  check_ok "xor" TBool "true xor true";
  check_ok "not" TBool "not true";
  check_ok "infix not" TBool "!false";
  check_ok "mixed logical expr" TBool "true and false xor true";
  check_error "int in logical expr" "1 and true";
  ()
;;

let test_comparison () =
  check_ok "eq" TBool "1=2";
  check_ok "neq" TBool "1!=2";
  check_ok "lt" TBool "1<2";
  check_ok "le" TBool "1<=2";
  check_ok "gt" TBool "1>2";
  check_ok "ge" TBool "1>=2";
  check_error_msg "compare bool to int" "Cannot equalise:\nInt\nBool" "1 = true";
  check_error "compare bool operands" "true = false";
  ()
;;

let test_if () =
  check_ok "true branch" TInt "if true then 1 else 2";
  check_ok "bool result" TBool "if true then true else false";
  check_ok "comparison condition" TBool "if 1=2 then true else false";
  check_ok "comparison condition int result" TInt "if 1=1 then 5 else 6";
  check_error "condition is not bool" "if 1 then 1 else 2";
  check_error "branches have different types" "if true then 1 else true";
  ()
;;

let test_variables_and_let () =
  check_ok "let binding" TInt "let x=5 in x";
  check_ok "let use in expr" TInt "let x=5 in x+1";
  check_ok "let-bound bool" TBool "let x=true in x and false";
  check_ok "unused let value" TBool "let x=5 in true";
  check_ok "let shadowing" TBool "let x=1 in let x=true in x";
  check_error "let-bound var of wrong type" "let x=5 in x and true";
  check_error_msg "unbound variable" "Unbound variable: \"x\"" "x";
  check_error "unbound variable in lambda" "\\x.y";
  check_error_msg "unbound variable in application" "Unbound variable: \"x\"" "1 x";
  ()
;;

let test_let_rec () =
  check_ok "recursive value unused" TInt "let rec x = x in 1";
  check_ok "recursive value used" TInt "let rec x = 1 in x";
  check_ok "factorial function type" (TArrow (TInt, TInt))
    "let rec fact n = if n=0 then 1 else n*fact(n-1) in fact";
  check_ok "factorial applied" TInt
    "let rec fact n = if n=0 then 1 else n*fact(n-1) in fact 5";
  check_error "rec value not a function" "let rec f = 1 in f 2";
  check_error_msg "infinite type" "Occur check error" "let rec x = \\y.x in x";
  ()
;;

let test_functions () =
  check_ok "identity" (TArrow (TVar 0, TVar 0)) "\\x.x";
  check_ok "int function" (TArrow (TInt, TInt)) "\\x.x+1";
  check_ok "bool function" (TArrow (TBool, TBool)) "\\x.x and true";
  check_ok "const function" (TArrow (TVar 0, TArrow (TVar 1, TVar 0))) "\\x.\\y.x";
  check_ok "multi arg function" (TArrow (TInt, TArrow (TInt, TInt))) "\\x y.x+y";
  check_ok "apply to int" TInt "(\\x.x) 5";
  check_ok "apply to bool" TBool "(\\x.x) true";
  check_ok "function of function type" (TArrow (TVar 0, TVar 0)) "(\\x.x) (\\y.y)";
  check_ok "let-polymorphic applied to int" TInt "let f = \\x.x in f 5";
  check_ok "let-polymorphic applied to bool" TBool "let f = \\x.x in f true";
  check_ok "let-polymorphic value" (TArrow (TVar 0, TVar 0)) "let f = \\x.x in f";
  check_ok "let-introduced constant" (TArrow (TVar 0, TInt)) "let x = 5 in \\y.x";
  check_error "int applied as function" "1 2";
  check_error "bool applied as function" "true false";
  ()
;;

let test_tuples () =
  check_ok "pair" (TTuple [ TInt; TBool ]) "(1, true)";
  check_ok "triple" (TTuple [ TInt; TInt; TInt ]) "(1, 2, 3)";
  check_ok "mixed tuple" (TTuple [ TInt; TBool; TInt ]) "(1, true, 3)";
  check_ok "nth inside parens" TBool "(nth 2) (1,2,true)";
  check_ok "nth without parens" TBool "nth 2 (1,2,true)";
  check_ok "nth of pair" TBool "nth 1 (5,true)";
  check_ok "equality of equal tuples" TBool "nth 2 (1,2,3) = 3";
  check_error_msg "tuple too small" "Tuple size match error" "nth 2 (1,2)";
  check_error_msg "tuple too small with parens" "Tuple size match error" "(nth 2) (1,2)";
  check_error "tuple in arithmetic" "(1,2,3) + 4";
  check_error "equality on tuple" "(1,2) = (1,2)";
  ()
;;

let test_case () =
  check_ok "constructor TInt" (TArrow (TInt, RecT ("X", TInt)))
    "type X = t1 of Int | t2 of Int in t1";
  check_ok "constructor with Bool payload" (TArrow (TBool, RecT ("X", TBool)))
    "type X = t1 of Int | t2 of Bool in t2";
  check_ok "constructor applied" (RecT ("X", TInt)) "type X = t1 of Int in t1 5";
  check_ok "case branches same result" TInt
    "type X = t1 of Int | t2 of Int in case (t1 5) of t1 x => x | t2 y => y";
  check_ok "case different payloads" TInt
    "type X = t1 of Int | t2 of Bool in case (t1 5) of t1 x => 0 | t2 y => if y then 1 else 0";
  check_ok "single constructor case" TInt
    "type X = t1 of Int in case (t1 5) of t1 x => x";
  check_ok "case with varying branch bodies" TInt
    "type X = t1 of Int | t2 of Bool in case (t1 5) of t1 x => 1 | t2 y => 2";
  check_error "branch results have different types"
    "type X = t1 of Int | t2 of Bool in case (t1 5) of t1 x => x | t2 y => y";
  check_error "constructor arity mismatch"
    "type X = t1 of Int | t2 of Int in case (t1 (1,2)) of t1 x => 0 | t2 y => y";
  check_error "wrong constructor argument type" "type X = t1 of Int in t1 true";
  ()
;;

let test_to_string () =
  let t = testable Format.pp_print_string String.equal in
  check t "int" "Int" (Typechecker.ml_type_to_string TInt);
  check t "arrow" "(Int -> Bool)" (Typechecker.ml_type_to_string (TArrow (TInt, TBool)));
  check
    t
    "tuple"
    "(Int * Bool)"
    (Typechecker.ml_type_to_string (TTuple [ TInt; TBool ]));
  check t "type variable" "t3" (Typechecker.ml_type_to_string (TVar 3));
  check t "named type variable" "s: X" (Typechecker.ml_type_to_string (TVarS "X"));
  check
    t
    "recursive type"
    "rec X.Int"
    (Typechecker.ml_type_to_string (RecT ("X", TInt)));
  check
    t
    "recursive arrow type"
    "(Int -> rec X.Int)"
    (Typechecker.ml_type_to_string (TArrow (TInt, RecT ("X", TInt))))
;;

let suite =
  [ "constants", `Quick, test_constants
  ; "arithmetic", `Quick, test_arithmetic
  ; "logic", `Quick, test_logic
  ; "comparison", `Quick, test_comparison
  ; "if-then-else", `Quick, test_if
  ; "variables and let", `Quick, test_variables_and_let
  ; "let rec", `Quick, test_let_rec
  ; "functions", `Quick, test_functions
  ; "tuples", `Quick, test_tuples
  ; "case", `Quick, test_case
  ; "ml_type_to_string", `Quick, test_to_string
  ]
;;

let () = Alcotest.run "Typechecker tests" [ "typechecker", suite ]