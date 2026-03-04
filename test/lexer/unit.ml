open MiniML
open Alcotest

let pp_tk_list fmt = function
  | Ok tk_list -> Format.pp_print_string fmt (Lexer.tk_list_to_string tk_list)
  | Error e -> Format.pp_print_string fmt "Lexer error"
;;

let tk_list_testable = testable pp_tk_list ( = )

let test_tk_list name expected input =
  check tk_list_testable name expected (Lexer.tokenize input)
;;

let test_correct_expressions () =
  test_tk_list
    "just 2+2=4"
    (Ok [ INT 2; OPERATOR PLUS; INT 2; OPERATOR EQ; INT 4; EOF ])
    "2+2=4";
  test_tk_list "Empty input" (Ok [ EOF ]) "";
  test_tk_list
    "let expression"
    (Ok [ KEYWORD LET; VAR "x"; OPERATOR EQ; INT 5; KEYWORD IN; VAR "x"; EOF ])
    "let x=5in x";
  test_tk_list
    "let expression (extra spaces)"
    (Ok [ KEYWORD LET; VAR "x"; OPERATOR EQ; INT 5; KEYWORD IN; VAR "x"; EOF ])
    "   let x =  5 in              x   ";
  ()
;;

let test_incorrect_expressions () =
  test_tk_list
    "Long num"
    (Error { pos = 0; tk_len = 20; tk = INVALID })
    "12345678900987654321";
  ()
;;

let suite =
  [ "correct expressions", `Quick, test_correct_expressions
  ; "incorrect expressions", `Quick, test_incorrect_expressions
  ]
;;

let () = Alcotest.run "Lexer tests" [ "lexer", suite ]
