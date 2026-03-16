open MiniML.Token
open Alcotest

let token_testable =
  testable (fun fmt (n, tk) -> Format.fprintf fmt "(%d, %s)" n (tokenToString tk)) ( = )
;;

let test_token expected input = check token_testable input expected (parseToken input)

let test_numbers () =
  test_token (2, INT 67) "67";
  test_token (2, INT 0) "00" (* TODO must be INVALID in this case*);
  test_token (1, INT 0) "0";
  test_token (1, INT 2) "2+2";
  test_token (20, INVALID) "12345678900987654321"
;;

let test_brackets () =
  test_token (1, BRACKET L_PAREN) "(";
  test_token (1, BRACKET R_PAREN) ")";
  test_token (1, BRACKET L_PAREN) "(abc";
  test_token (1, BRACKET R_PAREN) ")123"
;;

let test_operators () =
  test_token (1, OPERATOR PLUS) "+";
  test_token (1, OPERATOR MINUS) "-";
  test_token (1, OPERATOR MULT) "*";
  test_token (1, OPERATOR DIV) "/";
  test_token (1, OPERATOR EQ) "=";
  test_token (1, OPERATOR LT) "<";
  test_token (1, OPERATOR GT) ">";
  test_token (1, OPERATOR NOT) "!";
  test_token (3, OPERATOR NOT) "not";
  test_token (1, OPERATOR XOR) "^";
  test_token (3, OPERATOR XOR) "xor";
  test_token (2, OPERATOR NEQ) "!=";
  test_token (2, OPERATOR LE) "<=";
  test_token (2, OPERATOR GE) ">=";
  test_token (2, OPERATOR AND) "&&";
  test_token (3, OPERATOR AND) "and";
  test_token (2, OPERATOR OR) "||";
  test_token (2, OPERATOR OR) "or"
;;

let test_keywords () =
  test_token (3, KEYWORD LET) "let";
  test_token (3, KEYWORD REC) "rec";
  test_token (2, KEYWORD IF) "if";
  test_token (4, KEYWORD THEN) "then";
  test_token (4, KEYWORD ELSE) "else";
  test_token (2, KEYWORD IN) "in";
  test_token (1, KEYWORD LAMBD) "\\";
  test_token (2, KEYWORD ARROW) "->";
  test_token (4, VAR "letx") "letx"
;;

let test_booleans () =
  test_token (4, BOOLEAN true) "true";
  test_token (5, BOOLEAN false) "false";
  test_token (5, VAR "trueh") "trueh"
;;

let test_variables () =
  test_token (1, VAR "x") "x";
  test_token (4, VAR "x123") "x123";
  test_token (5, VAR "hello") "hello";
  test_token (3, VAR "x_y") "x_y";
  test_token (6, VAR "_aboba") "_aboba"
;;

let test_whitespace () =
  test_token (3, VAR "x") "  x";
  test_token (5, KEYWORD LET) "  let";
  test_token (2, OPERATOR PLUS) " +"
;;

let token_suite =
  [ "numbers", `Quick, test_numbers
  ; "brackets", `Quick, test_brackets
  ; "operators", `Quick, test_operators
  ; "keywords", `Quick, test_keywords
  ; "booleans", `Quick, test_booleans
  ; "variables", `Quick, test_variables
  ; "whitespace", `Quick, test_whitespace
  ]
;;

let () = Alcotest.run "Handle tests" [ "tokens", token_suite ]
