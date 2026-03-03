open MiniML.Token

let test_token expected input =
  let consumed, token = parseToken input in
  if token = expected
  then Printf.printf "✓ '%s' -> OK\n" input
  else
    Printf.printf
      "✗ '%s' expected %s but got %s\n"
      input
      (tokenToString expected)
      (tokenToString token)
;;

let test_token_full expected_consumed expected_token input =
  let consumed, token = parseToken input in
  if consumed = expected_consumed && token = expected_token
  then Printf.printf "✓ '%s' -> OK (consumed: %d)\n" input consumed
  else
    Printf.printf
      "✗ '%s' expected (%d, %s) but got (%d, %s)\n"
      input
      expected_consumed
      (tokenToString expected_token)
      consumed
      (tokenToString token)
;;

let test_numbers () =
  print_endline "\n--- Тесты чисел ---";
  test_token (INT 42) "42";
  test_token (INT 0) "0";
  test_token (INT 123) "123";
  test_token_full 2 (INT 42) "42+10";
  test_token_full 3 (INT 123) "123abc";
  test_token (INT 999) "999";
  test_token INVALID "418274987214972194712";
  ()
;;

let test_brackets () =
  print_endline "\n--- Тесты скобок ---";
  test_token (BRACKET L_PAREN) "(";
  test_token (BRACKET R_PAREN) ")";
  test_token_full 1 (BRACKET L_PAREN) "(abc";
  test_token_full 1 (BRACKET R_PAREN) ")123";
  ()
;;

let test_operators () =
  print_endline "\n--- Тесты операторов ---";
  test_token (OPERATOR PLUS) "+";
  test_token (OPERATOR MINUS) "-";
  test_token (OPERATOR MULT) "*";
  test_token (OPERATOR DIV) "/";
  test_token (OPERATOR EQ) "=";
  test_token (OPERATOR LT) "<";
  test_token (OPERATOR GT) ">";
  test_token (OPERATOR NOT) "!";
  test_token (OPERATOR NOT) "not";
  test_token (OPERATOR XOR) "^";
  test_token (OPERATOR XOR) "xor";
  test_token (OPERATOR NEQ) "!=";
  test_token (OPERATOR LE) "<=";
  test_token (OPERATOR GE) ">=";
  test_token (OPERATOR AND) "&&";
  test_token (OPERATOR AND) "and";
  test_token (OPERATOR OR) "||";
  test_token (OPERATOR OR) "or";
  test_token_full 3 INVALID "!==";
  test_token_full 3 INVALID "&&&";
  ()
;;

let test_keywords () =
  print_endline "\n--- Тесты ключевых слов ---";
  test_token (KEYWORD LET) "let";
  test_token (KEYWORD REC) "rec";
  test_token (KEYWORD IF) "if";
  test_token (KEYWORD THEN) "then";
  test_token (KEYWORD ELSE) "else";
  test_token (KEYWORD IN) "in";
  test_token (KEYWORD LAMBD) "lambd";
  test_token (KEYWORD ARROW) "->";
  test_token (VAR "letx") "letx";
  ()
;;

let test_booleans () =
  print_endline "\n--- Тесты булевых значений ---";
  test_token (BOOLEAN TRUE) "true";
  test_token (BOOLEAN FALSE) "false";
  test_token (VAR "truex") "truex";
  ()
;;

let test_variables () =
  print_endline "\n--- Тесты переменных ---";
  test_token (VAR "x") "x";
  test_token (VAR "x123") "x123";
  test_token (VAR "hello") "hello";
  test_token (VAR "x_y") "x_y";
  test_token (VAR "abc") "abc";
  test_token_full 1 (VAR "x") "x+1";
  test_token_full 6 (VAR "abc123") "abc123 ";
  ()
;;

let test_whitespace () =
  print_endline "\n--- Тесты пробелов ---";
  test_token_full 4 (INT 42) "  42";
  test_token_full 3 (VAR "x") "  x ";
  test_token_full 5 (KEYWORD LET) "  let";
  test_token_full 2 (OPERATOR PLUS) " +";
  test_token_full 3 EOF "   ";
  ()
;;

let () =
  print_endline "=== ТЕСТЫ ТОКЕНИЗАТОРА ===\n";
  test_numbers ();
  test_brackets ();
  test_operators ();
  test_keywords ();
  test_booleans ();
  test_variables ();
  test_whitespace ();
  print_endline "\n✅ Все тесты завершены!\n"
;;
