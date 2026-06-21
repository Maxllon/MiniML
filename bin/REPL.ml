open MiniML

let () =
  print_endline "miniML REPL";
  try
    while true do
      print_string "> ";
      flush stdout;
      match read_line () with
      | line ->
        (match Lexer.tokenize line with
         | Error e -> print_endline ("Lexer error: " ^ string_of_int e.pos)
         | Ok tokens ->
           (match Parser.parse tokens with
            | Error e -> print_endline ("Parser error: " ^ e)
            | Ok ast ->
              let result = Interpreter.eval (Lambda.ast_to_term ast) in
              print_endline (Lambda.term_to_string result);
              print_endline (Lambda.term_to_string (Lambda.ast_to_term ast))))
    done
  with
  | End_of_file -> print_endline "\nGoodbye!"
;;
