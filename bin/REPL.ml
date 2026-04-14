open MiniML

let rec repl env =
  print_string "> ";
  flush stdout;
  try
    match read_line () with
    | line when line.[0] = ':' -> parse_cmd (String.sub line 1 (String.length line - 1))
    | line ->
      let result =
        match Lexer.tokenize line with
        | Error e -> Error ("Lexer error: " ^ string_of_int e.pos)
        | Ok tokens ->
          (match Parser.parse tokens with
           | Error e -> Error e
           | Ok ast -> Interpretator.eval env ast)
      in
      (match result with
       | Error e -> print_endline ("Error: " ^ e)
       | Ok v -> print_endline (Interpretator.value_to_string v));
      repl env
  with
  | End_of_file -> print_endline "\nGoodbye!"

and parse_cmd cmd =
  match cmd with
  | "q" | "exit" | "quit" -> print_endline "Goodbye!"
  | "h" | "help" ->
    print_endline "Available commands:";
    print_endline "  :h, :help   - show this help";
    print_endline "  :l <file>  - load and evaluate file";
    print_endline "  :t         - show defined values";
    print_endline "  :c         - clear screen";
    print_endline "  :q, :quit  - exit REPL";
    repl []
  | "t" | "types" ->
    print_endline "Defined values: (not implemented)";
    repl []
  | "c" | "clear" ->
    print_endline "\027[2J\027[H";
    repl []
  | cmd when String.length cmd > 1 && cmd.[0] = 'l' ->
    let filename = String.sub cmd 1 (String.length cmd - 1) in
    print_endline ("Loading file: " ^ filename ^ " (not implemented)");
    repl []
  | _ ->
    print_endline ("Unknown command: :" ^ cmd);
    repl []
;;

let () =
  print_endline "miniML REPL - type :h to list commands";
  repl []
;;
