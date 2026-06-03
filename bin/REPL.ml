open MiniML

type repl_state =
  { term : Lambda.term option
  ; step : int
  }

let rec repl state =
  print_string "> ";
  flush stdout;
  try
    match read_line () with
    | line when line.[0] = ':' ->
      parse_cmd (String.sub line 1 (String.length line - 1)) state
    | line ->
      let result =
        match Lexer.tokenize line with
        | Error e -> Error ("Lexer error: " ^ string_of_int e.pos)
        | Ok tokens ->
          (match Parser.parse tokens with
           | Error e -> Error e
           | Ok ast -> Ok (Interpreter.beta_reduce (Lambda.ast_to_term ast)))
      in
      (match result with
       | Error e -> print_endline ("Error: " ^ e)
       | Ok v -> print_endline (Lambda.term_to_string v));
      repl state
  with
  | End_of_file -> print_endline "\nGoodbye!"

and parse_cmd cmd state =
  match cmd with
  | "q" | "exit" | "quit" -> print_endline "Goodbye!"
  | "h" | "help" ->
    print_endline "Available commands:";
    print_endline "  :h, :help      - show this help";
    print_endline "  :load <code>   - load code for step-by-step reduction";
    print_endline "  :next          - perform one step of reduction";
    print_endline "  :t             - show defined values";
    print_endline "  :c             - clear screen";
    print_endline "  :compile <code> - compile code to lambda and print it";
    print_endline "  :q, :quit      - exit REPL";
    repl state
  | "t" | "types" ->
    print_endline "Defined values: (not implemented)";
    repl state
  | "c" | "clear" ->
    print_endline "\027[2J\027[H";
    repl state
  | "next" ->
    (match state.term with
     | None ->
       print_endline "Nothing loaded. Use :load <code> first.";
       repl state
     | Some term ->
       (match Interpreter.beta_step term with
        | Some term' ->
          let step = state.step + 1 in
          print_endline ("Step " ^ string_of_int step ^ ": " ^ Lambda.term_to_string term');
          repl { term = Some term'; step }
        | None ->
          print_endline
            ("Step " ^ string_of_int state.step ^ ": " ^ Lambda.term_to_string term);
          print_endline "Nothing more to reduce.";
          repl { term = None; step = 0 }))
  | cmd when String.length cmd > 5 && String.sub cmd 0 4 = "load" ->
    let code = String.sub cmd 5 (String.length cmd - 5) in
    let new_state =
      match Lexer.tokenize code with
      | Error e ->
        print_endline ("Load error: " ^ string_of_int e.pos);
        state
      | Ok tokens ->
        (match Parser.parse tokens with
         | Error e ->
           print_endline ("Load error: " ^ e);
           state
         | Ok ast ->
           let term = Lambda.ast_to_term ast in
           print_endline ("Step 0: " ^ Lambda.term_to_string term);
           { term = Some term; step = 0 })
    in
    repl new_state
  | cmd when String.length cmd > 8 && String.sub cmd 0 7 = "compile" ->
    let code = String.sub cmd 8 (String.length cmd - 8) in
    (match Lexer.tokenize code with
     | Error e -> print_endline ("Compile error: " ^ string_of_int e.pos)
     | Ok tokens ->
       (match Parser.parse tokens with
        | Error e -> print_endline ("Compile error: " ^ e)
        | Ok ast ->
          let term = Lambda.ast_to_term ast in
          print_endline (Lambda.term_to_string term)));
    repl state
  | cmd when String.length cmd > 1 && cmd.[0] = 'l' ->
    let filename = String.sub cmd 1 (String.length cmd - 1) in
    print_endline ("Loading file: " ^ filename ^ " (not implemented)");
    repl state
  | _ ->
    print_endline ("Unknown command: :" ^ cmd);
    repl state
;;

let () =
  print_endline "miniML REPL - type :h to list commands";
  repl { term = None; step = 0 }
;;
