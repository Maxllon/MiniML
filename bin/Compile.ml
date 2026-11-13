open MiniML

let usage = "usage: mlc <source> <output>"

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))
;;

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)
;;

let compile_source source =
  match Lexer.tokenize source with
  | Error e ->
    Error
      (Printf.sprintf "lexer error at position %d: %s" e.pos (Token.tokenToString e.tk))
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error e -> Error ("parser error: " ^ e)
     | Ok ast ->
       (match Typechecker.get_type ast with
        | Error e -> Error ("typechecker error: " ^ e)
        | Ok _ -> Ok (Lambda.ast_to_term ast)))
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; source; output ] ->
    (try
       match compile_source (read_file source) with
       | Error e ->
         prerr_endline (source ^ ": " ^ e);
         exit 1
       | Ok term -> write_file output (Lambda.term_to_string term ^ "\n")
     with
     | Sys_error e ->
       prerr_endline e;
       exit 1)
  | _ ->
    prerr_endline usage;
    exit 2
;;
