open Token

type lexer_error =
  { pos : int
  ; tk_len : int
  ; tk : Token.token
  }

let tokenize input =
  let rec loop pos tk_list =
    if pos >= String.length input
    then Ok (List.rev (EOF :: tk_list))
    else (
      let cut = String.sub input pos (String.length input - pos) in
      let consume, token = parseToken cut in
      if token = INVALID
      then Error { pos; tk_len = consume; tk = INVALID }
      else loop (pos + consume) (token :: tk_list))
  in
  loop 0 []
;;

let tk_list_to_string tk_list =
  let rec loop = function
    | [] -> ""
    | [ x ] -> tokenToString x
    | x :: xs -> tokenToString x ^ "; " ^ loop xs
  in
  loop tk_list
;;
