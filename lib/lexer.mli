type lexer_error =
  { pos : int
  ; tk_len : int
  ; tk : Token.token
  }

val tokenize : string -> (Token.token list, lexer_error) result
val tk_list_to_string : Token.token list -> string
