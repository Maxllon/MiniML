open Lambda

type value =
  | VInt of int
  | VBool of bool
  | VClosure of term * value list
  | VError

val eval : value list -> term -> value
val run : term -> value
