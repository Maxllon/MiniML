(** Генератор прелюдии: арифметика на 32-битных числах, представленных
    кортежем из Church-булсов, написана на самом MiniML. *)

val width : int

(** Имена, на которые ссылается {v Lambda.bin_to_term v}. *)
val add_name : string
val sub_name : string
val eq_name : string

(** Исходник прелюдии: цепочка [let ... in], без обрамляющего выражения. *)
val source : string
