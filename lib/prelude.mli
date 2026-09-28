(** Генератор прелюдии: арифметика на 32-битных числах, представленных
    кортежем из Church-булсов, написана на самом MiniML. *)

val width : int

(** Имена, на которые ссылается {v Lambda.bin_to_term v}. *)
val add_name : string

val sub_name : string

(** Умножение: 32 частичных произведений [a << i], обрезанных по биту [i]
    второго операнда, складываются в цепочку [add]. *)
val mul_name : string

val eq_name : string

(** Сравнения: [a < b] — знаковое, [a <= b] и [a >= b] выведены через [lt],
    [a > b] — это [lt b a]. *)
val lt_name : string

val le_name : string
val gt_name : string
val ge_name : string

(** Исходник прелюдии: цепочка [let ... in], без обрамляющего выражения. *)
val source : string
