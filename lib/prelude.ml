(* Генератор прелюдии: арифметика над числами, которые [Lambda.compile_int]
   представляет как кортеж из 32 Church-буллов, написана на самом MiniML.

   Публичные имена, на которые ссылается компилятор:

   {v add_name v} / {v sub_name v} / {v eq_name v}

   Компилятор не может сам построить определение функции, поэтому исходник
   прелюдии генерируется здесь и оборачивается вокруг пользовательского
   выражения через [Ast.Let] (см. {v Lambda.with_prelude v}). *)

let width = 32

let fa_name = "__ml_fa"
let not_name = "__ml_not"
let addc_name = "__ml_addc"
let add_name = "__ml_add"
let sub_name = "__ml_sub"
let eq_name = "__ml_eq"

let stage_name i = Printf.sprintf "__s%d" i

(* [nth i t] — i-й бит числа [t]. Это уже существующий примитив языка:
   [Lambda.try_std] разворачивает его в селектор, [Typechecker] типизирует
   его правилом для [nth]. Отдельного [bit] не нужно — прелюдия никогда не
   проходит типизацию, она собирается только на этапе компиляции (см.
   {v Lambda.with_prelude v}). Индекс обязан быть литералом: проекции с
   типом индекса в обычном Хиндли-Милнере не существует. *)
let bit i t = Printf.sprintf "(nth %d %s)" i t

let tuple bits = "(" ^ String.concat ", " bits ^ ")"

let range f =
  let rec go i acc = if i = 0 then acc else go (i - 1) (f (i - 1) :: acc) in
  go width []
;;

(* [chain a b carry] — развёрнутый вручную последовательный (ripple-carry)
   сумматор: на каждом разряде полный сумматор, перенос из [carry], из
   предыдущего разряда берётся старший бит его 2-элементного кортежа
   [(__ml_fa _ _) = (сумма, перенос)]. *)
let rec chain a b carry i defs bits =
  if i = width
  then (List.rev defs, List.rev bits)
  else (
    let name = stage_name i in
    let def =
      Printf.sprintf "let %s = %s %s %s %s in" name fa_name (bit i a) (bit i b) carry
    in
    let bits = bit 0 name :: bits in
    let carry = bit 1 name in
    chain a b carry (i + 1) (def :: defs) bits)
;;

let defs =
  let fa =
    Printf.sprintf
      "let %s a b c = (a xor b xor c, (a and b) or ((a xor b) and c)) in"
      fa_name
  in
  let notb =
    Printf.sprintf "let %s t = %s in" not_name (tuple (range (fun i -> "not (" ^ bit i "t" ^ ")")))
  in
  let addc_defs, addc_bits = chain "a" "b" "c" 0 [] [] in
  let addc = Printf.sprintf "let %s a b c = %s %s in" addc_name (String.concat " " addc_defs) (tuple addc_bits) in
  let add = Printf.sprintf "let %s a b = %s a b false in" add_name addc_name in
  (* a - b == a + ~b + 1 в дополнительном коде; перенос за 32-й разряд
     отбрасывается, то есть результат берётся по модулю 2^32. *)
  let sub = Printf.sprintf "let %s a b = %s a (%s b) true in" sub_name addc_name not_name in
  let eq =
    let differs = range (fun i -> Printf.sprintf "(%s xor %s)" (bit i "a") (bit i "b")) in
    Printf.sprintf "let %s a b = not (%s) in" eq_name (String.concat " or " differs)
  in
  [ fa; notb; addc; add; sub; eq ]
;;

let source = String.concat "\n" defs ^ "\n"
