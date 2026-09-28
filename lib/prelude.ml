(* Генератор прелюдии: арифметика над числами, которые [Lambda.compile_int]
   представляет как кортеж из 32 Church-буллов, написана на самом MiniML.

   Публичные имена, на которые ссылается компилятор:

   {v add_name v} / {v sub_name v} / {v mul_name v} / {v eq_name v} /
   {v lt_name v} / {v le_name v} / {v gt_name v} / {v ge_name v}

   Компилятор не может сам построить определение функции, поэтому исходник
   прелюдии генерируется здесь и оборачивается вокруг пользовательского
   выражения через [Ast.Let] (см. {v Lambda.with_prelude v}). *)

let width = 32
let fa_name = "__ml_fa"
let not_name = "__ml_not"
let addc_name = "__ml_addc"
let add_name = "__ml_add"
let sub_name = "__ml_sub"
let mul_name = "__ml_mul"
let eq_name = "__ml_eq"
let lt_name = "__ml_lt"
let le_name = "__ml_le"
let gt_name = "__ml_gt"
let ge_name = "__ml_ge"
let stage_name i = Printf.sprintf "__s%d" i
let part_name i = Printf.sprintf "__ml_p%d" i

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
  then List.rev defs, List.rev bits
  else (
    let name = stage_name i in
    let def =
      Printf.sprintf "let %s = %s %s %s %s in" name fa_name (bit i a) (bit i b) carry
    in
    let bits = bit 0 name :: bits in
    let carry = bit 1 name in
    chain a b carry (i + 1) (def :: defs) bits)
;;

(* [cmp_chain a b defs carry i] — развёрнутый вручную компаратор: тот же
   сумматор, что и {v chain}, но берётся не сумма, а перенос из 32-го разряда.
   В сумме [a' + ~b' + 1] этот перенос равен 1 тогда и только тогда, когда
   [a' >= b'] по модулю 2^32, поэтому "меньше" — это его инверсия. Оба операнда
   предварительно отличаются от [a] и [b] инверсией 31-го разряда — именно это
   превращает знаковый порядок в беззнаковый, то есть [a <s b] равносильно
   [(a xor 2^31) <u (b xor 2^31)]. *)
let rec cmp_chain a b defs carry i =
  if i = width
  then List.rev defs, "(not " ^ carry ^ ")"
  else (
    let name = stage_name i in
    (* [~b'] — это [b] с инвертированными младшими разрядами и с нетронутым
       31-м, [a'] — наоборот, [a] с инвертированным только 31-м. *)
    let abit = if i = width - 1 then "(not " ^ bit i a ^ ")" else bit i a in
    let bbit = if i = width - 1 then bit i b else "(not " ^ bit i b ^ ")" in
    let def = Printf.sprintf "let %s = %s %s %s %s in" name fa_name abit bbit carry in
    cmp_chain a b (def :: defs) (bit 1 name) (i + 1))
;;

(* [partial i] — i-е частичное произведение в схеме «сдвиг и сложение»:
   [a << i], обрезанное по биту [i] второго операнда. Бит [j] результата равен
   [b_i and a_{j-i}] при [j >= i] и нулю при [j < i].

   Здесь [and] — обычный логический оператор языка, а не прелюдийный: оба
   операнда уже Church-булвы (результаты [nth]), так что перенос не нужен.
   Нижние биты [j < i] пишутся сразу [false]: [b_i and false] — то же самое,
   но компилятор не сворачивает [and] на константах. *)
let partial i =
  tuple
    (range (fun j ->
       if j < i
       then "false"
       else Printf.sprintf "(%s and %s)" (bit i "b") (bit (j - i) "a")))
;;

let defs =
  let fa =
    Printf.sprintf
      "let %s a b c = (a xor b xor c, (a and b) or ((a xor b) and c)) in"
      fa_name
  in
  let notb =
    Printf.sprintf
      "let %s t = %s in"
      not_name
      (tuple (range (fun i -> "not (" ^ bit i "t" ^ ")")))
  in
  let addc_defs, addc_bits = chain "a" "b" "c" 0 [] [] in
  let addc =
    Printf.sprintf
      "let %s a b c = %s %s in"
      addc_name
      (String.concat " " addc_defs)
      (tuple addc_bits)
  in
  let add = Printf.sprintf "let %s a b = %s a b false in" add_name addc_name in
  (* a - b == a + ~b + 1 в дополнительном коде; перенос за 32-й разряд
     отбрасывается, то есть результат берётся по модулю 2^32. *)
  let sub =
    Printf.sprintf "let %s a b = %s a (%s b) true in" sub_name addc_name not_name
  in
  let eq =
    let differs = range (fun i -> Printf.sprintf "(%s xor %s)" (bit i "a") (bit i "b")) in
    Printf.sprintf "let %s a b = not (%s) in" eq_name (String.concat " or " differs)
  in
  let lt_defs, lt_borrow = cmp_chain "a" "b" [] "true" 0 in
  let lt =
    Printf.sprintf "let %s a b = %s %s in" lt_name (String.concat " " lt_defs) lt_borrow
  in
  (* Остальные три сравнения выводятся из [lt] перестановкой операндов и
     отрицанием, отдельного развёрнутого сумматора для них не нужно. *)
  let le = Printf.sprintf "let %s a b = not (%s b a) in" le_name lt_name in
  let gt = Printf.sprintf "let %s a b = %s b a in" gt_name lt_name in
  let ge = Printf.sprintf "let %s a b = not (%s a b) in" ge_name lt_name in
  (* Умножение — «сдвиг и сложение» без сдвига: 32 частичных произведения
     развёрнуты в {v partial}, складываются уже готовым [add]. Сдвиг не нужен
     как отдельная операция, потому что [a << i] получается ещё на этапе
     генерации — бит [j] читается из [a] по индексу [j - i]. Сложение
     ассоциативно, а все переносы отбрасываются на 32-м разряде, поэтому
     порядок суммирования безразличен и результат берётся по модулю 2^32.

     Сумма набирается вложенными вызовами, а не списком аргументов: [add]
     двухаргументный, и третьим аргументом пришёл бы не операнд сложения, а
     уже готовый результат — число, то есть кортеж из 32 Church-буллов. К
     результату применился бы тогда сам кортеж, а не функция [add]. *)
  let parts =
    List.init width (fun i -> Printf.sprintf "let %s = %s in" (part_name i) (partial i))
  in
  let rec sum i acc =
    if i = width - 1
    then Printf.sprintf "%s %s %s" add_name acc (part_name i)
    else sum (i + 1) (Printf.sprintf "(%s %s %s)" add_name acc (part_name i))
  in
  let mul =
    Printf.sprintf
      "let %s a b = %s %s in"
      mul_name
      (String.concat " " parts)
      (sum 1 (part_name 0))
  in
  [ fa; notb; addc; add; sub; eq; lt; le; gt; ge; mul ]
;;

let source = String.concat "\n" defs ^ "\n"
