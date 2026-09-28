(* Генератор прелюдии: арифметика над числами, которые [Lambda.compile_int]
   представляет как кортеж из 32 Church-буллов, написана на самом MiniML.

   Публичные имена, на которые ссылается компилятор:

   {v add_name v} / {v sub_name v} / {v mul_name v} / {v div_name v} /
   {v eq_name v} / {v lt_name v} / {v le_name v} / {v gt_name v} / {v ge_name v}

   Компилятор не может сам построить определение функции, поэтому исходник
   прелюдии генерируется здесь и оборачивается вокруг пользовательского
   выражения через [Ast.Let] (см. {v Lambda.with_prelude v}). *)

let width = 32
let fa_name = "__ml_fa"
let not_name = "__ml_not"
let addc_name = "__ml_addc"
let subc_name = "__ml_subc"
let add_name = "__ml_add"
let sub_name = "__ml_sub"
let mul_name = "__ml_mul"
let div_name = "__ml_div"
let eq_name = "__ml_eq"
let lt_name = "__ml_lt"
let le_name = "__ml_le"
let gt_name = "__ml_gt"
let ge_name = "__ml_ge"
let zero_name = "__ml_zero"
let neg_name = "__ml_nb"
let abs_name = "__ml_abs"
let udiv_name = "__ml_udiv"
let stage_name i = Printf.sprintf "__s%d" i
let part_name i = Printf.sprintf "__ml_p%d" i
let shifted_name i = Printf.sprintf "__ds%d" i
let pair_name i = Printf.sprintf "__dp%d" i
let diff_name i = Printf.sprintf "__dd%d" i
let taken_name i = Printf.sprintf "__dq%d" i
let rem_name i = Printf.sprintf "__dr%d" i

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
   [(__ml_fa _ _) = (сумма, перенос)]. Возвращает определения, биты суммы и
   перенос из 32-го разряда — последний нужен делению, чтобы узнать, был ли
   заём. *)
let chain a b carry =
  let rec go i defs bits carry =
    if i = width
    then List.rev defs, List.rev bits, carry
    else (
      let name = stage_name i in
      let def =
        Printf.sprintf "let %s = %s %s %s %s in" name fa_name (bit i a) (bit i b) carry
      in
      go (i + 1) (def :: defs) (bit 0 name :: bits) (bit 1 name))
  in
  go 0 [] [] carry
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

(* [div_step i r] — один шаг деления «сдвиг и вычитание» по i-му биту делимого
   [a]: остаток [r] сдвигается влево и пополняется битом [a_i], то есть
   [t = 2r + a_i], и из [t] вычитается делитель [b]: бит частного [q_i]
   равен [t >=u b], а новый остаток — [t - b] или [t].

   [t] в 32 разряда не помещается: остаток всегда меньше делителя, но [2r] уже
   может не поместиться. Переполнение ловится отдельно — [2r + a_i >= 2^32]
   тогда и только тогда, когда установлен 31-й разряд [r], — и для такого [t]
   условие [t >=u b] выполняется автоматически, ведь [b < 2^32 <= t]. Вычитание
   [t - b] при этом считается по модулю 2^32, но это тот же остаток: истинный
   [t - b] лежит в [0, b) и потому совпадает со своим остатком.

   Отдельного определения для сравнения не нужно: перенос из 32-го разряда
   [__ml_subc] — это ровно [t >=u b] по модулю 2^32, то есть сравнение
   получается бесплатно, вторым сумматором.

   Разность достаётся из пары [__ml_subc] отдельным [nth 0] — и это не
   украшение: [nth j] пары при [j >= 2] возвращает саму пару, а не бит, так
   что читать биты разности прямо из пары нельзя. Остаток выбирается [if]ом
   сразу целиком, а не по битам: число — это тоже кортеж, а значит обычная
   Church-функция, которую [if] вправе вернуть как есть. Тип этого [if] не
   [Int], но прелюдию типизатор не видит (см. {v Lambda.with_prelude v}). *)
let div_step i r =
  let shifted = shifted_name i in
  let pair = pair_name i in
  let diff = diff_name i in
  let taken = taken_name i in
  let rem = rem_name i in
  ( [ Printf.sprintf "let %s = %s %s %s %s in" shifted addc_name r r (bit i "a")
    ; Printf.sprintf "let %s = %s %s b in" pair subc_name shifted
    ; Printf.sprintf "let %s = %s in" diff (bit 0 pair)
    ; Printf.sprintf "let %s = (%s or %s) in" taken (bit 31 r) (bit 1 pair)
    ; Printf.sprintf "let %s = (if %s then %s else %s) in" rem taken diff shifted
    ]
  , rem
  , taken )
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
  let addc_defs, addc_bits, _ = chain "a" "b" "c" in
  let addc =
    Printf.sprintf
      "let %s a b c = %s %s in"
      addc_name
      (String.concat " " addc_defs)
      (tuple addc_bits)
  in
  (* Вычитание с переносом: тот же сумматор, что и {v addc}, но вместо суммы
     возвращается пара [(a - b, [a >=u b])] — второй компонент это перенос из
     32-го разряда суммы [a + ~b + 1], и он равен 1 ровно тогда, когда заёма не
     было. Инверсия [b] считается один раз, а не на каждом разряде. *)
  let subc_defs, subc_bits, subc_carry = chain "a" neg_name "true" in
  let subc =
    Printf.sprintf
      "let %s a b = let %s = %s b in %s ((%s), %s) in"
      subc_name
      neg_name
      not_name
      (String.concat " " subc_defs)
      (tuple subc_bits)
      subc_carry
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
  let zero =
    Printf.sprintf "let %s = %s in" zero_name (tuple (List.init width (fun _ -> "false")))
  in
  (* Модуль числа: [t <s 0], значит [-t], иначе [t]. Для [t = 0-2^31] получается
     [2^31] — модуль не помещается в знаковый диапазон, но в беззнаковом он
     в порядке, а деление идёт именно по модулю 2^32. *)
  let abs =
    Printf.sprintf
      "let %s t = if %s t %s then %s %s t else t in"
      abs_name
      lt_name
      zero_name
      sub_name
      zero_name
  in
  (* Беззнаковое деление: 32 шага от старшего бита делимого к младшему, каждый
     — {v div_step}. Развёрнуто, а не циклом: цикл в MiniML потребовал бы
     рекурсии по Church-числу, а она тут дороже в сотни раз. *)
  let udiv_defs = ref []
  and udiv_bits = ref [] in
  let rec udiv i r =
    if i >= 0
    then (
      let step_defs, r', q = div_step i r in
      udiv_defs := !udiv_defs @ step_defs;
      udiv_bits := q :: !udiv_bits;
      udiv (i - 1) r')
  in
  udiv (width - 1) zero_name;
  let udiv =
    Printf.sprintf
      "let %s a b = %s %s in"
      udiv_name
      (String.concat " " !udiv_defs)
      (tuple !udiv_bits)
  in
  (* Деление со знаком: модули операндов, беззнаковое деление, обратный знак.
     Частное усекается к нулю, как в OCaml, а переполнение по модулю 2^32
     даёт [0-2^31 / 0-1 = 0-2^31].

     Деление на ноль — ошибка, и вот как она выражается в языке без типов.
     [if ... then raise else ...] не годится: ветки [if] вычисляются обе (строгий
     CBV), и [raise] в невыбранной ветке уронил бы всё выражение. Поэтому обе
     ветки — лямбды, и вычисляется только выбранная, а к ней применяется
     фиктивный аргумент: [raise] оказывается в хвосте уже применённой лямбды и
     даёт ошибку. Ошибка из выбранной ветки распространяется через
     [App (App (cond, th), els)] раньше, чем [els] будет вычислена, так что
     деление на ноль останавливает всё выражение — как и раньше, когда [/] был
     примитивом. *)
  let quotient = "__ml_q" in
  let div =
    String.concat
      " "
      [ Printf.sprintf
          "let %s a b = (if %s b %s then \\x. raise else \\x."
          div_name
          eq_name
          zero_name
      ; Printf.sprintf "let %s = %s (%s a) (%s b) in" quotient udiv_name abs_name abs_name
      ; Printf.sprintf "if ((%s a %s) xor (%s b %s))" lt_name zero_name lt_name zero_name
      ; Printf.sprintf "then %s %s %s else %s" sub_name zero_name quotient quotient
      ; ") 0 in"
      ]
  in
  [ fa; notb; addc; subc; add; sub; eq; lt; le; gt; ge; mul; zero; abs; udiv; div ]
;;

let source = String.concat "\n" defs ^ "\n"
