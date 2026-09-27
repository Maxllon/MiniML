open Ast

exception FreeVar

type var_type =
  | Idx of int
  | Name of string

type term =
  | Var of var_type
  | Fun of term
  | App of term * term
  | Int of int
  | Error
  | Try of term * term

let ch_true = Lambd ("x", Lambd ("y", Var "x"))
let ch_false = Lambd ("x", Lambd ("y", Var "y"))
let ltrue = Fun (Fun (Var (Idx 1)))
let lfalse = Fun (Fun (Var (Idx 0)))

let rec find_pos n name = function
  | name' :: _ when name' = name -> n
  | _ :: rest -> find_pos (n + 1) name rest
  | _ -> raise FreeVar
;;

(* [nth i] и [bit i] превращаются в селектор: значение-кортеж применяется к
   цепочке из [i + 1] Church-условий, каждое из которых либо оставляет
   аргумент, либо заменяет его на [true]/[false]. *)
let nth_expander (i : int) : expr =
  let first = Lambd ("f", App (Var "f", ch_true)) in
  let second = Lambd ("f", App (Var "f", ch_false)) in
  let rec helper n : expr = if n = 0 then Var "g" else App (second, helper (n - 1)) in
  Lambd ("g", App (first, helper i))
;;

let try_std (e : expr) : expr =
  match e with
  | App (Var ("nth" | "bit"), Int i) -> nth_expander i
  | _ -> e
;;

let compile_int (n : int) : expr =
  Tuple (List.init Prelude.width (fun i -> Bool (n land (1 lsl i) <> 0)))
;;

(* Определения прелюдии разбираются один раз и кэшируются: исходник один и
   тот же для каждой строки REPL. Тело [true] дописывается только чтобы
   последний [let] был чем-то завершён — оно отбрасывается. *)
let prelude_defs =
  print_endline "here!";
  lazy
    (match Lexer.tokenize (Prelude.source ^ "\ntrue") with
     | Error _ -> failwith "Prelude: lexer error"
     | Ok tokens ->
       (match Parser.parse tokens with
        | Error e -> failwith ("Prelude: " ^ e)
        | Ok ast ->
          let rec split acc = function
            | Let (name, value, body) -> split ((name, value) :: acc) body
            | _ -> List.rev acc
          in
          print_endline "now here!";
          split [] ast))
;;

(* Оборачивает пользовательское выражение в определения прелюдии.

   Прелюдия — это обычный MiniML, но тайпчекер её не видит: он проверяет
   исходную программу, где [+] — это [TInt], а не кортеж из 32 буллов.
   Соединение происходит здесь, на этапе компиляции в бестиповые лямбды:
   [compile] раскрывает [let] в применения лямбд, а [nth] в
   {v try_std} — в селектор, и [Int] раскрывается в 32-кортеж. *)
let with_prelude (e : expr) : expr =
  List.fold_right
    (fun (name, value) acc -> Let (name, value, acc))
    (Lazy.force prelude_defs)
    e
;;

let rec compile (ctx : string list) (e : expr) : term =
  match try_std e with
  | Unit -> compile ctx (Lambd ("x", Var "x"))
  | Var "raise" -> Error
  | Var s ->
    (try
       let i = find_pos 0 s ctx in
       Var (Idx i)
     with
     | FreeVar -> Var (Name s))
  | Int v -> compile ctx (compile_int v)
  | Bool v ->
    (match v with
     | true -> ltrue
     | false -> lfalse)
  | Let (name, value, body) -> compile ctx (App (Lambd (name, body), value))
  | Let_rec (name, value, body) ->
    let z_expr =
      let helper_expr : expr =
        Lambd ("x", App (Var "f", Lambd ("y", App (App (Var "x", Var "x"), Var "y"))))
      in
      Lambd ("f", App (helper_expr, helper_expr))
    in
    compile ctx (App (Lambd (name, body), App (z_expr, Lambd (name, value))))
  | Lambd (name, expr) -> Fun (compile (name :: ctx) expr)
  | App (expr, expr') -> App (compile ctx expr, compile ctx expr')
  | If (cond, th, els) -> App (App (compile ctx cond, compile ctx th), compile ctx els)
  | Bin_op (op, a, b) -> compile ctx (bin_to_expr op a b)
  | Un_op (op, expr) -> compile ctx (un_to_expr op expr)
  | Tuple tuple -> compile ctx (compile_tuple tuple)
  | Constr (_, idx, total, _) ->
    let rec helper i : expr =
      if i = total
      then App (Var ("a" ^ string_of_int idx), Var "x")
      else Lambd ("a" ^ string_of_int i, helper (i + 1))
    in
    compile ctx (Lambd ("x", helper 0))
  | Case (a, case_body) ->
    let rec helper acc case_body : expr =
      match case_body with
      | [] -> acc
      | (_, name, body) :: rest -> helper (App (acc, Lambd (name, body))) rest
    in
    compile ctx (helper a case_body)
  | Try (e1, e2) -> Try (compile ctx e1, compile ctx e2)

and compile_tuple (tuple : expr list) : expr =
  match tuple with
  | [] -> Lambd ("f", Var "f")
  | expr :: rest -> Lambd ("f", App (App (Var "f", expr), compile_tuple rest))

and bin_to_expr (op : bin_op) (a : expr) (b : expr) : expr =
  (* [+, -, =] считаются прелюдией на битах. Остальные примитивы остаются
     свободными именами: их разбирает [Interpreter.try_std], раскодируя
     операнды (кортежи из Church-булсов) в машинный int. *)
  let prim (name : string) : expr = App (App (Ast.Var name, a), b) in
  match op with
  | Add -> prim Prelude.add_name
  | Sub -> prim Prelude.sub_name
  | Eq -> prim Prelude.eq_name
  | Neq -> un_to_expr Not (prim Prelude.eq_name)
  | Mult -> prim "*"
  | Div -> prim "/"
  | Lt -> prim "<"
  | Le -> prim "<="
  | Gt -> prim ">"
  | Ge -> prim ">="
  | And -> App (App (a, b), ch_false)
  | Or -> App (App (a, ch_true), b)
  | Xor -> App (App (a, un_to_expr Not b), b)

and un_to_expr (op : un_op) (term : expr) : expr =
  match op with
  | Not -> App (App (term, ch_false), ch_true)
  | Neg -> bin_to_expr Sub (Int 0) term
;;

let ast_to_term (e : expr) = compile [] (with_prelude e)

let rec term_to_string = function
  | Var (Name s) -> s
  | Var (Idx i) -> "i" ^ string_of_int i
  | Int v -> string_of_int v
  | Fun body -> "(λ" ^ "." ^ term_to_string body ^ ")"
  | App (term, term') -> "(" ^ term_to_string term ^ " " ^ term_to_string term' ^ ")"
  | Error -> "error"
  | Try (term1, term2) ->
    "(try " ^ term_to_string term1 ^ " with " ^ term_to_string term2 ^ ")"
;;
