open Lambda

(* ---------- Значения ---------- *)
type value =
  | VInt of int
  | VBool of bool
  | VClosure of term * value list
  | VError

(* ---------- Интерпретатор на окружениях ---------- *)
let rec eval (env : value list) (t : term) : value =
  match t with
  | Int n -> VInt n
  | Var (Idx k) -> List.nth env k
  | Var (Name _) -> VClosure (t, env) (* примитив как значение *)
  | Fun body -> VClosure (body, env)
  | Try (t1, t2) ->
    (match eval env t1 with
     | VError -> eval env t2
     | v -> v)
  | App (t1, t2) ->
    let v1 = eval env t1 in
    if v1 = VError
    then VError
    else (
      let v2 = eval env t2 in
      if v2 = VError
      then VError
      else (
        match v1 with
        | VClosure (body, env') -> eval (v2 :: env') body
        | _ -> VError))
  (* [raise] компилируется в [Error], и это такой же терм, как остальные:
     он вычисляется в значение ошибки в любой позиции, а не только когда стоит
     сам по себе. Иначе прелюдия не смогла бы выразить «деление на ноль»:
     ветки [if] вычисляются обе, и ошибка в невыбранной ветке требовала бы
     отложить её до применения лямбды. *)
  | Error -> VError
;;

let run t = eval [] t
