open Lambda

let rec shift d c = function
  | Var (Idx k) -> if k < c then Var (Idx k) else Var (Idx (k + d))
  | Fun t -> Fun (shift d (c + 1) t)
  | App (t1, t2) -> App (shift d c t1, shift d c t2)
  | t -> t
;;

(* [j->s](\.t) *)
let rec subst j s t =
  match t with
  | Var (Idx k) when k = j -> s
  | Fun t1 -> Fun (subst (j + 1) (shift 1 0 s) t1)
  | App (t1, t2) -> App (subst j s t1, subst j s t2)
  | _ -> t
;;

let try_std = function
  | App (App (Var (Name "+"), Int a), Int b) -> Int (a + b)
  | App (App (Var (Name "-"), Int a), Int b) -> Int (a - b)
  | App (App (Var (Name "*"), Int a), Int b) -> Int (a * b)
  | App (App (Var (Name "/"), Int a), Int b) -> if b = 0 then Error else Int (a / b)
  | App (App (Var (Name "="), Int a), Int b) -> if a = b then ltrue else lfalse
  | App (App (Var (Name "="), a), b) when a = lfalse -> if a = b then ltrue else lfalse
  | App (App (Var (Name "="), a), b) when a = ltrue -> if a = b then ltrue else lfalse
  | App (App (Var (Name "<"), a), b) -> if a < b then ltrue else lfalse
  | App (App (Var (Name "<="), a), b) -> if a <= b then ltrue else lfalse
  | App (App (Var (Name ">"), a), b) -> if a > b then ltrue else lfalse
  | App (App (Var (Name ">="), a), b) -> if a >= b then ltrue else lfalse
  | term -> term
;;

let is_val = function
  | App _ | Try _ -> false
  | _ -> true
;;

let is_term t = not (is_val t)

let rec eval term =
  match try_std term with
  | Try (t1, t2) ->
    let v = eval t1 in
    (match v with
     | Error -> eval t2
     | _ -> v)
  | App (t1, Error) when is_term t1 ->
    let _ = eval t1 in
    Error
  | App (Error, v2) when is_val v2 -> Error
  | App (t1, t2) when is_term t1 && is_term t2 -> eval (App (t1, eval t2))
  | App (t1, v2) when is_term t1 && is_val v2 -> eval (App (eval t1, v2))
  | App (v1, t2) when is_val v1 && is_term t2 -> eval (App (v1, eval t2))
  | App (v1, v2) when is_val v1 && is_val v2 ->
    (match v1 with
     | Fun body ->
       let res = subst 0 (shift 1 0 v2) body in
       eval (shift (-1) 0 res)
     | _ -> Error)
  | t when is_val t -> t
  | _ -> failwith "Error: incorrect interpreter, should never reach here"
;;
