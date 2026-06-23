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
  | App (App (Var (Name "/"), Int a), Int b) when b != 0 -> Int (a / b)
  | App (App (Var (Name "="), Int a), Int b) -> if a = b then ltrue else lfalse
  | App (App (Var (Name "="), a), b) when a = lfalse -> if a = b then ltrue else lfalse
  | App (App (Var (Name "="), a), b) when a = ltrue -> if a = b then ltrue else lfalse
  | App (App (Var (Name "<"), a), b) -> if a < b then ltrue else lfalse
  | App (App (Var (Name "<="), a), b) -> if a <= b then ltrue else lfalse
  | App (App (Var (Name ">"), a), b) -> if a > b then ltrue else lfalse
  | App (App (Var (Name ">="), a), b) -> if a >= b then ltrue else lfalse
  | term -> term
;;

let rec eval = function
  | App (t1, t2) ->
    let t1 = eval t1 in
    let t2 = eval t2 in
    (match t1 with
     | Fun body ->
       let res = subst 0 (shift 1 0 t2) body in
       eval (shift (-1) 0 res)
     | _ ->
       let t = App (t1, t2) in
       try_std t)
  | t -> t
;;
