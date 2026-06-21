open Lambda

let rec subst body x value =
  match body with
  | Var name -> if x = name then value else Var name
  | Fun (name, body) ->
    if x = name then Fun (name, body) else Fun (name, subst body x value)
  | App (t, t') -> App (subst t x value, subst t' x value)
  | Int v -> Int v
;;

let try_std = function
  | App (App (Var "+", Int a), Int b) -> Int (a + b)
  | App (App (Var "-", Int a), Int b) -> Int (a - b)
  | App (App (Var "*", Int a), Int b) -> Int (a * b)
  | App (App (Var "/", Int a), Int b) when b != 0 -> Int (a / b)
  | App (App (Var "=", Int a), Int b) -> if a = b then ltrue else lfalse
  | App (App (Var "=", a), b) when a = lfalse -> if a = b then ltrue else lfalse
  | App (App (Var "=", a), b) when a = ltrue -> if a = b then ltrue else lfalse
  | term -> term
;;

let rec eval term =
  match term with
  | App (t1, t2) ->
    let t1 = eval t1 in
    let t2 = eval t2 in
    (match t1 with
     | Fun (x, body) -> eval (subst body x t2)
     | _ ->
       let t = App (t1, t2) in
       try_std t)
  | _ -> term
;;
