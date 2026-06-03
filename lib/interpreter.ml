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
  | App (App (Var "+", Int a), Int b) -> Some (Int (a + b))
  | App (App (Var "-", Int a), Int b) -> Some (Int (a - b))
  | App (App (Var "*", Int a), Int b) -> Some (Int (a * b))
  | App (App (Var "/", Int a), Int b) when b != 0 -> Some (Int (a / b))
  | App (App (Var "=", Int a), Int b) -> Some (if a = b then ltrue else lfalse)
  | App (App (Var "=", a), b) when a = lfalse -> Some (if a = b then ltrue else lfalse)
  | App (App (Var "=", a), b) when a = ltrue -> Some (if a = b then ltrue else lfalse)
  | _ -> None
;;

let rec beta_step term =
  match try_std term with
  | Some term' -> Some term'
  | None ->
    (match term with
     | App (t, t') ->
       (match beta_step t' with
        | Some t'' -> Some (App (t, t''))
        | None ->
          (match t with
           | Fun (name, body) -> Some (subst body name t')
           | _ ->
             (match beta_step t with
              | Some t''' -> Some (App (t''', t'))
              | None -> None)))
     | _ -> None)
;;

let rec beta_reduce term =
  match beta_step term with
  | Some term' -> beta_reduce term'
  | None -> term
;;
