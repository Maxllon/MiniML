open Ast

type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * expr * env
  | VRecClosure of string * string * expr * env

and env = (string * value) list

let ( let* ) = Result.bind

let rec eval env = function
  | Int n -> Ok (VInt n)
  | Bool b -> Ok (VBool b)
  | Var name -> find env name
  | Bin_op (op, l, r) ->
    let* left = eval env l in
    let* right = eval env r in
    eval_bin_op op left right
  | Un_op (op, e) ->
    let* expr = eval env e in
    eval_un_op op expr
  | If (cond, _then, _else) ->
    (match eval env cond with
     | Ok (VInt 0) -> eval env _else
     | Ok (VInt _) -> eval env _then
     | Ok (VBool false) -> eval env _else
     | Ok (VBool true) -> eval env _then
     | _ -> Error "cond must be int or bool")
  | Lambd (param, body) -> Ok (VClosure (param, body, env))
  | App (f, arg) ->
    let* vf = eval env f in
    let* varg = eval env arg in
    (match vf with
     | VClosure (param, body, clo_env) -> eval ((param, varg) :: clo_env) body
     | VRecClosure (name, param, body, clo_env) ->
       let closure = VRecClosure (name, param, body, clo_env) in
       eval ((param, varg) :: (name, closure) :: env) body
     | _ -> Error "Cannot apply: not a function")
  | Let (name, value, body) ->
    let* v = eval env value in
    eval ((name, v) :: env) body
  | Let_rec (name, value, body) ->
    let* v = eval env value in
    (match v with
     | VClosure (param, body_fun, clo_env) ->
       let closure = VRecClosure (name, param, body_fun, clo_env) in
       eval ((name, closure) :: clo_env) body
     | _ -> Error "Let rec binds only function")

and find env name =
  match env with
  | [] -> Error ("Unbound variable: " ^ name)
  | (n, v) :: _ when n = name -> Ok v
  | _ :: rest -> find rest name

and eval_bin_op op l r =
  match l, r with
  | VInt a, VInt b -> eval_bin_int_int op a b
  | VBool a, VBool b -> eval_bin_bool_bool op a b
  | _ -> Error "Error in eval bin op: Type mismatch"

and eval_bin_int_int op l r =
  match op with
  | Add -> Ok (VInt (l + r))
  | Sub -> Ok (VInt (l - r))
  | Mult -> Ok (VInt (l * r))
  | Div ->
    (match r with
     | 0 -> Error "Division by zero"
     | _ -> Ok (VInt (l / r)))
  | Eq -> Ok (VBool (l = r))
  | Neq -> Ok (VBool (l <> r))
  | Lt -> Ok (VBool (l < r))
  | Le -> Ok (VBool (l <= r))
  | Gt -> Ok (VBool (l > r))
  | Ge -> Ok (VBool (l >= r))
  | _ -> Error "Unknown bin op for <Int, Int>"

and eval_bin_bool_bool op l r =
  match op with
  | Eq -> Ok (VBool (l = r))
  | Neq -> Ok (VBool (l <> r))
  | And -> Ok (VBool (l && r))
  | Or -> Ok (VBool (l || r))
  | Xor -> Ok (VBool (l <> r))
  | _ -> Error "Unknown bin op for <Bool, Bool>ьаг "

and eval_un_op op v =
  match op, v with
  | Not, VBool b -> Ok (VBool (not b))
  | Neg, VInt n -> Ok (VInt (-n))
  | _ -> Error "Type mismatch in unary operation"
;;

let value_to_string = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VClosure (param, _, _) -> "<fun " ^ param ^ ">"
  | VRecClosure (name, _, _, _) -> "<fun " ^ name ^ ">"
;;
