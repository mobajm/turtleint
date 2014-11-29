module P = Pencil
open Asynt

exception Stop_exn

let float = function
  | Int i -> float_of_int i
  | Float f -> f

let op_num op_int op_float a b = match a, b with
  | Int x, Int y -> Int (op_int x y)
  | Float x, Float y -> Float (op_float x y)
  | (Float x, Int y) | (Int y, Float x) -> Float (op_float (float_of_int y) x)
      
let add_num = op_num (+) (+.)
let mul_num = op_num ( * ) ( *. )
let sub_num = op_num (-) (-.)
let div_num = op_num (/) (/.)

let comp_op = function
  | "<" -> (<)
  | "<=" -> (<=)
  | ">" -> (>)
  | ">=" -> (>=)
  | "=" -> (=)
  | _ -> failwith "op de comparaison inconnu"

let int_value = function
  | Int n -> n
  | Float f -> int_of_float f      

let rec expr_value env = function
  | Constant n -> n
  | Var v -> List.assoc v env
  | Sum (a, b) -> add_num (expr_value env a) (expr_value env b)
  | Mul (a, b) -> mul_num (expr_value env a) (expr_value env b)
  | Sub (a, b) -> sub_num (expr_value env a) (expr_value env b)
  | Div (a, b) -> div_num (expr_value env a) (expr_value env b)
  | Comp (c, a, b) ->
      match comp_op c (float (expr_value env a)) (float (expr_value env b)) with
        | true -> Int 1
        | false -> Int 0

let builtins = ref ([]:(string * func) list)
let get_func f = List.assoc f !builtins
let def_func f =
  builtins := (f.name, f)::!builtins      

(*_ Evaluator *)      
let rec eval' env = function
  | Av expr -> P.forward (float (expr_value env expr))
  | Re expr -> P.forward (-. (float (expr_value env expr)))
  | Tg expr -> P.turn (float (expr_value env expr))
  | Td expr -> P.turn (-. (float (expr_value env expr)))
  | Lc -> P.toggle_pencil true
  | Bc -> P.toggle_pencil false
  | Ve -> P.clear_screen ()
  | Rep (expr, exprs) ->
      for i = 1 to int_value(expr_value env expr) do eval env exprs done
  | Exec (f, args) ->
      let f = get_func f in
      let rec new_env = function
        | [], [] -> env
        | x::xs, y::ys -> (x, expr_value env y)::(new_env (xs, ys)) 
        | _ -> failwith "Le nombre de paramètres est incorrect"
      in
      eval (new_env (f.params, args)) f.body
  | Fun f -> def_func f
  | Si (cond, tr, fa) -> eval env
      (if int_value(expr_value env cond) = 0 then fa else tr)
  | Stop -> raise Stop_exn
and eval env ordres = try List.iter (eval' env) ordres with
  | Stop_exn -> ()
