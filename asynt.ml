open Alex

type num = Int of int | Float of float

type expr =
  | Constant of num
  | Var of string
  | Sum of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Comp of string * expr * expr

type ordre =
  | Av of expr | Re of expr (* Avancer, Reculer *)
  | Td of expr | Tg of expr (* Tourner à droite, tourner à gauche *)
  | Lc | Bc (* Lève crayon, Baisse crayon *)
  | Ve (* Vider l'écran *)
  | Rep of expr * ordre list (* boucle *)
  | Fun of func
  | Exec of string * expr list
  | Si of expr * ordre list * ordre list
  | Stop
and func = {name: string; params: string list; body: ordre list}

let digit = parser
  | [< 'Int_const n >] -> Int n
  | [< 'Float_const n >] -> Float n

let rec simple_expr = parser
  | [< d = digit >] -> Constant d
  | [< 'Symbol ':'; 'Word var; >] -> Var var
  | [< 'Symbol '('; e = expr; 'Symbol ')'; >] -> e
and expr = parser
  | [< d = simple_expr; rest >] -> fin_expr d rest
and fin_expr d = parser
  | [< 'Symbol '+'; d2 = simple_expr >] -> Sum (d, d2)
  | [< 'Symbol '*'; d2 = simple_expr >] -> Mul (d, d2)
  | [< 'Symbol '-'; d2 = simple_expr >] -> Sub (d, d2)
  | [< 'Symbol '/'; d2 = simple_expr >] -> Div (d, d2)
  | [< 'Symbol '<'; rest >] ->
      (match rest with parser
         | [< 'Symbol '='; d2 = simple_expr >] -> Comp ("<=", d, d2)
         | [< d2 = simple_expr >] -> Comp (String.make 1 '<', d, d2))
  | [< 'Symbol '>'; rest >] ->
      (match rest with parser
         | [< 'Symbol '='; d2 = simple_expr >] -> Comp (">=", d, d2)
         | [< d2 = simple_expr >] -> Comp (String.make 1 '>', d, d2))
  | [< 'Symbol '='; d2 = simple_expr >] -> Comp (String.make 1 '=', d, d2)
  | [< >] -> d

let rec instr = parser
  | [< '(Word "baisse_crayon" | Word "bc") >] -> Bc
  | [< '(Word "leve_crayon" | Word "lc") >] -> Lc
  | [< '(Word "vide_ecran" | Word "ve") >] -> Ve
  | [< '(Word "avance" | Word "av"); n = expr >] -> Av n
  | [< '(Word "recule" | Word "re"); n = expr >] -> Re n
  | [< '(Word "droite" | Word "td"); a = expr >] -> Td a
  | [< '(Word "gauche" | Word "tg"); a = expr >] -> Tg a
  | [< '(Word "repete" | Word "rep"); n = expr; l = instr_list >] ->
      Rep (n, l)
  | [< 'Word "stop" >] -> Stop
  | [< 'Word "si"; cond = expr; code = instr_list; altern = instr_list >] ->
      Si (cond, code, altern)
  | [< 'Word f; args = args_list [] >] -> Exec (f, args)
and func = parser
  | [< 'Word "pour"; 'Word f; pars = params_list []; func = instr_list'; 'Symbol '.' >] ->
      Fun {name = f; params = pars; body = func}
and args_list acc = parser
  | [< n = expr; rest >] -> args_list (n::acc) rest
  | [< >] -> List.rev acc
and instr_list = parser
  | [< 'Symbol '['; l = instr_list'; 'Symbol ']' >] -> l
and instr_list' = parser
  | [< ordre = instr; rest = rest_instr_list' >] -> ordre::rest
and rest_instr_list' = parser
  | [< rest = instr_list' >] -> rest
  | [< >] -> []
and params_list list = parser
  | [< 'Symbol ':'; 'Word par; rest >] -> params_list (par::list) rest
  | [< >] -> List.rev list
and program = parser
  | [< l = instr_list'; 'Symbol '.' >] -> l
  | [< >] -> []
and logo = parser
  | [< f = func; rest = logo>] -> f::rest
  | [< l = program >] -> l

let parse str =       
  logo (lex (Stream.of_string str))
