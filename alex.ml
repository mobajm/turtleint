type token =
  | Word of string
  | Symbol of char
  | Int_const of int
  | Float_const of float

let rec skip_spaces = parser
  | [< ' ('\t' | '\n' | ' '); rest >] -> skip_spaces rest
  | [< >] -> ()

let rec read_int acc = parser
  | [< ' ('0'..'9') as c; rest >] ->
      read_int (acc*10 + int_of_char c - 48) rest
  | [< >] -> acc

let rec read_float acc scale = parser
  | [< ' ('0'..'9') as c; rest >] ->
      read_float (acc +. float_of_int(int_of_char c - 48) *. scale)
        (scale /. 10.0) rest
  | [< >] -> acc

let buffer = String.make 16 '-'      

let rec read_word pos = parser
  | [< ' ('A'..'Z' | 'a'..'z' | '_') as c; rest >] ->
      if pos < String.length buffer then
        buffer.[pos] <- c;
      read_word (pos + 1) rest
  | [< >] -> String.sub buffer 0 (min pos (String.length buffer))

let read_token stream =
  skip_spaces stream;
  match stream with parser
    | [< ' ('A'..'Z' | 'a'..'z') as c >] ->
        buffer.[0] <- c;
        Word (read_word 1 stream)
    | [< ' ('0'..'9') as c >] ->
        let n = read_int (int_of_char c - 48) stream in
        begin match stream with parser
          | [< ''.' >] ->
              Float_const (read_float (float_of_int n) 0.1 stream)
          | [< >] -> Int_const n
        end
    | [< ' c >] -> Symbol c

let rec lex = parser
  | [< l = read_token; rest >] -> [< 'l; lex rest >]
  | [< >] -> [< >]
