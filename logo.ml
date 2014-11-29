open Graphics
open Language
open Alex
open Asynt

(*_ Finally combining parse and eval *)
let () =
  let rec loop () =
    print_string ">>> ";
    let s = read_line () in
    if s <> "quit" then
      begin eval [] (parse s); loop () end
    else
      Graphics.close_graph ()
  in
  Graphics.open_graph "";
  loop ()
