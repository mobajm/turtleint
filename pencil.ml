open Graphics

(*_ round *)
let round x =
  if x >= 0.0 then
    int_of_float (x +. 0.5)
  else
    int_of_float (x -. 0.5)
      
(*_ Le crayon programmable *)
type pencil = {
  mutable x: float;
  mutable y: float;
  mutable direction: float; (* angle in radians *)
  mutable off: bool (* crayon levé ou pas *) }

(*_ Instance globale d'un crayon *)
let pencil = {x = 0.0; y = 0.0; direction = 0.0; off = false}

(*_ change l'état du crayon : levé ou pas *)  
let toggle_pencil b = pencil.off <- b

(*_ Fait tourner le crayon de theta *)
let turn theta =
  let pi = 4.0 *. atan 1.0 in
  pencil.direction <- pencil.direction +. theta *. (pi /. 180.0)

(*_ Fait avancer le crayon dans la direction désirée *)
let forward d =
  let dx = d *. cos (pencil.direction) in
  let dy = d *. sin (pencil.direction) in
  pencil.x <- pencil.x +. dx;
  pencil.y <- pencil.y +. dy;
  if pencil.off then
    moveto (round pencil.x) (round pencil.y)
  else
    lineto (round pencil.x) (round pencil.y)

(*_ Prépare le canvas pour les dessins *)      
let clear_screen () =
  let bg_color = white in
  let fg_color = black in
  let zero_x = float_of_int (size_x () / 2) in
  let zero_y = float_of_int (size_y () / 2) in
  set_color bg_color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color fg_color;
  pencil.x <- zero_x;
  pencil.y <- zero_y;
  pencil.direction <- 0.0;
  pencil.off <- false;
  moveto (round pencil.x) (round pencil.y)
