(* A way of executing, typically side-effect based expressions in a sequence. 
  With (e1 ; e2 ; e3) e1 & e2 really should return unit.  e3 can return something. *)

let print_and_add x y = print_int (x + y) ; print_newline () ; x + y