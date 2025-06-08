let a1 = [| 1;2;3 |]

(* Use the dot syntax to get at an index *)
let _ = a1.(0)

(* Arrays are mutable *)
let _ = a1.(0) <- 10

(* Vectors using arrays *)

type vec = float array

(* This is a little imperative *)
let vec_print v = 
  for i = 0 to Array.length v - 1 do
    print_float v.(i) ; print_newline ()
  done

let vec_print' v =
  let print_elem e = print_float e; print_newline ()
  in 
  Array.iter print_elem v

let vec_print'' v = 
  Array.iter (Printf.printf "%F\n") v

(** [vec_add v1 v2] is the sum of the vectors [v1] and [v2].
    Example: [vec_add [| 1.;2. |] [| 3.;4.|]]
      is [| 4.;6. |]   *)
let vec_add v1 v2 = 
  Array.map2 ( +. ) v1 v2