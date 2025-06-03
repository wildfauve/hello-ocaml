(* Instead of mapping over a list to produce a new list, say we want to combine the elements together
in some fashion.  Like sum or concat them.  This is fold in ocaml (or reduce in other langs, like Python) *)
let rec sum = function
| [] -> 0
| h :: t -> h + sum t

let rec concat = function
| [] -> ""
| h :: t -> h ^ concat t

(* Again, like with map.ml, a bunch of duplication. *)

let rec combine init_val f = function
| [] -> init_val
| h :: t -> f h (combine init_val f t)

let sum' lst = combine 0 ( + ) lst
let concat' lst = combine "" ( ^ ) lst
(* or with partial *)
let concat'' = combine "" ( ^ )

(* We have fold_right and fold_left
  fold_right, folds in from the right... e.g.
  List.fold_right f [a;b;c] init looks like 
  f a ( f b ( f c init ) ) ) 
  The std lib implementation looks like this...*)
let rec fold_right f lst acc = match lst with
| [] -> acc
| h :: t -> f h (fold_right f t acc)

(* fold_left...
  List.fold_left f [a;b;c] init looks like  
  f ( f ( f init a ) b ) c *)
let rec fold_left f acc lst = match lst with
| [] -> acc
| h :: t -> fold_left f ( f acc h ) t

(* Why have left and right....well, not all functions are commutative.... *)
  