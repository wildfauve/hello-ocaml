(* Add 1 to every element in a list; not using map, of course *)
let rec add1_to_each_elem = function
| [] -> []
| h :: t -> h + 1 :: add1_to_each_elem t

(* do something else with mapping over a list *)
let rec add_str_to_each_elem = function
| [] -> []
| h :: t -> (h ^ "!!!") :: add_str_to_each_elem t

(* Of course, both are very similar, transforming each list element with an expression, so lets refactor that... *)

let rec list_transform f = function
| [] -> []
| h :: t -> f h :: list_transform f t

(* rewriting add1_to_each_elem *)
let add1_to_each_elem' lst = list_transform (fun x -> x + 1) lst

(* or using point free / partial application *)
let add1_to_each_elem''  = list_transform (fun x -> x + 1)

(* And....list transform is actually List.map from the std lib.... *)

let x1 = List.map (fun x -> string_of_int x) [1;2;3]

(* There is something redundent about the x1 line....here we are wrapping an annon fn around a 
  fn which takes 1 arg....a common problem...this wrapping can be removed.... *)
let x2 = List.map string_of_int [1;2;3]
