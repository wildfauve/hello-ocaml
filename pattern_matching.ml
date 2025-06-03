let x =
  match not true with
  | true -> "no"
  | false -> "yup"


let z = 
  match [1;2;3;4;5] with
  | [] -> None
  | head :: tail -> Some head

let first_from_tuple t = 
  match t with
  | (a,b,c) -> a

type cat = {
  n: string;
  y: int;
}

let dinsdale = {n="dinsdale"; y=1990}

let name_with_year c = 
  match c with 
  | {n; y} -> n ^ " " ^ string_of_int y

let empty lst =
  match lst with
  | [] -> true
  | _ -> false

let rec sum lst = 
  match lst with 
  | [] -> 0
  | h :: t -> h + (sum t)

let rec length lst = 
match lst with 
  | [] -> 0
  | h :: t -> 1 + (length t)

let rec append lst1 lst2 = 
  match lst1 with 
  | [] -> lst2
  | h :: t -> h :: (append t lst2)
    
(*  There is syntax sugar where we have a fn with args and we want to pattern match against 
    the last arg (or the first if only 1 arg) we can remove the last arg and the match with
    and replace with the function keyword. *)
(* So, empty becomes... *)

let empty2 = function
  | [] -> true
  | _ -> false

let rec length2 = function
  | [] -> 0
  | h :: t -> 1 + (length t)


let rec sum2 = function
  | [] -> 0
  | h :: t -> h + (sum t)

(* But this cant be done with append as we match against the first arg *)