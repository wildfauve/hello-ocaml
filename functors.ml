(* I'm not sure that these are actually functors; which strictly must only have an identity and map fn. *)

module type X = sig
  val x : int
end

module A : X = struct
  let x = 0
end

(* The function keyword here takes in a module value and produces a module value
   The module is named M locally and is specified to have type X *)
module IncX = functor (M : X) -> struct
  let x = M.x + 1  (* whatever x was inside the input to, add 1 to it. *)
end

(* Now we bind the output of the functor, a module, to a new module *)
module B = IncX(A)
let inc = B.x  (* => 1 *)

module C = IncX(B)

let inc' = C.x  (* => 2 *)

(* An example of using functors in this manner is all through the stdlib.  We'll look at map; which
  is like a Python dictionary *)

(* The map module contains a functor called Make which makes a map data structure based on an input
  which is, itself, a structure (OrderedType). So to create a map we need to pass a module into 
  the Make function which contains the type for the keys and a compare fn. The output of Make returns
  a map of type Map.S https://ocaml.org/manual/5.3/api/Map.S.html*)

(* Lets create a map containing days of the week. *)

type day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

(* And a fn which maps a day to an int *)
let int_of_day = function
  | Mon -> 1
  | Tue -> 2 
  | Wed -> 3 
  | Thu -> 4 
  | Fri -> 5 
  | Sat -> 6 
  | Sun -> 7

(* Now, if we want to create a map whose key is days.  We need to create a module which specifies the key
  type and a compare fn. *)
module DayKey = struct
  type t = day
  let compare day1 day2 = 
  int_of_day day1 - int_of_day day2
end

module DayMap = Map.Make(DayKey)

type day_spec = {
  name: string;
  pos: int;
}


let m =
  let open DayMap in 
    empty
    |> add Mon {name = "Monday"; pos = 1}  (* The value in the map can be any value type, as long as its the same for all values.*)
    |> add Tue {name = "Tuesday"; pos = 2}


let has_mon = DayMap.mem Mon m
let val_of_key = DayMap.find Mon 

(* We cant see inside the implementation when printing m in utop, because the implemenation type is 
  sealed through its interface.  However try:
  > bindings m;;  
  bindings is also from Map.S sig *)

(* Obviously the map m is immutable. *)
let m' = DayMap.add Fri {name="Friday"; pos=5} m

