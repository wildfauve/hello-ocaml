(* Fns can have botgh named and optional args, mixed in the typical positional args. *)

(* here x and z are positional, while y is named as same as the arg is known internally.  
  We could have specified ~myy:y which means that it is known as myy outside and y inside*)
let add3 x ~y z = x + y + z

let a = add3 10 ~y:20 100

(* We cant use the named arg as the init arg in a pipeline ( |> ) but we can use the others...*)
let a' = 10 |> add3 100 ~y:20


(* Optional args are defined with ?.  Here init is optional, has a label and has a default. *)
let sum ?(init=0) lst = List.fold_left ( + ) init lst 

let sumit = sum [1;2;3]
let sumit' = sum ~init:10 [1;2;3]

(* The optional arg can also be passed in as a Option 'a, using a ?, with None taking the default. *)
let sumit'' = sum ?init:(Some 10) [1;2;3]

(* We can define sum to use a different external name for the arg... *)

let sum' ?init:(acc=0) lst = List.fold_left ( + ) acc lst 

(* Optionals can have no default value.. 
  len is the substring's length. If missing, it defaults to String.length s - pos .*)
let sub ?(pos=0) ?len:len_opt s =
  let default = String.length s - pos in
  let length = Option.value ~default len_opt in
  String.sub s pos length

(* here pos defaults to 0, as defined, but len becomes String.length - 0  *)
let substr = sub "immutability"

(* Optionals are partial application.
   In the concat_warn fn, when the fn is called without a sep, when we get a fn back--not the result.*)
let concat_warn ss ?(sep="-") = String.concat sep ss
let concat ?(sep="-") ss = String.concat sep ss

let concat_partially_applied = concat_warn ["a"; "b"]  (* returns a fn *)
let concat_fully_applied = concat ["a"; "b"]  (* returns a str *)

(* The only way to get concat_warn to return a concatenated string is to provide the optional arg. 
  Which is a contradiction.  So...NO optional args as the last args. *)

let concat_fully_applied = concat_warn ["a";"b"] ~sep:"--";;

(* Of course, what if ALL args are optional! 
   A dummy last positional arg MUST be added.  Usually we use unit ().  *)

let hello ?(who="world") () = "hello, " ^ who

let greet = hello ()
let greet' = hello ~who:"There" ()

let greet_with_option = hello ?who:None ()
let greet_with_option = hello ?who:(Some "There") ()

(* Passing and optional arg with a ? allows forwarding without unwrapping. *)

let take ?len s = sub ?len s

let takeit = take "immutable" ~len:2