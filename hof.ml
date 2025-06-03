(* Higher-order functions means that functions ARE values and can be passed around as such. *)

let double x = x * 2

let sq x = x * x

let quad x = x * 4

let quad' x = double (double x)
(* Using pipeline op *)
let quad'' x = x |> double |> double

(* of course, this is just applying a function twice, which can be a more generate function... *)

let twice f x = f (f x)

(* rewriting quad again to use twice... *)
let quad''' x = twice double x

(* But we can make quad point free by leaving off the arg, because quad''' now gives back a function 
   *)
let quad'''' = twice double

(* Then.. *)
let x = quad'''' 10