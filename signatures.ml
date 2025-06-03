(* Signatures are sort of like interfaces.  Its like a value module, but in this case
  we are specifying the types to be found. 
  These are also the only names which wilkl be exposed externally for a module that implements 
  this signature; hence why we dont need to nest helper functions to hide them. *)
module type Fact = sig
  (* [fact n] is [n] factorial. *)
  val fact : int -> int

end

(* Then we can provide modules which HAVE this type. Note that the module has a type of Fact *)
module RecursiveFact : Fact = struct
  let rec fact n = 
    if n = 0 then 1 else n * fact (n - 1)
end

(* module NotAFact : Fact = struct
  let inc x = x + 1
end *)

module TailRecursiveFact : Fact = struct
  
  let rec fact_acc n acc =
    if n = 0 then acc else
      fact_acc (n - 1) (n * acc)

  let fact n =
    fact_acc n 1
end

(* With the Tail recursive version, we can use the (helper fn) fact outside the module,  
but we cant use the fact (actually fact_aux) outside the module. *)

let x = TailRecursiveFact.fact 10

(* but not *)
(* let x' = TailRecursiveFact.fact_aux 10 1 *)