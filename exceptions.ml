(* Exceptions are just variants.  The built-in type is exn.  All exceptions are vals of that type
  Exceptions are defined by using the "exception" constructor, which can be constant or non-constant. 
  exn is an extensible variant.  In that using the exception constructor we are creating new 
  constructors of the type exn.  That is Boom defined a type constructor for exn.*)

exception Boom
exception BoomWithMsg of string

(* raising uses the raise function *)
let boom m = 
  raise (BoomWithMsg m)

let safe_div x y = 
  try x / y with 
  | Division_by_zero -> 0