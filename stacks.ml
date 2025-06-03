(* There are a couple of ways to define an interface to a module.  In-line, which is this
  example, or in a separate .mli file. *)

module type StackSig = sig

  (* A generalised signature for the 2 different stack types *)

  type 'a stack   (* this is an abstract type, it declares the type, but does not define it
                     as type 'a stack = 'a list would.
                     we could have declared it as type 'a t to show it was abstract.*)

  val empty : 'a stack

  val push : 'a -> 'a stack -> 'a stack

  val peek : 'a stack -> 'a

  val pop : 'a stack -> 'a stack
  
end

module MyStack = struct
  type 'a stack =
  | Empty
  | Entry of 'a * 'a stack

  let empty = Empty

  let push x s = Entry (x, s)

  let peek = function
  | Empty -> failwith "Its Empty!"
  | Entry (x, _) -> x

  let pop = function
  | Empty -> failwith "Its Empty!"
  | Entry (_, s) -> s

end

(* Of course, this stack is just a list; Empty, Entry == Nil, Cons *)
module ListStack : StackSig = struct
  type 'a stack = 'a list   (* using the sig means that the type is always going to be 'a stack 
                              meaning we have hidden the fact that ListStack is a 'a list *)
  let empty = []

  let push x s = x :: s

  let peek = function
  | [] -> failwith "stack empty"
  | x :: _ -> x

  let pop = function
  | [] -> failwith "stack empty"
  | _ :: s -> s
end

let list_stack = ListStack.empty
let list_stack' = ListStack.push 1 list_stack

(* This is a functional data structure.  That is, the original stack is not changed, its immutable.
  The function takes an old value of the thing and returns a new value for the thing.
  Functional data structures are persistent rather than ephermeral. *)

(* Looking at module scopes *)

let s1 = ListStack.peek (ListStack.push 42 ListStack.empty)
(* Inside the () all of the names from ListStack will be in scope. *)
let s2 = ListStack.(peek (push 42 empty))
(* We can do the same with the pipeline op *)
let s3 = ListStack.(empty |> push 42 |> peek)
(* There is also a concept called "local open", with open makes all the names within the module available *)
let s4 = 
  let open ListStack in
  empty |> push 42 |> peek

(* There is even a global open, but dont do this. *)
open ListStack
let s5 = empty |> push 42 |> peek