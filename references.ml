(* References are pointers to locations in memory.  They are created using the ref fn. *)

let x = ref 1  (* This is a location. *)

(* the bang op (!) gets the contents of the location *)
let y = !x

(* The assignment op ( := ) MUTATES the contents at a location. *)

let () = x := 2  (* This is a side effect *)

(* y wont have changed at this point as it got the original value of the location x. *)

(* Refs can also be aliased. *)
let a = ref 42
let b = ref 42
let z = a  (*Alias.   a ref which points to the same location as a. *)

(*  Now we can see why there are multiple equality ops.
  ( == ) is physical equality; that is, do they point to the same physical location. 
  ( != ) is the negation of physical equality.  *)

let _ = a == z  (* -> true *)
let _ = a != b  (* -> true *)

(* Structural equality ( = ) is the structure of value equality.  
   The negation of structural equality is the ( <> ) op. *)
let _ = a == z  (* -> true *)
let _ = a <> b  (* -> false *)


(* Suppose we want a fn which increments a val by 1 everytime we call it.  With FP and immutability
  we cant do this.  But we can with references and side effects. 
  Note that we must have some sort of arg to next, otherwise next wouldn't be a fn, 
  rather it would just eval the counter inc resulting in an int. *)

let counter = ref 0
let next = fun () -> counter := !counter + 1 ; !counter

let ct1 = next ()  

(* Next refactored... *)
let next' () = incr counter ; !counter

(* We can encapsulate the counter ref inside next using some clever let in syntax... *)

let next'' =
  let ct = ref 0 in  (* the creation of the reference only gets created once, and the val of ref 0
                        the location is substituted into the anon fn, which is what gets returned. *)
  fun () -> 
  incr ct ;
  !ct

(* Where as this wont work... *)
let next''' () =
  let ct = ref 0 in  (* creates a new ref everytime, and hence will always return 1. *)
  incr ct ;
  !ct

  (* When we look at a ref (a say), we get something that looks like a record
    > a;;
    > : int ref = {contents = 42} 
    
  Actually, that is exactly what they are; they are a record with 1 MUTABLE field called contents *)

  type point = {x : int; y : int; mutable c : string}
  let p = {x=1; y=2; c="red"}
  (* We can change [c] with the <- op *)
  let _ = p.c <- "green"
  (* So our ref is really a type... *)
  
type 'a myref = { mutable contents : 'a } 

let myref x = { contents = x }
let ( !! ) rf = rf.contents   (* that is the op ( ! ) *)  
let ( ^^:= ) rf newval = rf.contents <- newval  (* The ( := ) op *)


(* Mutable Single Linked List *)

type 'a node = {
  value : 'a;
  mutable next : 'a  node option;  (* read types from right to left, i.e. an optional node of type 'a *)
}

(** A ['a mlist] is a mutable single linked list with elements of type ['a]. *)
type 'a mlist = {
  mutable first : 'a node option;
}

let create_node v = {
  next = None;
  value = v;
}

(** [singleton v] is a singly-linked list with exactly 1 node containing 1 value [v].  *)
let singleton v = {
  first = Some (create_node v)
}

(** [insert_first lst v] mutates the [lst] by inserting the value [v] as the first value in the list. *)
let insert_first lst v = 
  match lst.first with 
  | None -> lst.first <- Some (create_node v)  (* Where the list is empty *)
  | old_first -> 
    let new_first = create_node v in
    new_first.next <- old_first;
    lst.first <- Some new_first

(* A fn to create an empty list.  Well...this isn't a fn, its a record with first set to None. *)
let empty_not_a_fn = {
  first = None
}

(* Instead we want to give it a arg; typically unit. *)
let empty () = {
  first = None
}


