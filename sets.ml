(* The data abstraction specification for a set *)
(* Note that comments which start with ** are used by ocaml doc tools; https://ocaml.org/manual/5.0/doccomments.html *)
module type Set = sig
  (** ['a t] is the type of a set whose elements have type ['a]. *)
  type 'a t

  (** [empty] is the empty set. *)
  val empty : 'a t

  (** [size s] is the number of elements in [s]. [size empty] is [0].  *)
  val size : 'a t -> int

  (** [add x s] is a set containing all the elements of [s] 
      as well as the elements of [x]. *)
  val add : 'a -> 'a t -> 'a t

  (** [mem x s] is [true] iff [x] is a member of [s].  *)
  val mem : 'a -> 'a t -> bool

  (** [union s1 s2] is the set containing both elements of [s1] and [s2].  *)
  val union : 'a t -> 'a t -> 'a t

  (** [string s] is a representation of [s] as a string. *)
  val string : ('a -> string) -> 'a t -> string
end

(** [dedup lst] is [lst] but with no dups.  *)
let dedup lst =
  lst |> List.sort_uniq Stdlib.compare

let interior string_of_elem h t =
  t
  |> List.map string_of_elem
  |> List.fold_left (fun acc elt -> acc ^ ", " ^ elt ) (string_of_elem h)

let string_of_list string_of_elem = function
  | [] -> "{}"
  | h :: t -> "{" ^ interior string_of_elem h t ^ "}"



(* Implementation of set as a list *)
module ListSet : Set = struct
  (** The list [a1; ... an] represents a set [{b1; ... bn}]
      where [b1; ... bn] is the same list as [a1; ..an]
      but with duplicates removed.
      The empty list [[]] is the empty set.  The list must not contain dups.*)
  type 'a t = 'a list

  let empty = []

  let size = List.length

  let mem = List.mem

  (* This fn is linear time *)
  let add x s = 
    if mem x s then s else x :: s 
  
  let union s1 s2 = 
    s1 @ s2 |> List.sort_uniq Stdlib.compare
  
  let string f s = 
    s |> string_of_list f
end

module ListSetWithDups : Set = struct
  (** The list [a1; ... an] represents a set {a1; ... an}.
      The empty list [[]] is the empty set.  The list can contain dups.*)
  type 'a t = 'a list

  let empty = []

  let mem = List.mem

  (* Now this is not efficient, in fact it is quadratic time O(n^2) *)
  let rec size' = function
  | [] -> 0
  | h :: t -> (if mem h t then 0 else 1) + size' t

  (* sort_uniq removes the dups and runs in linearithmic time (O(n log n)) *)
  let rec size s = 
    s |> dedup |> List.length


  (* This fn is constant time *)
  let add = List.cons

  let union = List.append

  let string f s = 
    s |> dedup |> string_of_list f

end