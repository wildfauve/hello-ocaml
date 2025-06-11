module type Set = sig
   (* Maps can be see in the abstract as a SET of bindings with values.  That is, a Set of keys.  
      So we can create functional data structure using sets. *)

   (* Lets start with a functional set implementation first. *)
   type 'a t

   val empty : 'a t

   val insert : 'a -> 'a t -> 'a t

   (* val find : 'a -> 'a t -> 'a option *)

   (* val remove : 'a -> 'a t -> 'a t *)


   val mem :  'a -> 'a t -> bool

end

module ListSet : Set = struct
   (* As we use a List to represent the set, the insert (which uses mem) is an O(n) op, that is linear time. *)

   type 'a t = 'a list

   let empty = []

   let mem = List.mem

   let insert x s = 
      if mem x s then s else x :: s

end

module BstSet : Set = struct
   (* While a linear search, like our list, is O(n), binary search (that is, a binary tree) searches
      by halving the search space -> an invariant is that the array/list must be in sorted order.
      Binary search reduces to O(log n) 
      A Binary Search Tree (BST) has the following invariant; for a value v:
      + all the values in the left tree are lt v
      + all the values in the right tree are gt v *)

   (** AF: [Leaf] is the empy set.  
           [Node (l, v, r)] is the set containing [v] as well as all the elements of the sets represented
           by [l] and [r]  
       RI: For every [Node (l, v, r)] all the values in [l] are strictly less than [v] and 
           all the values in [r] are greater than v. *)

   type 'a t = Leaf | Node of 'a t * 'a * 'a t

   let empty = Leaf

   (* Remember the function syntax sugar.  The last arg (the BST) is used ib the pattern match
      so we dont need to provide it. *)
   let rec mem x = function
      | Leaf -> false
      | Node (l, v, r) -> 
         if x < v then mem x r
         else if  x > v then mem x l
         else true

   let rec insert x s = failwith "unimplemented"

end