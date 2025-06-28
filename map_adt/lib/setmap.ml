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

   (** [insert x s]  
      The insertion order of the values into the set matters in terms of the balance of the tree.
      For example, if the vals are inserted in ascending order, then the tree will be imbalanced
      to the right, hence giving it the same efficiency as a list, which is linear (O(n)). 
      The performance of an ascending insert into the BST is 2-3x greater than the List implementation.
      On the otherhand, random order insert is an order of magnitude better than the List. 
      
      The best case BST would look something like this..
                           4
                        /     \
                       2       6
                     /   \   /   \
                    1     3 5     7
      *)
   let rec insert x = function
      | Leaf -> Node (Leaf, x, Leaf) (* With an empty tree, we create a node with x as the value 
                                        and 2 empty trees left and right. *)
      | Node (l, v, r) as node -> 
         if x < v then Node (insert x l, v, r)
         else if x > v then Node (l, v, insert x r)
         else node


end

module RedBlackTree : Set = struct
   (* A Red Black tree is one type of data structure which attempts to deal with imbalanced BSTs.
      RI: 
         + Its a BST.
         + No red node has a red child.
         + Every path from root to leaf has the same number of black nodes.
          *)
   (** AF: [Leaf] is the empty set. [Node (c, l, v, r)] is the set containing [v] as well as all the
           all the elements of the sets [l] and [r].  
       RI: The BST invariant holds, as well as the RB Tree invariants. *)
   type colour = Red | Black

   type 'a t = Leaf | Node of (colour * 'a t * 'a * 'a t)

   let empty = Leaf

   let rec mem x = function
   | Leaf -> false
   | Node (_, l, v, r) -> 
      if x < v then mem x r
      else if  x > v then mem x l
      else true


   (** [balance c l v r] implements the 4 possible rotations (ONLY 4) that could be necessary to 
       balance a node and restore the RI clause about red nodes.  *)
   let balance = function
   | (Black, Node (Red, Node (Red, a, x, b), y, c), z, d)
   | (Black, Node (Red, a, x, Node (Red, b, y, c)), z, d)
   | (Black, a, x, Node (Red, Node (Red, b, y, c), z, d))
   | (Black, a, x, Node (Red, b, y, Node (Red, c, z, d)))
   -> Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
   | t -> Node t

   let rec insert_aux x = function
      | Leaf -> Node (Red, Leaf, x, Leaf)  (* colour new node Red *)
      | Node (c, l, v, r) as n ->
         if x < v then balance (c, insert_aux x l, v, r)
         else if x > v then balance (c, l, v, insert_aux x r)
         else n

   let rec insert x s =
      match insert_aux x s with
      | Leaf -> failwith "impossible"  (* [insert_aux] can not return a Leaf. *)
      | Node (_, l, v, r) -> Node (Black, l, v, r)  (* colour the root black. *)


end

(* 
   let s = empty |> insert 5 |> insert 4 |> insert 9 |> insert 8 |> insert 1;;

Node 
   (
      Node (
         Node (
            Leaf, 
            1, 
            Leaf
         ), 
         4, 
         Leaf
      ), 
      5,
      Node (
         Node (
            Leaf, 
            8, 
            Leaf
         ), 
         9, 
         Leaf
      )
   ) 
*)
