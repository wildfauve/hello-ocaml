module MyList = struct

type 'a mylist = 
  | Nil
  | Cons of 'a *  'a mylist

let rec map f = function
| Nil -> Nil
| Cons (h,  t) -> Cons (f h, map f t)


end

module Tree = struct

type 'a tree =
| Leaf
| Node of 'a * 'a tree * 'a tree

let rec map f = function
  | Leaf -> Leaf
  | Node (value, left, right) -> Node (f value, map f left, map f right)

end

let lst = MyList.map succ (Cons (1, Nil))

(* Note that defined Tree.Node ends with the type inference working out that we Leaf is also in that module. *)
let tree = Tree.Node (1, Leaf, Leaf)
(* Or we can annotate the type *)
let tree' : int Tree.tree = Node (1, Leaf, Leaf)
