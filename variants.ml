(*  Variants are "one-of" types, which is also known as sum-types.  
    that is we take the Union of n sets e.g. string U int.  This is known as a tagged union,
    where the tag is the constructor name, e.g. String or Int.
    This allows for heterogenous lists e.g. string_or_int list*)
type string_or_int =
  | String of string
  | Int of int

(*  So, a Variant is a sum (or Union) type, where as a record is a Product type (an each-of type, that is, this attr AND this attr) 
    Therefore, these types are Algebraic because they allow one-of and each-of types. 
    Hence, Algebraic Data Types (ADTs) *)

type primary_colour = Red | Yellow | Blue

type point = float * float

(* Shape is a variant type, hence the constructors with seperated by |  Constructor names must be caps. *)
type shape = 
  | Circle of {centre: point; radius: float}
  | Rectangle of {lower_left: point; upper_right: point}
  | Point of point

let c1 = Circle {centre = (0., 0.); radius = 10.}
let r1 = Rectangle {lower_left = (0., 0.); upper_right = (10., 10.)}

let p1 = Point (5., 5.)

let avg x y = 
  (x +. y) /. 2.

let centre s = 
  match s with
  | Circle {centre} -> centre 
  | Rectangle {lower_left; upper_right} -> 
    let (x_ll, y_ll) = lower_left in
    let (x_ur, y_ur) = upper_right in
    (avg x_ll x_ur, avg y_ll y_ur)
  | Point p -> p

let centre2 s = 
  match s with
  | Circle {centre} -> centre 
  | Rectangle {lower_left=(x_ll, y_ll); upper_right=(x_ur, y_ur)} -> 
    (avg x_ll x_ur, avg y_ll y_ur)
    | Point p -> p


(*  Types can be recursive *)

type intlist = 
    | Nil
    | Cons of int * intlist

let rec length = function
    | Nil -> 0
    | Cons (_, t) -> 1 + length t

let mylist1 = Cons (1, Cons (2, Nil))

(* Parameterising the intlist to support any type of list *)

type 'a mylist = 
    | PNil
    | PCons of 'a *  'a mylist


let rec my_list_length = function
| PNil -> 0
| PCons (_, t) -> 1 + my_list_length t


(* We can replace the constructors with operators or punctuation to make it look like the ocaml list type *)

type 'a mylist_like_ocaml = 
    | []
    | (::) of 'a *  'a mylist_like_ocaml
