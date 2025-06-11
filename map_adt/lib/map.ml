(* Lets make our own map / dictionary / etc, starting with the ADT; Abstract Data Type. *)
module type Map = sig

  (** [('k,'v) t] is the type of maps that binds keys of type ['k] to values of type ['v].  *)
  (* cause the type 't needs to be a type constructor parameterised on a type var for the keys
    and a type var for the vals. *)
  type ('k, 'v) t

  (** [insert k v m] is the same map [m], but with an additional binding from [k] to [v]. 
      if [k] was already bound in [m], that binding is replaced by the binding to [v] in the new map. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  
  (** [find k m] is [Some v] if [k] is bound to [v] in [m] else [None].  *)
  val find : 'k -> ('k, 'v) t -> 'v option

  (** [remove k m] is the same map [m] but without the binding of [k].  If [k] is not 
      bound in [m] then the map [m] is unchanged.  *)
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t 

  (** [empty] is the empty map. *)
  val empty : ('k, 'v) t

  (** [of_list lst] is a map containing the same bindings as the association list [lst] 
      Requires: [lst] does not contain duplicate keys. *)
  val of_list : ('k * 'v) list -> ('k, 'v) t

  (** [bindings m] is an association list containing the same bindings as [m].  
      There are no duplicate keys in the list. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

(* If we use Arrays as an implementation of the map_adt, we actually going to need another ADT,
    as the types change. Of course arrays are mutable, so its nolonger a functional data structure.  *)
   
module type DirectAddressMap = sig

  (** [t] is the type of maps that binds keys of type int to values of type ['v].  *)
  type 'v t

  (** [insert int v m] mutates map [m] with an additional binding from [int] to [v]. 
      if [int] was already bound in [m], that binding is replaced by the binding to [v] in existing map. 
      Requires: [k] is in bounds for [m].  *)
  val insert : int -> 'v -> 'v t -> unit
  
  (** [find k m] is [Some v] if [k] is bound to [v] in [m] else [None].  
      Requires: [k] is in bounds for [m].  *)
  val find : int -> 'v t -> 'v option

  (** [remove int m] mutates the map [m] removing the binding at [int].  If [int] is not 
      bound in [m] then the map [m] is unchanged.
      Requires: [k] is in bounds for [m].  *)
  val remove : int -> 'v t -> unit

  
  (** [create c] creates a map with the capacity of [c]. Leys [0] to [c-1] are 
      _in bounds_ for the map.  *)
  val create : int -> 'v t

  (** [of_list c lst] is a map containing the same bindings as the association list [lst]
      and with the capacity [c]. 
      Requires: [lst] does not contain duplicate keys, 
      and every key [k] is in bounds for  capacity [c].  *)
  val of_list : int -> (int * 'v) list -> 'v t

  (** [bindings m] is an association list containing the same bindings as [m].  
      There are no duplicate keys in the list. *)
  val bindings : 'v t -> (int * 'v) list
end

(* The first implementation of the map ADT uses Association Lists. *)

module AssocListMap : Map = struct

  (* AF = Abstraction Function...see videos 
     RI = Representation Invariant *)
  (** AF: [[(k1, v1); (k2, v2); ...; (kn, vn)]] is the map 
          {k1: v1, k2: v2, ..., kn: vn}.  If a key appears more than once in a list, then in the map
          it is bound to the left-most occurance in the list.  For example:
          [[(k, v1), (k, v2)]] is the map {k: v1}.  The empty list represents the empty map.
      RI: None*)
      
  type ('k, 'v) t = ('k * 'v) list

  let insert k v m =
    (k, v) :: m
  

  (* let find k m =
    List.assoc_opt k m *)
  (* Notice that we can refactor the above into the following due to partial appl.
     find is the same fn as List.assoc 
     CAUSE assoc_opt TAKES THE SAME ARGS IN THE SAME ORDER! *)
  let find = List.assoc_opt

  let remove k m =
    List.filter (fun (key, _) -> key <> k) m

  let empty = []

  let of_list lst =
    lst

  let keys m = 
    m |> List.map fst |> List.sort_uniq Stdlib.compare

  let binding m k =
    (k, List.assoc k m)

  let bindings m = 
    List.map (binding m) (keys m)
  
end

module ArrayMap : DirectAddressMap = struct
  (** AF: [[|Some v0; Some v1; ...|]] represents {0: v0, 1: v1, ...} if element [i] of the array
          is [None], then [i] is not bound in the map.
      RI:  None *)
  type 'v t = 'v option array

  let insert k v m = 
    m.(k) <- Some v

  let create c = Array.make c (None)

  let remove k m =
    m.(k) <- None

  let find k m = 
    m.(k)

  let of_list c lst = 
    let m = create c in
    List.iter (fun (k,v) -> insert k v m) lst;
    m

  let build_binding b k v =
    match v with
    | None -> ()
    | Some v -> b := (k,v) :: !b 

  let bindings m = 
    let b = ref [] in
    Array.iteri (build_binding b) m;
    !b 

end