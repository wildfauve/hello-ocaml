(* Lets make our own map / dictionary / etc, starting with the ADT; Abstract Data Type. *)
module type Map = sig

  (** [('k,'v) t] is the type of maps that binds keys of type ['k] to values of type ['v].  *)
  (* cause the type 't needs to be a type constructor parameterised on a type var for the keys
    and a type var for the vals. *)
  type ('k, 'v) t

  (** [insert k v m] is the same map [m], but with an additional binding from [k] to [v]. 
      if [k] was already bound in [m], that binding is replaced by the binding to [v] in the new map. *)
  (* val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t  *)
  
  (** [find k m] is [Some v] if [k] is bound to [v] in [m] else [None].  *)
  (* val find : 'k -> ('k, 'v) t -> 'v option *)

  (** [remove k m] is the same map [m] but without the binding of [k].  If [k] is not 
      bound in [m] then the map [m] is unchanged.  *)
  (* val remove : 'k -> ('k, 'v) t -> ('k, 'v) t  *)

  (** [empty] is the empty map. *)
  val empty : ('k, 'v) t

  (** [of_list lst] is a map containing the same bindings as the association list [lst] 
      Requires: [lst] does not contain duplicate keys. *)
  val of_list : ('k * 'v) list -> ('k, 'v) t

  (** [bindings m] is an association list containing the same bindings as [m].  
      There are no duplicate keys in the list. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list
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

  (* let insert k v m =
    failwith "Unimplemented" *)
  

  (* let find k m =
    failwith "Unimplemented"

  let remove k m =
    failwith "Unimplemented" *)

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

module MyMap = struct
  let empty = []
end