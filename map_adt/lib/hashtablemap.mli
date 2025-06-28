(** [('k, 'v) t] is the type of mutable table-based maps that bind keys of type ['k] 
    to values of type ['v].*)
type ('k, 'v) t

(** [insert k v m] mutates map [m] to bind [k] to [v].  If [k] is already bound in [m], 
    that binding is replaced with [v].  *)
val insert : 'k -> 'v -> ('k, 'v) t -> unit

(** [find k m]  *)
val find : 'k -> ('k, 'v) t -> 'v option

(** [remove k m]  *)
val remove : 'k -> ('k, 'v) t -> unit

(** [create hash c] creates a new hash table map with capacity [c] that will use the hash fn [hash]
    to convert keys to ints.   
    Requires: [hash] distributes keys uniformly over ints.  *)
val create : ('k -> int) -> int -> ('k, 'v) t
