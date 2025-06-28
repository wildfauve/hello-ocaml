(** AF: If [buckets] is 
        [| 
          [(k11, v11); (k12, v12); ...]; 
          [(k21, v21); (k22, v22); ...]; 
          ...
        |]  
        that represents the map 
        {
          k11: v11, k12: v12, ...,
          k21: v21, k22: v22, ...,
          ...,
        }
    RI: No key appears more than once in the array (no dups in the association lists).
        All keys are in the right buckets.  If [k] is in [bucket] at index [b]
        then [hash(k) = b].
        The output of the [hash] must be non-negative.*)
type ('k, 'v) t = { 
  hash : 'k -> int;
  mutable size : int;
  mutable buckets : ('k * 'v) list array
}

(** [capacity m] is the number of buckets in [m].  *)
let capacity m =
  Array.length m.buckets

(** [index k m] is the index at which key [k] should be stored in the buckets of [m].  *)
let index k m = 
  m.hash k mod (capacity m)

let insert_no_resize k v m = 
  let b = index k m in
  let old_bucket = m.buckets.(b) in
  let trimmed_bucket = List.remove_assoc k old_bucket in
  m.buckets.(b) <- (k, v) :: trimmed_bucket;
  if not (List.mem_assoc k old_bucket) then
    m.size <- m.size + 1;
  ()


(** [load_factor m] is the load factor of the map [m].  Load factor is the number of bindings
    div the number of buckets.  *)
let load_factor m =
  float_of_int m.size /. float_of_int (capacity m)


(** [rehash m new_capacity] replaces the buckets array of [m] with a new array of size [new_capacity], 
    and reinserts all the bindings of [m] into the new array. The keys are rehashed, so the bindings 
    are __likely__ to land in new buckets. *)
let rehash m new_capacity =
  (* [rehash_binding] and [rehash_bucket] are like nested fns.  As such they have access to the
      args passed in to the main fn, much like a closure. *)
  let rehash_binding (k, v) =
    insert_no_resize k v m
  in 
  let rehash_bucket b =
    List.iter rehash_binding b
  in
  let old_buckets = m.buckets in 
  m.buckets <- Array.make new_capacity [];
  m.size <- 0;
  Array.iter rehash_bucket old_buckets


(** [resize_if_needed m] resizes and re-hashes the map [m] if the load size is either too big
    of too small.  Load factors are allowed to range from 0.5 to 2.  *)
let resize_if_needed m = 
  let lf = load_factor m in
  if lf > 2.0 then
    rehash m (capacity m * 2)
  else if lf < 0.5 then
    rehash m (capacity m / 2)
  else 
    ()

(** [insert k m]  *)
let insert k v m = 
  insert_no_resize k v m;
  resize_if_needed m

(** [find k m]  *)
  let find k m = 
  List.assoc_opt k m.buckets.(index k m)


let remove_no_resize k m =
  let b = index k m in
  let old_bucket = m.buckets.(b) in
  m.buckets.(b) <- List.remove_assoc k m.buckets.(b);
  if List.mem_assoc k old_bucket then
    m.size <- m.size - 1;
  ()

(** [remove k m]  *)
let remove k m = 
  remove_no_resize k m;
  resize_if_needed m

(** As we are targetting a bucket load factor of 1 then [c] is a good number of buckets to create.
    [create h c] creates a new hash table map with capacity [c] that will use the hash fn [hash] to 
    convert keys into ints.
    Requires: [hash] distributes keys uniformly over ints and the output of [hash] is non-negative
              because a -ve bucket makes no sense.
    Efficiency: O(c). *)
let create h c = {
  hash = h;
  size = c;
  buckets = Array.make c []
}
