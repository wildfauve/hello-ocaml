(* Dont worry about the algebra of Rings and Fields *)
module type Ring = sig
  type t
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ~- ) : t -> t   (* uniary negation *)

  val string : t -> string

end

module type Field = sig
  include Ring
  val ( / ) : t -> t -> t
end

module IntRingImpl = struct
  type t = int
  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let string = string_of_int
end

module IntFieldImpl = struct
  include IntRingImpl
  let ( / ) = Stdlib.( / )
end

module FloatRingImpl = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = Stdlib.( +. )
  let ( * ) = Stdlib.( *. )
  let ( ~- ) = Stdlib.( ~-. )
  let string = string_of_float
end

module FloatFieldImpl = struct
  include FloatRingImpl  (* we want to include the impl not the sealed interface as it will miss the type t*)
  (* include adds the definitions from FloatRingImpl and makes those available externally. *)
  let ( / ) = Stdlib.( /. )
end


module IntRing : Ring = IntRingImpl  (* remember this "seals" the interface of IntRingImpl to the sig Ring *)
module IntField : Ring = IntFieldImpl
module FloatRing : Ring = FloatRingImpl
