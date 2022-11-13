module type Ring = sig
    type t
    val zero : t
    val one : t
    val ( + ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( ~- ) : t -> t
end 

(* module type Field = sig *)
(*     include Ring *)
(*     val ( / ) : t -> t -> t *)
(* end *)

module type INT_RING = Ring (*with type t = int*)
(* module type INT_FIELD = Field with type t = int *)

module RingInt : INT_RING = struct
    type t = int
    let zero = 0
    let one = 1
    let ( + ) = Stdlib.( + )
    let ( * ) = Stdlib.( * )
    let ( ~- ) = Stdlib.( ~- )
end

(* module FieldInt : INT_FIELD = struct *)
(*     include RingInt *)
(*     let ( / ) = Stdlib.( / ) *) 
(* end *)
