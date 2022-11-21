module type OrderedType = sig
    type t
    val compare : t -> t -> int
end 

module type S = sig
    type key
    type t
    val id : t
    val apply : t -> key -> key
    val invert : t -> t
    val swap : key -> key -> t
    val compose : t -> t -> t
    val compare : t -> t -> int
    val list_of : t -> (key * key ) list
end

module type S1 = sig
    include S
    val is_generated : t -> t list-> bool
end


module Make (Key : OrderedType) : S with type key = Key.t
module Make1 (Perm : S) : S1 with type key = Perm.key 

