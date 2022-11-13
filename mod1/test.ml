module type T = sig
    type t
    val x : t
end 

module M : T = struct
    type t = int
    let x = 42
end
