module type X  = sig
    val x : int
end

module type Add = sig
    val add : int -> int
end 

module INC (M : X) = struct
    let x = M.x + 1
end

module AddX (M : X) = struct
    let add y = M.x + y 
end


module Zero = struct
    let x = 0
end

module Two =  INC (INC (Zero))
module Add2 = AddX (Two)
(* type annotation for functors *)
module CheckAddX : X -> Add = AddX

module type T = sig
    type t
    val x : t
end 
module Pair1 (M : T) = struct
    let p = (M.x, 1)
end 

module Pair2137 = Pair1 (struct type t = int let x = 2137 end)
module Pairchar = Pair1 (struct type t = char let x = 'b' end)
