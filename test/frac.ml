module type Fraction = sig
    type t

    val make : int -> int -> t

    val numerator : t -> int
    val denominator : t -> int
    val to_string : t -> string
    val to_float : t -> float

    val add : t -> t -> t
    val mul : t -> t -> t
end 

module Frac : Fraction = struct
    let rec gcd a b =
        if b = 0 then a
        else gcd b (a mod b)
    let normalize (x, y) = 
        let g = gcd x y in 
            if y < 0 then (-x / g, -y / g) else (x / g, y / g)

    type t = int*int
    let make x y = normalize (x, y)
    let numerator (x, y) = x
    let denominator (x, y) = y
    let to_string (x, y) = string_of_int x ^ "/" ^ string_of_int y
    let to_float (x, y) = float_of_int x /. float_of_int y
    let add (x1, y1) (x2, y2) = 
        normalize(x1*y2 + x2*y1, y1*y2)
    let mul (x1, y1) (x2, y2) = 
        normalize(x1*x2, y1*y2)
end
