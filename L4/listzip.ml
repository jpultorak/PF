type 'a zlist = LZip of  'a list * 'a list

let of_list xs = LZip([], xs) 

let to_list = function
    | LZip (xs, ys) -> List.rev xs @ ys

let elem (LZip (_, ys)) = match ys with 
    | [] -> None
    | y::ys -> Some y 

let move_left = function 
    | LZip (x::xs, ys) -> LZip (xs, x::ys)
    | LZip ([], _) -> failwith "out of bounds"

let move_right = function 
    | LZip (xs, y::ys) -> LZip (y::xs, ys)
    | LZip (_, []) -> failwith "out of bounds"

let insert x xs = match xs with
    | LZip (xs, ys) -> LZip(x::xs, ys)

let remove = function 
    | LZip (x::xs, ys) -> LZip(xs, ys)
    | LZip ([], _) -> failwith "out of bounds"
