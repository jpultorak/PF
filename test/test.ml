type colour =
    | Red
    | Blue
    | Green
    | Yellow
    | RGB of int * int * int;;
type 'a option = None | Some of 'a;; 

let div a b = 
    print_endline "dividing two numbers!";
    if b = 0. then None
    else Some (a /. b)


let color = function
    | Red -> print_endline "Czerw"
    | Blue -> print_endline "B"
    | Green -> print_endline "G"
    | _ -> print_endline "Other" 
    
let rec even = function
    | 0 -> true
    | x -> odd (x-1)

and odd = function
    | 0 -> false
    | x -> even (x-1)
    




