let rec pref = function
    | [] -> [[]]
    | x::xs -> []::List.map (fun ls -> x::ls) (pref xs);;

let rec suff = function
    | [] -> [[]]
    | x::xs -> (x::xs)::(suff xs);; 

let a = [1; 2; 3; 4];;
