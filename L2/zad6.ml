let rec insert x xs =
    match xs with 
    | [] -> [[x]]
    | hd::tl -> 
           (x::xs) :: List.map (fun l -> hd::l) (insert x tl);;

let rec perm = function 
    | [] -> [[]]
    | x::xs ->
            List.flatten(List.map (insert x) (perm xs));;


