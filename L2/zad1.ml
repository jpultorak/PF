let length xs =
    List.fold_left (fun acc _ -> acc+1) 0 xs;;

let rev xs = 
    List.fold_left (fun acc x -> x::acc) [] xs;;

let map f xs = 
    List.fold_right (fun x tl -> f x :: tl) xs [];;

let rev_map f xs = 
    List.fold_left (fun acc x -> f x :: acc) [] xs;;

let append xs ys = 
	List.fold_right (fun x tl -> x::tl) xs ys;;

let rev_append xs ys = 
    List.fold_left (fun acc x -> x::acc) (rev xs) ys;;

let filter f xs =
    List.fold_right (fun x tl -> if f x then x::tl else tl) xs [];;

let arr = [-1; 2; -5; 6; 0]
let neg x = 
    if x < 0 then true
    else false;;

let mul2 x = 2*x;;

