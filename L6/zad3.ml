exception Done

let for_all1 pred xs = List.fold_left (fun acc x -> print_endline "xd"; pred x && acc) true xs

let for_all pred xs = match 
    List.fold_left (fun acc x -> print_endline "xd"; if pred x then true else raise Done) true xs with
    | exception Done -> false
    | _ -> true

let mult_list xs = match 
    List.fold_left (fun acc x -> print_endline "xd"; if x <> 0 then x*acc else raise Done) 1 xs with
    | exception Done -> 0
    | x -> x

let sorted1 xs = match List.fold_left 
    (fun acc x -> print_endline "xd";
        match acc with 
        | (true, _) -> (false, x)
        | (_, y) -> if y < x then (false, x) else raise Done) 
    (true, 0) xs 
        with 
    | exception Done -> false
    | _ -> true

let sorted xs = match 
    List.fold_left (fun acc x -> print_endline "xd"; if x <> 0 then x*acc else raise Done) 1 xs with
    | exception Done -> 0
    | x -> x

let tab1 = List.init 5 (fun x -> x+1)
let tab2 = [1; 0; 1; 1; 2; 2; 2; 2; 2]
let tab3 = [1; 2; 3; 4; 2; 5; 6; 7; 1000]
let pred = (fun x -> x > 3)

(* zadanie 4 *) 
let rec fold_left_cps f acc xs k = match xs with  
    | [] -> k acc
    | x :: xs -> f acc x (fun res -> (fold_left_cps f res xs k))

let mul_opt l =
  fold_left_cps
    (fun x y k ->   (* f_cps *)
       if y = 0
       then None     (* Terminate fold!    *)
       else k(x * y) (* Compute & continue *)
    )
    1
    l
    (fun x -> Some x)

let fold_left f acc xs =
    fold_left_cps (fun acc x k -> k (f acc x) ) acc xs Fun.id

let mult_list_cps xs = 
    fold_left_cps (fun acc x k -> if x = 0 then 0 else k(acc * x)) 1 xs Fun.id

let for_all_cps pred xs =
    fold_left_cps (fun acc x k -> if not (pred x) then false else (k acc)) true xs Fun.id  

let sorted_cps xs = match  
    fold_left_cps (fun acc x k -> match acc with 
        | (true,_ ,_) -> k (false, true, x)
        | (_,_, y) -> if y < x then (k  (false, true, x)) else (false, false, x) 
    )
    (true, true, 0) xs Fun.id with
    (_, b, _) -> b
