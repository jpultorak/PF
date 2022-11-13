let rec merge cmp xs ys =
    match xs, ys with
    | [], _ -> ys
    | _, [] -> xs
    | x::xs, y::ys -> 
            if cmp x y then x::merge cmp xs (y::ys) else y::merge cmp (x::xs) ys;;

let rec merge2 cmp xs ys =
    
    let rec it cmp xs ys acc = 
        match xs, ys with
        | [], _ -> List.rev ys @ acc 
        | _, [] -> List.rev xs @ acc
        | x::xs, y::ys ->
                if cmp x y then it cmp xs (y::ys) (x::acc)
                else it cmp (x::xs) ys (y::acc)
    in 
        List.rev (it cmp xs ys []) ;;
let rec merge3 cmp xs ys=

    let rec it cmp acc xs ys = 
        match xs, ys with
        | [], l | l, [] -> acc @ l
        | x::xs, y::ys ->
                if cmp x y then it cmp (acc @ [x]) xs (y::ys)
                else it cmp (acc @ [y]) (x::xs) ys
    in
        it cmp [] xs ys

let rec halve xs =
    match xs with
    | [] -> [], []
    | x1::x2::xs -> let lf, rt = halve xs in x1::lf, x2::rt
    | x::xs -> [x], [];;

let rec mergesort cmp xs = 
    match xs with
    | [] -> []
    | x::[] -> [x]
    | x::xs as lst -> 
            let l1, l2 = halve lst in merge cmp (mergesort cmp l1) (mergesort cmp l2);;
let rec mergesort2 cmp xs = 
    match xs with
    | [] -> []
    | x::[] -> [x]
    | x::xs as lst -> 
            let l1, l2 = halve lst in merge2 cmp (mergesort2 cmp l1) (mergesort2 cmp l2);;

let rec mergesort3 cmp xs = 
    match xs with
    | [] -> []
    | x::[] -> [x]
    | x::xs as lst -> 
            let l1, l2 = halve lst in merge3 cmp (mergesort3 cmp l1) (mergesort cmp l2);;



let time f x y = 
    let start = Unix.gettimeofday()
    in let res = f x y
    in let stop = Unix.gettimeofday()
    in let () = Printf.printf "exec itme : %fs\n%!" (stop -.start)
    in res;;

let rec gen n acc = 
    if n == 0 then acc 
    else gen (n-1) (Random.int 1000::acc);;
let test_list  = gen 10000 [];;
