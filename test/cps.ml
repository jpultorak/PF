(* styl zwykły *)
let f x = x + 2
let g x = 5 * (f x)
(* styl kontynuacyjny *)
let f1 x k = k (x + 2)
let g1 x = f1 x (fun x -> 5*x)
let f2 x = f1 x Fun.id

let rec app xs ys = match xs with
  | [] -> ys 
  | x::xs -> x :: app xs ys

let app_cps xs ys =
  let rec app xs k = match xs with
    | [] -> k ys
    | x::xs -> app xs (fun r -> k (x::r))
  in
    app xs Fun.id

let rec mult_list xs = match xs with
  | [] -> 1
  | x::xs -> x * (mult_list xs)

let mult_list_cps xs =
  let rec mult_list xs k = match xs with
    | [] -> k 1
    | 0 :: xs -> 0
    | x :: xs -> mult_list xs (fun res -> k (x*res))
  in
    mult_list xs Fun.id

let sorted_cps xs = 
  let rec sorted xs k = match xs with
    | [] | [_] -> k true
    | x0::x1::xs -> if x0 > x1 then false else sorted xs (fun res -> k res)
  in 
    sorted xs Fun.id

let rec fold_right (f : 'a -> 'b -> 'b) (xs : 'a list) (init : 'b) = match xs with 
    | [] -> init 
    | x :: xs -> f x (fold_right f xs init)

let rec fold_left (f : 'a -> 'b -> 'a) (acc : 'a) (xs : 'b list) = match xs with
    | [] -> acc
    | x :: xs -> fold_left f (f acc x) xs 

let fold_right_cps (f : 'a -> 'b -> 'b) (xs : 'a list) (init : 'b) =
    let rec fold_right xs k = match xs with
        | [] -> k init
        | x :: xs -> fold_right xs (fun res -> k (f x res))
    in
        fold_right xs Fun.id

let rec fold_left_cps f_cps acc xs k = match xs with  
    | [] -> k acc
    | x :: xs -> f_cps acc x (fun res -> (fold_left_cps f_cps res xs k))

let fold_left_cps1 f acc xs = fold_left_cps
(* let fold_left_cps1 f acc xs = fold_left (fun a b -> f a b) acc xs  *)
