let hd s = s 0;;

(*let tl s x = s (x+1);;*)
let tl s = fun x -> s (x+1);;
      
let array_of_stream ?(n = 20) ?(start = 0) s =
   let rec foo s n cnt =
      if n > 0 then (s cnt)::(foo s (n-1) (cnt+1))
      else []
   in
      foo s n start ;;
      
(* zad 5 *)
let rec tabulate1 ?(l = 0) r s = 
   if l > r then []
   else (s l)::(tabulate1 ~l:(l+1) r s)

let tabulate ?(l = 0) r s =
   array_of_stream ~n:(r-l+1) ~start:l s;;
   
let map f s = fun x -> f (s x);;
let map2 f s1 s2 = fun x -> f (s1 x) (s2 x);;

let replace n a s  = fun x ->
   if x == n then a
   else s x;; 
  
let take_every n s = fun x -> s (n*x);;
(*let take_every n s x = s (n*x);;*)

let mul2 x = 2*x;;
let nats x = x+0;;

let nats2 = map mul2 nats;;
let squares = map2 ( * ) nats nats;; 
let nats_replaced = replace 3 42 nats;;

(* zad 4 *)
let rec scan f a s x = 
  if x == 0 then f a (s 0)
  else f (scan f a s (x-1)) (s x);;
