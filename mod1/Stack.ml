module type STACK = sig
    type 'a t
    exception Empty
    val empty : 'a t
    val is_empty : 'a t -> bool
    val push : 'a -> 'a t -> 'a t
    val peek : 'a t -> 'a
    val pop : 'a t -> 'a t
    val to_list : 'a t -> 'a list
end

module ListStack : STACK = struct
    type 'a t = 'a list
    let empty = []
    let is_empty s =
        if s == empty then true
        else false
    let push x s = x :: s
    
    exception Empty
    
    let peek = function
        | [] -> raise Empty
        | x :: _ -> x

    let pop = function 
        | [] -> raise Empty
        | _ :: xs -> xs
    let to_list = Fun.id
end

(* module ListStackCached : STACK = struct *)
(*     type 'a t = 'a list * int *) 
(*     exception Empty *)

(*     let empty = ([], 0) *)

(*     let is_empty s = *) 
(*         if s == empty then true *)
(*         else false *)

(*     let push x (xs, size) = (x::xs, size + 1) *)

(*     let peek = function *)
(*         | [], _ -> raise Empty *)
(*         | x::xs, _ -> x *)

(*     let pop = function *)
(*         | [], _ -> raise Empty *)
(*         | x::xs, n -> (xs, n - 1) *)
(*     let to_list = Fun.id *)
(* end *)


