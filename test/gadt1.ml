(* type t = C1 | C2 *)
(**)
(* let z_int = function  *)
(*    *)
(*   | C1 -> 0 *)
(*   | _ -> assert false *)
(**)
(**)
(* type _ t = C1 : int t | C2 : float t *)
(**)
(**)
(* let z : type a. a t -> a = function C1 -> 0 | C2 -> 0.0 *)
(* let f : int t -> int = function C1 -> 42 *)

type _ value = 
  | Bool : bool -> bool value 
  | Int : int -> int value 


type _ expr = 
  | Value : 'a value -> 'a expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
  | Eq : 'a expr * 'a expr -> bool expr
  | Lt : 'a expr * 'a expr -> bool expr


let rec eval : type a. a expr -> a = function
  | Value (Bool b) -> b
  | Value (Int i) -> i
  | If (b, l, r) -> if eval b then eval l else eval r
  | Eq (a, b) ->  (eval a) = (eval b)
  | Lt (a, b) -> eval a < eval 
