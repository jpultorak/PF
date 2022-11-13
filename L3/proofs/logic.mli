(** reprezentacja formuł *)
type formula =  Var of string| False | Impl of formula * formula 

val pp_print_formula : Format.formatter -> formula -> unit

(** reprezentacja twierdzeń *)
type theorem =  
    | Assume of formula 
    | ImpI of formula list * formula 
    | ImpE of formula list * formula 
    | FalseE of formula list * formula 

(** założenia twierdzenia *)
val assumptions : theorem -> formula list
(** teza twierdzenia *)
val consequence : theorem -> formula

val pp_print_theorem : Format.formatter -> theorem -> unit

(** by_assumption f konstruuje następujący dowód

  -------(Ax)
  {f} ⊢ f  *)
val by_assumption : formula -> theorem

(** imp_i f thm konstruuje następujący dowód

       thm
      Γ ⊢ φ
 ---------------(→I)
 Γ \ {f} ⊢ f → φ *)
val imp_i : formula -> theorem -> theorem

(** imp_e thm1 thm2 konstruuje następujący dowód

    thm1      thm2
 Γ ⊢ φ → ψ    Δ ⊢ φ 
 ------------------(→E)
 Γ ∪ Δ ⊢ ψ *)
val imp_e : theorem -> theorem -> theorem

(** bot_e f thm konstruuje następujący dowód

   thm
  Γ ⊢ ⊥
  -----(⊥E)
  Γ ⊢ f *)
val bot_e : formula -> theorem -> theorem

val theorem1 : theorem
val theorem2 : theorem
val theorem3 : theorem
val theorem4 : theorem
