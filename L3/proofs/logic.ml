type formula = Var of string | False | Impl of formula * formula 
(* let f = Impl(Impl(Var "p", False)), Impl(Var "p", Var "q")*)
let rec string_of_formula f = match f with 
    | Var q -> q 
    | False -> "⊥"
    | Impl (f1, f2) -> match f1 with
        | Impl _ -> "(" ^ (string_of_formula f1) ^ ") → " ^ (string_of_formula f2)
        | _ ->  (string_of_formula f1) ^ " → " ^ (string_of_formula f2)

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

exception InvalidTheorem

type theorem =  
    | Assume of formula 
    | ImpI of formula list * formula 
    | ImpE of formula list * formula
    | FalseE of formula list * formula

let assumptions thm = match thm with
    | Assume x -> [x]
    | ImpI (x, _) -> x
    | ImpE (x, _) -> x
    | FalseE (x, _) -> x

let consequence thm = match thm with
    | Assume x -> x
    | ImpI (_, x) -> x
    | ImpE (_, x) -> x
    | FalseE (_, x)-> x

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f =
    Assume f

let imp_i f thm =
    let a = assumptions thm and thesis = consequence thm in 
        ImpI ((List.filter (fun x -> compare x f != 0) a), Impl(f, thesis))   

let imp_e th1 th2 =
    let a1 = assumptions th1 and thesis1 = consequence th1 
    and a2 = assumptions th2 and thesis2 = consequence th2 in match thesis1 with
        | Impl (f1, f2) -> if (compare f1 thesis2)= 0 then  ImpE ((a1@a2), f2) else raise InvalidTheorem
        | _ -> raise InvalidTheorem

let bot_e f thm = 
    let a = assumptions thm and thesis = consequence thm in
    match thesis with 
    | False -> FalseE (a, f)  
    | _ -> raise InvalidTheorem

(* theorem 1 *)
let assume_p = by_assumption (Var "p")
let theorem1 = imp_i (Var "p") assume_p
(*theorem 2*)
let t1 = imp_i (Var "q") assume_p
let theorem2 = imp_i (Var "p") t1
(*theorem 3*)
let f1 = Impl (Var "p", Impl (Var "q", Var "r"))
let f2 = Impl (Var "p", Var "q")
let t1 = by_assumption f1
let t2 = by_assumption f2
let t3 = by_assumption (Var "p")
let t4 = imp_e t2 t3
let t5 = imp_e t1 t3
let t6 = imp_e t5 t4
let t7 = imp_i (Var "p") t6
let t8 = imp_i f2 t7
let theorem3 = imp_i f1 t8
(* theorem 4 *)
let assume_false = by_assumption False
let t1 = bot_e (Var "p") assume_false
let theorem4 = imp_i False t1
