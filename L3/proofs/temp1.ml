type formula = Var of string | False | Impl of formula * formula 

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
    | Assume of formula list 
    | ImpI of theorem * formula list * formula 
    | ImpE of theorem * formula list * formula * theorem 
    | FalseE of theorem * formula list * formula 

let assumptions thm = match thm with
    | Assume x -> x
    | ImpI (_, x, _) -> x
    | ImpE (_, x, _, _) -> x
    | FalseE (_, x, _) -> x

let consequence thm = match thm with
    | Assume (x::[]) -> x
    | ImpI (_, _, x) -> x
    | ImpE (_, _, x, _) -> x
    | FalseE (_, _, x)-> x

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
    Assume [f]

let imp_i f thm =
    let a = assumptions thm and thesis = consequence thm in 
        ImpI (thm, (List.filter (fun x -> compare x f != 0) a), Impl(f, thesis))   

let imp_e th1 th2 =
    let a1 = assumptions th1 and thesis1 = consequence th1 
    and a2 = assumptions th2 and thesis2 = consequence th2 in match thesis1 with
        | Impl (f1, f2) -> ImpE (th1, (a1@a2), f2, th2)
        | _ -> raise InvalidTheorem

let bot_e f thm = 
    let a = assumptions thm and thesis = consequence thm in
    match thesis with 
    | False -> FalseE (thm, a, f)  
    | _ -> raise InvalidTheorem

(* theorem 1 *)
let assume_p = by_assumption (Var "p")
let theorem1 = imp_i (Var "p") assume_p
(* theorem 4 *)
let assume_false = by_assumption False
let t1 = bot_e (Var "p") assume_false
let theorem4 = imp_i False t1
