open Logic

type proof_1 = 
    | Goal of (string*formula) list * formula 
    | CompleteProof of theorem
    | ImpE1 of formula list * formula * proof_1 * proof_1
    | ImpI1 of formula list * formula * proof_1
    | FalseE1 of formula list * formula * proof_1

type context = 
    | Top
    | ImpEL of formula list * formula * context * proof_1
    | ImpER of formula list * formula * proof_1 * context
    | FalseC of formula list * formula * context
    (* | GoalC of named_assum * th * context *)
    | ImpIC of formula list * formula * context

type proof =
    | Complete of theorem
    | Incomplete of context*proof_1

let proof g f =
    Incomplete (Top, Goal (g, f))

let goal = function
    | Complete _ -> None
    | Incomplete (ctx, t) -> 
            match t with
            | Goal (g, f) -> Some (g, f) 
            | _ -> failwith ""

let qed = function
    | Complete th -> th
    | _ -> failwith "Incomplete Proof"

let go_up pr = match pr with
    | Complete _ -> failwith ""
    | Incomplete (ctx, t) ->
            match ctx with
            | Top -> failwith "Top"
            | ImpEL (assm, th, ctx, r) -> Incomplete(ctx, ImpE1 (assm, th, t, r))
            | ImpER (assm, th, l, ctx) -> Incomplete(ctx, ImpE1 (assm, th, l, t))
            | FalseC (assm, th, ctx) -> Incomplete(ctx, FalseE1 (assm, th, t))
            | ImpIC (assm, th, ctx) -> Incomplete(ctx, ImpI1(assm, th, t)) 

let go_left pr = match pr with 
    | Complete _ -> failwith ""
    | Incomplete (ctx, t) ->
            match t with
            | Goal _ | CompleteProof _ -> failwith "Leaf" 
            | ImpE1 (assum, th, l, r) -> Incomplete (ImpEL (assum, th, ctx, r), l)
            | ImpI1 (assum, th, t) -> Incomplete (ImpIC (assum, th, ctx), t)
            | FalseE1 (assum, th, t) -> Incomplete (FalseC (assum, th, ctx), t)

let go_right pr = match pr with 
    | Complete _ -> failwith ""
    | Incomplete (ctx, t) ->
            match t with
            | Goal _ | CompleteProof _ -> failwith "Leaf" 
            | ImpI1 _ | FalseE1 _ -> failwith "left"
            | ImpE1 (assum, th, l, r) -> Incomplete (ImpER (assum, th, l, ctx), r)

type move = UpL | UpR | Left | Right | First
let rec search pf last_move =
    match pf with 
    | Complete _ -> failwith ""
    | Incomplete (ctx, t) ->
            match t with
            | Goal (named_assums, th) -> 
                    (match last_move, ctx with 
                    | First, Top -> pf
                    | First, _ -> search (go_up pf) UpR
                    | _, _ -> pf)

            | CompleteProof _ -> 
                    (match ctx with
                    | Top -> failwith "complete proof"
                    | ImpEL _ -> search (go_up pf) UpL 
                    | ImpER _ | FalseC _ | ImpIC _ -> search (go_up pf) UpR)

            | _ -> match last_move with
                | UpL -> search (go_right pf) Right
                | UpR -> (match ctx with
                    | Top -> search (go_left pf) Left
                    | ImpEL _ -> search (go_up pf) UpL 
                    | ImpER _ | FalseC _ | ImpIC _ -> search (go_up pf) UpR)
                | Left | Right | First -> search (go_left pf) Left

let next pf = search pf First 
    
let intro name pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply f pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply_thm thm pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply_assm name pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let pp_print_proof fmtr pf =
  match goal pf with
  | None -> Format.pp_print_string fmtr "No more subgoals"
  | Some(g, f) ->
    Format.pp_open_vbox fmtr (-100);
    g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
    Format.pp_print_cut fmtr ();
    Format.pp_print_string fmtr (String.make 40 '=');
    Format.pp_print_cut fmtr ();
    pp_print_formula fmtr f;
    Format.pp_close_box fmtr ()
