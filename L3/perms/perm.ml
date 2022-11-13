module type OrderedType = sig
    type t
    val compare : t -> t -> int
end 

module type S = sig
    type key
    type t
    val id : t
    val apply : t -> key -> key
    val invert : t -> t
    val swap : key -> key -> t
    val compose : t -> t -> t
    val compare : t -> t -> int
    val list_of : t -> (key * key ) list
end

module type S1 = sig
    include S
    val is_generated : t -> t list-> bool
end

module Make (Key : OrderedType) = struct
    module MapT = Map.Make(Key)

    type key = Key.t

    type t = {f : Key.t MapT.t; finv : Key.t MapT.t}

    let id = {f = MapT.empty; finv = MapT.empty}

    let apply perm x = match (MapT.find_opt x perm.f) with
        | Some x -> x
        | None -> x

    let invert perm = {f = perm.finv; finv = perm.f}

    let swap x y = let open MapT in
        {f = empty |> add x y |> add y x; 
        finv  = empty |> add y x |> add x y}

    let comp bi key a b = match a, b with
        | None, None -> None
        | Some x, None -> Some x 
        | _, Some x -> 
            match (MapT.find_opt x bi) with 
            | None -> Some x
            | Some x -> Some x

    let compose p1 p2 = 
        {f = MapT.merge (comp p1.f) p1.f p2.f; 
        finv = MapT.merge (comp p2.finv) p2.finv p1.finv}

    let compare p1 p2 = 
        MapT.compare (Key.compare) p1.f p2.f 

    let list_of p1 =
        MapT.bindings p1.f;;  
end

module Make2 (Perm : S) = struct
    include Perm

    module SetP = Set.Make(Perm)

    let is_generated p g = 
        let rec saturate sn = 
            let s1 = SetP.fold (fun perm set -> SetP.add perm (SetP.add (invert perm) set)) sn SetP.empty 
            and s2 = SetP.fold 
                    (fun perm1 set -> SetP.fold (fun perm2 set -> SetP.add (compose perm1 perm2) set) sn SetP.empty) 
                    sn 
                    SetP.empty 
            in
                let sn1 = SetP.union s1 s2 in
                    match SetP.find_opt p sn1 with
                    | Some x -> true
                    | None -> if SetP.equal sn sn1 then false else saturate sn1
            in
                saturate (SetP.add Perm.id (SetP.of_list g))           
end
