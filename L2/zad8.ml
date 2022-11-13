module PrioQueue = struct
    type 'a lheap = 
        | Leaf
        | Node of 'a lheap * 'a * 'a lheap * int;;

    let dist = function
        | Leaf -> 0
        | Node (l, e, r, d) -> d;;

    let rec merge t1 t2 = 
        match t1, t2 with
        | Leaf, t | t, Leaf -> t
        | Node (l1, e1, r1, d1), Node (l2, e2, r2, d2) ->
                if e1 > e2 then merge t2 t1
                else 
                    let r = merge r1 t2 in
                    let dist_r = dist r and dist_l = dist l1 in
                    if dist_r > dist_l then Node (r, e1, l1, dist_l+1)
                    else Node (l1, e1, r, dist_r+1);;

    let init_tree x = Node(Leaf, x, Leaf, 1);;

    let insert t x = merge t (init_tree x);;

    let delete_min = function
        | Leaf -> raise (Failure "empty heap")
        | Node (l, e, r, _) -> merge l r;;


    let get_min = function
        | Leaf -> raise (Failure "empty heap")
        | Node (_, e, _, _) -> e;;
end;;

