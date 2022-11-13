
let id_int x = x + 0;;

let comp1 g h x = g(h(x));;
let comp2 g h = fun x -> g(h(x))
let f1 x y  = x;;

let f2 a b = 
if true then a
else b;;

(*zadanie 2*)
let rec f3 a = f3 a;;
