let zero f  = fun x -> x;;
let succ fn f = fun x ->  f(fn(x));;
let add1 x = x+1;;
