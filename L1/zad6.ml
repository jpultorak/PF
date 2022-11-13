let ctrue a b = 
   if true then a
   else b;;
  
let cfalse a b = 
   if false then a
   else b;;
   
let cand a b = a b cfalse;;
let cor a b = a ctrue b;;

let cbool_of_bool b = 
   if b == true then ctrue
   else cfalse
   
 let bool_of_cbool b = 
    if b true false == true then true
    else false;;


let b1 = (cand ctrue cfalse) "kotki" "pieski";;
let b2 = (cand ctrue ctrue) "kotki" "pieski";;
let b3 = (cor ctrue cfalse) 1234 567;;
let b4 = (cor cfalse cfalse) 1234 567;;
