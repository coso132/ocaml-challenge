
let dice x = if Random.int(100)<=(x-1) then 6
  else Random.int(5)+1
;;

(* cagate mie *)
let ifdice6 x =if (dice x) = 6 then 1 else 0;;

let rec testprobability f x n =
  if n>0 then f x + (testprobability f x (n-1))
  else 0
;;