
let rec countzero f a b = 
  if a >= b then 0 else match f a with
  |0-> 1+ countzero f (a+1) b
  |_-> countzero f(a+1) b
;; 

assert (countzero (fun x -> x) (-10) 10 = 1);;

assert (countzero (fun x -> x) 1 10 = 0);;

assert (countzero (fun x -> x*x - 1) (-10) 10 = 2);;

assert (countzero (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);;