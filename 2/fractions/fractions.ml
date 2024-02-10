let xor a b = (not(a) && b )|| (a && not(b));;
let abs a = if a<0 then -a else a;;
let ternary b x y = if b then x else y;;
let is_posfrac (a,b) = 
  if a=0 || b=0 then failwith("cringe")
  else not (xor (a<0) (b<0))
;;

let adapt_frac (a,b)= 
  if is_posfrac(a,b) then (abs a, abs b)
  else if b<0 then ((-a),abs b) else (a,b)
;;

let compare_frac (a,b) (c,d) =
  let (a,b) = adapt_frac (a, b) in 
  let (c,d) = adapt_frac (c, d) in
    match a*d = c*b with 
    | true -> 0
    | false -> ternary (a*d > c*b) 1 (-1)
;;

let compare_posfrac (a,b) (c,d)=
  if is_posfrac (a,b) && is_posfrac (c,d)  
  then compare_frac (a,b) (c,d)
  else failwith("Frazione Negativa")
;;

assert (compare_frac (-1,2) (-2,4) == 0);;
assert (compare_frac (1,2) (1,3) == 1);;
assert (compare_frac (-1,2) (1,3) == -1);;
assert (compare_frac (1,2) (-1,3) == 1);;
assert (compare_frac (-1,2) (-1,3) == -1);;