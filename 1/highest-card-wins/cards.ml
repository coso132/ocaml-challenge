type card = Joker | Val of int;;
let range x b t = (x<=t && x>=b);;
let win d p = match (d,p) with
  |(Joker,Joker)|(Joker, Val _ ) -> false
  |(Val _, Joker ) -> true
  |(Val x, Val y) -> if x>y then false else true
;; 