let max_nat x y = 
  if x<0 || y<0 
  then failwith("Not natural")
  else if x>y then x else y
;;