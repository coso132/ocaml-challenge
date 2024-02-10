let rec sumrange x y = 
  if x>y then 0
  else x + sumrange (x+1) y
;;