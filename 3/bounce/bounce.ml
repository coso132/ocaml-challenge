let bounce n x= 
  let x = x mod (2*n) in
  if x < n 
    then x
    else n - (x mod n)
;;
