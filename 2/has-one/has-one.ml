let trn b x y = if b then x else y;;

let rec has_number x m y = 
  if x<0 then failwith"outside of range"
  else if x = 0 then false 
  else if (x mod m) = y 
    then true 
    else has_number (x/m) m y
;;
let has_one x = has_number x 10 1;;
  
assert(has_one 10 = true);;
assert(has_one 220 = false);;
assert(has_one 911 = true);;
assert(has_one 451 = true);;
assert(try has_one (-1) |> fun _ -> false with _ -> true);;