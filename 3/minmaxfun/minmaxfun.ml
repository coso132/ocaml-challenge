let mod2 x y = y mod x;;
 
let rec minmaxfun f a b = 
  if b<a then None else
  let fa = f a in 
  match minmaxfun f (a+1) b with
  |None -> (Some (fa , fa))
  |Some (x, y) -> let newx = if fa < x then fa else x in
                  let newy = if fa>y then fa else y in
                  Some (newx ,newy)
;;
