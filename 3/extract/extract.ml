let rec extract l i = match l with
  |[] -> failwith"out of bounds"
  |h::t -> if i=0 then (h,t) 
                  else match extract t (i-1) with
                        |(e,l')-> (e,h::l')
;;

