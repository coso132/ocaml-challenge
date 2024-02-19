
let rec enum_nat n (x,y) = match n with 
  |n when n<=0 -> (x,y)
  |_ -> if y <= 0  then enum_nat (n-1) (0,x+1) 
                  else enum_nat (n-1) ((x+1),(y-1))
;;
let enum_nat_nat n = enum_nat (n) (0,0);;

List.init 20 enum_nat_nat;;