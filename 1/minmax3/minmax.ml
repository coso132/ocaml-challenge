let ternary b x y = if b then x else y;;
let rec find b = function
  |[]-> 0 
  |h::[]-> h
  |h::m::t -> find b ((ternary (b h m) h m)::t)
;;
let rec max lst = find ( > ) lst
let rec min lst = find ( < ) lst
;;
let minmax3 a b c = (min[a;b;c],max[a;b;c]);;