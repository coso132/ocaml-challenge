type bitstring = E | Z of bitstring | U of bitstring;;

let rec string_of_bitstring  = function
  |E -> ""
  |Z e -> "0" ^ string_of_bitstring e  
  |U e -> "1" ^ string_of_bitstring e
;;

let rec len = function
  |E -> 0
  |Z e|U e -> 1+ len e
;;

let rec countZ = function
  |E -> 0
  |Z e  -> 1+ countZ e
  |U e -> countZ e
;;
let rec countU = function
  |E -> 0
  |U e  -> 1+ countZ e
  |Z e -> countZ e
;;

let rec concat bs1 bs2 = match bs1 with
  |E -> bs2
  |U e -> U(concat e bs2)
  |Z e -> Z(concat e bs2)
;;

let rec equals bs1 bs2 = match (bs1,bs2) with
  |(E,E) -> true
  |(Z e, Z e1)|(U e, U e1) -> equals e e1
  | _ -> false
;;

let tl = function |E -> E |U e|Z e -> e;;

let rec prefix s1 s2 = match (s1,s2) with 
  |(E,_) -> true
  |(Z e1,Z e2) |(U e1,U e2) -> prefix e1 e2
  | _ -> false
;;

let rec substring1 s1 s2= match s2 with
  |E-> false
  |Z e|U e -> if prefix s1 s2 then true else substring1 s1 e
;;