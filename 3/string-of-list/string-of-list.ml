let rec string_of_list1 = function 
  |[]-> ""
  |[h]-> (string_of_int h) 
  |h::t -> (string_of_int h) ^";"^ string_of_list1 t
;;

let string_of_list l= "[" ^ string_of_list1 l ^ "]" 
;;