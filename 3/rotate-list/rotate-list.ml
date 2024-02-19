let rec rotate n l = match n with
  |0 -> l 
  |_ -> match l with 
        |[]->[] 
        | h::t -> rotate (n-1) (t@[h])
;;