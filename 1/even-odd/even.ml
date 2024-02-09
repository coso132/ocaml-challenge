let is_even x = ( x mod 2 )==0;;

let win a b = 
  let aw = (a>=1 && a<=5) in 
    let bw = (b>=1 && b<=5) in
      match (aw,bw) with
      |(true,false)-> 1
      |(false,true)->(-1)
      |(false,false) -> 0
      |(true,true) -> if (is_even (a+b)) then 1 else -1 
;;

(* true (0) 
assert(win(0)(6)=0);;

(* false (1) *) 
assert(win(((2*0)+(1*3)) mod 7)(((0+6)-(6*4)) mod 7)=0);;

(* false (-1) *)
assert(win(win (win ((0*3) mod 6)((2+5)  mod 6) mod 6)(((1+4)-win (1 mod 6)(6 mod 6)) mod 6) mod 7)(win (((0*5)+(5+0)) mod 6)(((2-0)+win (2 mod 6)(6  mod 6)) mod 6) mod 7)=1);;

(* true (-1) *)
assert(win((((win (0 mod 6)(3 mod 6)+(1+4))+((5+0)+win (1 mod 6)(6 mod 6)))*(((6+4)+(1-6))*((2+5)-(5+0)))) mod 7)(((((2*2)+(3+4))+(win (4 mod 6)(3 mod 6)+(1+2)))-win (((2-3)*win (3 mod 6)(4 mod 6)) mod 6)(((2-4)-win (5 mod 6)(2 mod 6)) mod 6)) mod 7) = -1);;

(* true (1) *)
assert(win(((((4*3)*win (3 mod 6)(1 mod 6))+win ((3-5) mod 6)(win (4 mod 6)(1 mod 6) mod 6))-((win (0 mod 6)(3 mod 6)-(4-0))-((6-6)-win (5 mod 6)(5 mod 6)))) mod 7)((win ((win (5 mod 6)(6 mod 6)*(2-6)) mod 6)(((2*4)+win (6 mod 6)(5 mod 6)) mod 6)*(win ((1-0) mod 6)((6-5) mod 6)+((3+4)-(0*5)))) mod 7)=1);;

(* true (0) *)
assert(win((win (((1-3)*(4*4)))(((1-2)*(2+2)))-(((5*4)-(4*3))-win ((6+3))((6*3)))) mod 6)((((win (2)(1)+(3-3))+win ((5*3))((3-1)))+(((3*3)-(3*1))*(win (2)(4)-(5+4)))) mod 6)=0);;
*)