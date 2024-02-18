type student = {
  id: string;
  name: string;
  surname: string;
  vote: int option;
  laude: bool
}
let alf2023 = [
  { id="60/61/65570"; name="Ambra"; surname="Ambu"; vote=Some 21; laude=false };
  { id="61/61/65778"; name="Brunello"; surname="Brundu"; vote=Some 18; laude=false };
  { id="60/61/65624"; name="Costantino"; surname="Cossu"; vote=Some 24; laude=false };
  { id="60/61/65808"; name="Deborah"; surname="Demurtas"; vote=Some 28; laude=false };
  { id="60/61/65668"; name="Efisio"; surname="Ennas"; vote=Some 18; laude=false };
  { id="60/61/65564"; name="Felicino"; surname="Frau"; vote=None; laude=false };
  { id="60/64/20203"; name="Gavino"; surname="Girau"; vote=Some 20; laude=false };
  { id="60/61/65892"; name="heidi"; surname="hernandez"; vote=Some 8; laude=true };
  { id="60/61/65563"; name="Igino igor"; surname="Ibba"; vote=Some 15; laude=false };
  { id="60/61/64427"; name="Lillo"; surname="Lilliu"; vote=Some 25; laude=false };
  { id="60/61/65448"; name="Morgan"; surname="Murtas"; vote=Some 15; laude=false };
  { id="61/61/65213"; name="Nathan"; surname="Nieddu"; vote=Some 16; laude=false };
  { id="60/61/65832"; name="Ornella"; surname="Onnis"; vote=Some 30; laude=true };
  { id="60/61/65517"; name="Pinuccio"; surname="Puddu"; vote=Some 28; laude=false };
  { id="60/64/21222"; name="Quintilio"; surname="Quaglioni"; vote=Some 22; laude=false };
  { id="60/61/65907"; name="Rihanna"; surname="Ruzzu"; vote=Some 18; laude=false };
  { id="60/61/65766"; name="Samantah"; surname="Sulis"; vote=Some 30; laude=false };
  { id="60/61/65730"; name="Tatiana"; surname="Truzzu"; vote=Some 30; laude=true };
  { id="60/61/65738"; name="Ubaldo"; surname="Urru"; vote=None; laude=true };
  { id="60/61/65722"; name="Valentina"; surname="Vargiu"; vote=Some 30; laude=true };
  { id="60/61/65592"; name="Zlatan"; surname="Zuncheddu"; vote=Some 18; laude = false }
];;
(* let no_show = (function {vote=v; id=i} -> (v=None));; *)
let rec concatid l = match l with |[]->[] |{vote=v; id = i}::t-> i::concatid t ;;


let rec id_no_show l = 
  let lf = List.filter (function {vote=v; id=i} -> (v=None)) l in 
  concatid lf;;

let rec concatname l = match l with |[]->[] |{name=s;surname=sn}::t-> 
  (s ^ " "^sn)::concatname t ;;

let canupgrade = (function {vote=v} -> ( v<=Some 17 && v>=Some 15));;

let rec upgradeable l= 
  let lf = List.filter canupgrade l in 
  concatname lf;;

(* let rec upgrade = function
  |[]->[]
  |a::t -> if not (canupgrade a) then a::upgrade t 
    else match a with {id=i;name=n;surname=sn;vote=v;laude=l} ->
      {id=i;name=n;surname=sn;vote=Some 18;laude=l}::upgrade t
;; *)
let rec upgrade l= l
  |> List.map (fun s -> match s with 
                        |e when canupgrade e -> {e with vote = Some 18}
                        |e -> s)
;;

let wrong_laude l = l
  |> List.filter (fun s -> s.vote<Some 30 && s.laude=true)
  |> List.map (fun s -> s.name ^ " " ^ s.surname)
;;

let fix_laude l = l
  |> List.map (fun s -> match s with 
                        |s when s.vote<Some 30 && s.laude=true -> {s with laude = false}
                        |s -> s)
;;

let percent_passed l = let count= l |> List.length in
  (l |> List.filter(fun s -> s.vote>Some 17)
    |> List.length)*100/count
;;

let avg_grade l = 
  let l = l |> List.filter(fun s -> s.vote>=Some 18)
            |> List.map(fun s -> match s.vote with | Some x -> if s.laude then x+2 else x|_ -> 0)in
  (float_of_int )(List.fold_left ( + ) 0 l )/. (float_of_int) (List.length l)
;;