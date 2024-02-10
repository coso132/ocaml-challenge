type winner = Player | Computer | Tie ;;

let win (hp,gp)= 
  let hc = Random.int(6) in
  let gc = hc +Random.int(6) in
  let sh = hc+hp in 
  ((hc,gc),
    match (gp=sh,gc=sh) with
    |(true,false) -> Player
    |(false,true) -> Computer
    |(_,_) -> Tie  )
;;