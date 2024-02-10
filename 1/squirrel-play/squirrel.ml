
type season = Spring | Summer | Autumn | Winter

let squirrel_play c = function
  |Summer -> c<=35 && c>=15
  |_ ->   c<=30 && c>=15
;;