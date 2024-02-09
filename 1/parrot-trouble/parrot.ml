type option = Some of bool | None
let parrot_trouble speaks time = 
  if time > 23 || time < 0 then None else 
  if speaks=false then (Some false) else 
  if time < 7 || time >16 then Some true else Some false