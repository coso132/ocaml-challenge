let guess5 n = if (n>5 || n<1) 
  then failwith("Cringiata di fuori")
  else 
    let r = 1+Random.int(5) in (n=r,r)
;;