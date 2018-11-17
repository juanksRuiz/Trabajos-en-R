

#punto 3
Sn <-  function(n){
  p <- runif(2,-1/2,1/2)
  y <- sqrt(1/4-(p[1]^2))
  if((p[2] <= y) & (p[2] >=-y)){
    k=1
  }
  else{
    k=0
  }
  message("k=")
  message(k)
  
  s <- k
  if(n == 1){
    return(s)
  }
  else{
    for(i in 2:n){
      p <- runif(2,-1/2,1/2)
      y <- sqrt(1/4-p[1]^2)
      if((p[2]<=y) & (p[2]>=-y)){
        k=1
      }
      else{
        k=0
      }
      message("k=")
      message(k)
      s <- s*(i-1/i) + k/(i)
      
    }
    return(s)
  }
}
message(Sn(3))
