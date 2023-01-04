

# ) -------goldbach----------------------------


es.primo <- function(n){
  can=0
  for(i in 0:n){ 
    if(n %% (i+1) !=0){ 
      next
    }
    else{ 
      can=can+1
    }
  }
  if(can == 2){
    TRUE
  }else{
    FALSE
  }
}


goldbach <- function(n){
  if(n<3){
    return("El número debe ser mayor a 3")
  }
  if(n %% 2 != 0){
    return("El número debe ser par")
  }
  for(i in 0:n){
    if(es.primo(i)){
      for(j in 0:n){ 
        if(es.primo(j)){
          if(i+j==n){
            return(paste(i, j))
          }else{
            next
          }
        }
      }
    }
    next
  }
}
goldbach(128)
goldbach(31) 
goldbach(56)

# ) -------Collatz----------------------------

collatz <- function (n) {
  
  if (n == 1){
    return (0)
  } 
  if (n%%2==0) {
    1 + collatz(n/2)
  }else{
    1 + collatz(3*n +1)
  }
}
collatz(16)
collatz(12)
collatz(31)

