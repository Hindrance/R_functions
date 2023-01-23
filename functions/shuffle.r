# Basic randomiser / shuffler for vectors



  shuffle = function(x){
  
    return(x[order(runif(length(x),0,1))])
  
  }

