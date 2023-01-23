## Downsampling cells within a matrix to meet a specific threshold?



  downsample.cells <- function(X, ds.threshold){
    X = as.matrix(X)
    cells2downsample = names(which(colSums(X) > ds.threshold))
    
  ## number of pseudocells to create for each
    m = 1

  ## generate our cells
    for(cell in cells2downsample){
     X[,cell] = as.numeric(rmvhyper(m, X[,cell], ds.threshold))
    }
    return(X)
  }
  
  
