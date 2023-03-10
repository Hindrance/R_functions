## This file is the revised version of the original 'R_funcitons.R' of the original
## submission.




## Pairwise robust covariance
cormad <- function(x,y) {

    cost   <- 1.4826
    sqrt2  <- sqrt(2)

    med_x    <- median(x)
    med_y    <- median(y)

    mad_x <- cost * median( abs(  x - med_x ) )
    mad_y <- cost * median( abs(  y - med_y ) )

    zx  <-  { x - med_x} / { sqrt2 * mad_x }   ## !!
    zy  <-  { y - med_y} / { sqrt2 * mad_y }   ## !!

    U   <-  zx + zy
    V   <-  zx - zy

    mad_U2 <- { cost * median( abs( U - median(U) )  ) }^2
    mad_V2 <- { cost * median( abs( V - median(V) )  ) }^2

    return( { mad_U2 - mad_V2 }  /  {mad_U2  + mad_V2} )

}




## Vectorized upper triangle of the robust correlation matrix
cormad.vec <- function(data){

    p     <- ncol(data)
    idx   <- which(upper.tri(matrix(0L, ncol=p, nrow=p ), diag=FALSE), arr.ind=TRUE)
    ncors <- { p * {p-1} / 2 }
    ans   <- rep(0, times = ncors )

    ## Constants
    cost   <- 1.4826
    sqrt2  <- sqrt(2)

    ## Compute principal variables
    for (k in 1:p){
        m    <- median(data[,k] )
        s    <- cost * median( abs( data[,k] -   m ) )
        data[,k] <- { data[,k] - m} / { sqrt2 * s }
    }

    ## Compute pairwise correlations
    for (k in 1:ncors){
        i      <- idx[k,1]  ## row index of the upper triangle
        j      <- idx[k,2]  ## col index of the upper triangle
        U      <- data[,i] + data[,j]
        V      <- data[,i] - data[,j]
        mad_U2 <- { cost * median( abs( U - median(U) )  ) }^2
        mad_V2 <- { cost * median( abs( V - median(V) )  ) }^2
        ans[k] <- { mad_U2 - mad_V2 }  /  {mad_U2  + mad_V2}
    }

    return(ans)
}




## Cross-validation for Rmad
cv.Rmad <- function(data, nsplits=10){
  n <- nrow(data)
  p <- ncol(data)
  nc       <- p * {p-1} / 2
  tgrid    <- seq(0.005, 0.995, by=0.05)
  ngrid    <- length(tgrid)
  n1       <- n - floor(n/log(n))
  n2       <- n - n1

  ## Estimate expected loss
  C1 <- C2 <- rep(0, times=nc)
  FLOSSES  <- matrix(0, nrow=nsplits, ncol=ngrid)

  for (i in 1:nsplits){
    message("Split: ", i, " (of ", nsplits, ")")
    idx <- sample(1:n, size=n1, replace=FALSE)
    C1 <- cormad.vec(  data[ idx,  ]  )
    C2 <- cormad.vec(  data[-idx,  ]  )
    for (k in 1:ngrid){
      C1[  abs(C1) <  tgrid[k]  ] <- 0
      FLOSSES[i,k] <- sum( 2 * { C1 - C2}^2 )
    }
  }

  avgLoss <- colMeans(FLOSSES)
  hstar   <- tgrid[which.min(avgLoss)]

  ans <- list()
  ans$avgloss   <- avgLoss
  ans$threshold <- hstar

  return(ans)
}






################################################################################
## Software for the TBW of doi:10.1186/1471-2105-8-220
## This is a re-wrapping of the original software in the supplementary files
## which is 'R_functions_hardin.R'. Note that the original software has been
## slightly modified to manage non convergent cases
################################################################################


## Computes the TBW correlation for variables x,y with breakdown point equal to
## r = 0.2. Initial are given as median/mad
## The function may return NA if there are convergence problems, see comments in
## 'R_functions_hardin'
TBW <- function(x, y, r=0.2){
   data <- cbind(x,y)
   n1   <- nrow(data)
   c1   <- rejpt.bw(p=2, r)[1]
   b0   <- erho.bw(p=2, c1)[1]

   init        <-  list()
   init$center <-  apply( data, 2 , median, na.rm=TRUE )
   init$cov    <-  diag ( apply( data, 2 , mad, na.rm=TRUE)^2 )   ## !! Questa linea ?? stat modificata !! ##
   ans <- NA
   try({
      u   <- biwt.est(data, n1,  p=2, r, c1, b0, init)
      ans <- u$biwt.sig[1,2] / sqrt( u$biwt.sig[1,1] * u$biwt.sig[2,2] )
      if(!is.finite(ans)){
         ans <- NA
      }
   }, silent=TRUE)

   return(ans)
}




## Vectorized upper triangle of the correlation matrix obtained with TBW
## the breakdown point is set to 0.2 as default (see Harding et al.)
TBW.vec <- function(data, r=0.2){
   p     <- ncol(data)
   idx   <- which(upper.tri(matrix(0L, ncol=p, nrow=p ), diag=FALSE), arr.ind=TRUE)
   ncors <- { p * {p-1} / 2 }
   ans   <- rep(0, times = ncors )

   ## !! NOTE: heavy for loop to be parallelized
   ## Compute pairwise TBW
   for (k in 1:ncors){
      i      <- idx[k,1]  ## row index of the upper triangle
      j      <- idx[k,2]  ## col index of the upper triangle
      ans[k] <- TBW(x=data[,i], y=data[,j], r=r)
   }

   return(ans)
}




## Cross-validation for TBW
cv.TBW <- function(data, nsplits=10){
   n <- nrow(data)
   p <- ncol(data)
   nc       <- p * {p-1} / 2
   tgrid    <- seq(0.005, 0.995, by=0.05)
   ngrid    <- length(tgrid)
   n1       <- n - floor(n/log(n))
   n2       <- n - n1

   ## Estimate expected loss
   C1 <- C2 <- rep(0, times=nc)
   FLOSSES  <- matrix(0, nrow=nsplits, ncol=ngrid)

   for (i in 1:nsplits){
      message("Split: ", i, " (of ", nsplits, ")")
      idx <- sample(1:n, size=n1, replace=FALSE)
      C1 <- TBW.vec(  data[ idx,  ]  )
      C2 <- TBW.vec(  data[-idx,  ]  )
      for (k in 1:ngrid){
         C1[  abs(C1) <  tgrid[k]  ] <- 0
         FLOSSES[i,k] <- sum( 2 * { C1 - C2}^2 )
      }
   }



   avgLoss <- colMeans(FLOSSES)
   hstar   <- tgrid[which.min(avgLoss)]


   ans           <- list() ## !! Questa linea ?? stat modificata !! ##
   ans$avgloss   <- avgLoss
   ans$threshold <- hstar

   return(ans)
}










################################################################################
## Software for cross-validation of Rn, Rho, Tau
################################################################################

## Cross-validation for sample correlation
cv.Rn <- function(data, nsplits=10, monitor=1){
   n <- nrow(data)
   p <- ncol(data)
   tgrid    <- seq(0.005, 0.995, by=0.05)
   ngrid    <- length(tgrid)
   n1       <- n - floor(n/log(n))
   n2       <- n - n1

   ## Estimate expected loss
   C1 <- C2 <- matrix(0, nrow=p, ncol=p)
   FLOSSES  <- matrix(0, nrow=nsplits, ncol=ngrid)

   for (i in 1:nsplits){
      if (monitor>=1) { message("Split: ", i, " (of ", nsplits, ")") }
      idx <- sample(1:n, size=n1, replace=FALSE)
      C1 <- cor(  data[ idx,  ]  )
      C2 <- cor(  data[-idx,  ]  )
      for (k in 1:ngrid){
         C1[  abs(C1) <  tgrid[k]  ] <- 0
         diag(C1) <- rep(1,p)
         FLOSSES[i,k] <- norm(C1-C2, "F")
      }
   }

   avgLoss <- colMeans(FLOSSES)
   hstar   <- tgrid[which.min(avgLoss)]

   ans <- list()
   ans$avgloss   <- avgLoss
   ans$threshold <- hstar

   return(ans)
}





## Cross-validation for Kendall Tau
cv.Tau <- function(data, nsplits=10, monitor=1){
   n <- nrow(data)
   p <- ncol(data)
   tgrid    <- seq(0.005, 0.995, by=0.05)
   ngrid    <- length(tgrid)
   n1       <- n - floor(n/log(n))
   n2       <- n - n1

   ## Estimate expected loss
   C1 <- C2 <- matrix(0, nrow=p, ncol=p)
   FLOSSES  <- matrix(0, nrow=nsplits, ncol=ngrid)

   for (i in 1:nsplits){
      if (monitor>=1) { message("Split: ", i, " (of ", nsplits, ")") }
      idx <- sample(1:n, size=n1, replace=FALSE)
      C1 <- cor(  data[ idx,  ]  , method = "kendall" )
      C2 <- cor(  data[-idx,  ]  , method = "kendall" )
      for (k in 1:ngrid){
         C1[  abs(C1) <  tgrid[k]  ] <- 0
         diag(C1) <- rep(1,p)
         FLOSSES[i,k] <- norm(C1-C2, "F")
      }
   }

   avgLoss <- colMeans(FLOSSES)
   hstar   <- tgrid[which.min(avgLoss)]

   ans <- list()
   ans$avgloss   <- avgLoss
   ans$threshold <- hstar

   return(ans)
}




## Cross-validation for Spearman Rho
cv.Rho <- function(data, nsplits=10, monitor=1){
   n <- nrow(data)
   p <- ncol(data)
   tgrid    <- seq(0.005, 0.995, by=0.05)
   ngrid    <- length(tgrid)
   n1       <- n - floor(n/log(n))
   n2       <- n - n1

   ## Estimate expected loss
   C1 <- C2 <- matrix(0, nrow=p, ncol=p)
   FLOSSES  <- matrix(0, nrow=nsplits, ncol=ngrid)

   for (i in 1:nsplits){
      if (monitor>=1) { message("Split: ", i, " (of ", nsplits, ")") }
      idx <- sample(1:n, size=n1, replace=FALSE)
      C1 <- cor(  data[ idx,  ]  , method = 'spearman' )
      C2 <- cor(  data[-idx,  ]  , method = 'spearman' )
      for (k in 1:ngrid){
         C1[  abs(C1) <  tgrid[k]  ] <- 0
         diag(C1) <- rep(1,p)
         FLOSSES[i,k] <- norm( C1 - C2, "F")
      }
   }

   avgLoss <- colMeans(FLOSSES)
   hstar   <- tgrid[which.min(avgLoss)]

   ans <- list()
   ans$avgloss   <- avgLoss
   ans$threshold <- hstar

   return(ans)
}









################################################################################
## Software for the Monte Carlo experiment
################################################################################

## Construct an AR(1) covariance matrix
cov.ar <- function(p, rho = 0.7, marg.vars = NULL) {
   ans     <- rho ^ abs(outer(1:p, 1:p, "-"))

   if (!is.null(marg.vars)) {
      D <- diag(sqrt(marg.vars))
      ans <- D  %*% ans %*% D
   }

   return(ans)
}




## Generate random multivariate gaussian samples
rmnorm <- function(n, mean, eigcov, cov = NULL) {
   if (!is.null(cov)) {
      eigcov  <- eigen(cov, symmetric = TRUE)
   }
   P    <- length(mean)
   Mnp  <- matrix(mean, nrow = n, ncol = P, byrow = TRUE, dimnames = NULL)
   Z    <- matrix(rnorm(P * n), nrow = n, ncol = P, byrow = FALSE, dimnames = NULL )
   Q    <- eigcov$vectors %*% diag(sqrt(eigcov$values), n = P, ncol = P)
   ans  <- Mnp  +   Z %*% t(Q)
   return(ans)
}










## !! la seguente funzione ?? stata modificata !!  ##
# ## Generate contaminated data
dgp <- function(n, eigcov, cont.rate=0.15){
    p <- length(eigcov$values)
    X <- rmnorm(n=n, mean=rep(0, times=p), eigcov=eigcov)
    nout    <- floor(cont.rate * n)
    out.idx <- sample(1:n, size=nout, replace=FALSE)
    for (i in seq_along(out.idx)){
       # X[out.idx[i] , ] <- runif(p, 5, 250) * sample(c(-1,1), size=p, replace=TRUE)
       X[out.idx[i] , ]   <- 4 + 10 * rchisq(p, df=1)
    }
    ans <- list()
    ans$data <- X
    ans$class <- rep(1,n)
    ans$class[out.idx] <- 0
    return(ans)
}





################################################################################
## Misc
################################################################################

##   Compute computing speed
##   start.time <- Sys.time()
##   x <- rnorm(1e+6)
##   compute.speed(start.time)
compute.speed <- function(start){
   ans <- Sys.time() - start
   return(as.numeric(ans))
}



## Support recovery statistics
##
fitcounts <- function(ref, fitted, ht=0.75){

   ref    <- ref[upper.tri(ref, diag=FALSE)]
   fitted <- fitted[upper.tri(fitted, diag=FALSE)]

   ans <- c(zero2nonzero=0,   zero2high=0, high2zero=0,  signswitch=0)

   ## zero2nonzero:  zero correlation set to non-zero correlations
   ans[1] <- sum(ref==0 & fitted!=0)

   ## zero2high: zero correlations set to high correlation (>ht in absolute values)
   ans[2] <- sum(ref==0 & abs(fitted)>ht)

   ## high2zero:  non zero correlations set to zero
   ans[3] <- sum(abs(ref)>ht & fitted==0)

   ## signswitch: Switch sign
   ans[4] <- sum({ref>0 & fitted<0}   |   {ref<0 & fitted>0})

   return(ans)
}




## !! la seguente funzione ?? stata modificata !!  ##
## cormad vector from <cormad.vec>  to cormad  matrix conversion
cormad.vec2mat <- vec2mat <- function(rho){

   ncors <- length(rho)
   p     <- {1+sqrt(1 + 4*2*ncors )}/2
   ans   <- matrix(1, ncol=p, nrow=p)
   idx   <- which(upper.tri(ans, diag=FALSE), arr.ind=TRUE)

   for (k in 1:ncors ){
      i <- idx[k,1]  ## row index of the upper triangle
      j <- idx[k,2]  ## col index of the upper triangle
      ans[i,j]<-ans[j,i]<- rho[k]
   }

   return(ans)
}


## Vectorized upper triangle of the sample correlation matrix
corsamp.vec <- function(x){
   R_samp   <- cor(x)
   R_samp   <- R_samp[upper.tri(R_samp, diag=FALSE)]
   return(R_samp)
}








