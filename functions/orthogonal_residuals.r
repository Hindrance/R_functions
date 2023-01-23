
### orthogonal residuals, including a weight parameter (not customisable yet) which scales the relationship by ratio of variance


 orthogonal.residuals = function(x, y, weight=T, plot.it=F){
  
 # usages::
  
 # x 
 # y
 # weight == T or F (depending if you seek true orthogonality (where variances of X and Y are similar or assumed to be not important)
 # plotit == T or F depending on if you want to see the inner workings of the function and troubleshoot and weird results.
 
 h
    # dataframe format 
      df = data.frame("x" = x, "y" = y) 
    # create linear model  
      linear.model = lm(y~x, df)
    # parse coefficients
      y.coefficient = linear.model$coefficient
     
    # linear model for regression line: 
    # a1 * x1 + b1 * y1 = c1
      a1 = as.numeric(y.coefficient[2])
      b1 = 1
      c1 = as.numeric(y.coefficient[1])

    # apply function over x and y to find orthogonal residual
      output.1 = t(sapply(1:length(df[,1]), function(i){
        x1 = as.numeric(df[i,1])
        y1 = as.numeric(df[i,2])

      # perpendicular, but we don't know x2 and y2
      # a2* x2 + b2 * y2 = c2

      # we can calculate the slope of the line (negative reciprocal)
        a2 = -(1/a1)  

      # apply a weight, here I choose a variance as a weight (the reason we do this is because although 
      # we want an orthogonal intersection, the influence of Y over X using real orthogonality may be 
      # too much for X. For example, in cases where the range, size and therefore variance of Y is much greater than X 
        if(weight == T){ 
          w = var(y)/var(x)
          a2 = w*a2
        }
        
      # and consider that our y coefficient is 1.
        b2 = 1

      # And we can replace the values of x here to solve to find the intercept (c2)
        c2 = y1 - a2*x1

      # Cramers rule to find Coordinate B
        x2 = (b1*c2 - b2*c1)/(a1*b2 - a2*b1)
        y2 = -(c1*a2 - c2*a1)/(a1*b2 - a2*b1)
        
        return(c(x2, y2, a2, b2, c2))
      })
    )

  # locate first point in residual     
    min.pt = which.min(sapply(1:length(output.1[,1]), function(i){prod(output.1[i,1:2])}))
  # compare distance of all other points to this one (to create a continuum)
    
    pseudo.dist = sapply(1:length(output.1[,1]), function(i){
        x1 = output.1[min.pt,1]
        y1 = output.1[min.pt,2]
        
        x2 = output.1[i,1]
        y2 = output.1[i,2]
      # Distance equation:
        d = sqrt((x2-x1)^2+(y2-y1)^2)
        return(d)
    })
    
  # calculate residual distances 
    orth.residuals = sapply(1:length(output.1[,1]), function(i){
        x1 = df[i,1]
        y1 = df[i,2]
        
        x2 = output.1[i,1]
        y2 = output.1[i,2]
      # Distance equation:
        d = sqrt((x2-x1)^2+(y2-y1)^2)
        return(d)
    })
    
  # construct output dataframe  
    output = data.frame(
    "x" = x,
    "y" = y,
    "x2" = output.1[,1],
    "y2" = output.1[,2],
    "continuum" = pseudo.dist,
    "residuals" = orth.residuals,
    "a2" = output.1[,3],
    "b2" = output.1[,4],
    "c2" = output.1[,5]
    )
#  plot if you want to see if the function is messed up (troubleshooting) 
   if(plot.it == T){
    plot(df)
    abline(linear.model)
      for (i in 1:length(df[,1])){
        x1 = as.numeric(df[i,1])
        y1 = as.numeric(df[i,2])

      # Plot coordinate A
        points(x1, y1, pch=16, col="red")
      # points for intersect
        points(output.1[i,1], output.1[i,2], pch=16, col="blue")
      # perpendicular line
        abline(output.1[i,5], output.1[i,3], lty=2) 
      }
    }
      
  # return output
    return(
      list(
        "output" = output,
        "linear.model" = linear.model
      )
     )
  }
 
# 
##### example: uncomment to run
#x = trees[,1]
#y = trees[,3]
# 
#test = orthogonal.residuals(x,y, weight=T)
#linear.model = test$linear.model

#par(mfrow=c(2,1))
#plot(x, y)
#abline(linear.model)
#segments(x, y, x, linear.model$coefficient[2]*x+linear.model$coefficient[1], col="red")


#segments(test[[1]][,1], test[[1]][,2], test[[1]][,3], test[[1]][,4], col="blue")

#plot((x-min(x))/(max(x)-min(x)), rep(1,length(x)), ylim=c(0,3))

#  x3 = test$output[,"continuum"]
#points((x3-min(x3))/(max(x3)-min(x3)), rep(2,length(x)))




