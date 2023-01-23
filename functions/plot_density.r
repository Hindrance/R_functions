# Plotting of densities for distributions


  plot.density <- function(x, classes, bw.multiplier = 0.05, bw = diff(range(x))*bw.multiplier, colours=Discrete, PDF=F, report=F, range.modifier = c(0.2, 0.2), ...){
    x.range = seq(min(x) - (range.modifier[1]*diff(range(x))), max(x) + (range.modifier[2]*diff(range(x))), length=512)
      
      
      if(PDF == F){
          density.matrix = sapply(levels(classes), function(class) {
            density(x[classes==class], bw=bw, from=min(x.range), to=max(x.range))$y / max(density(x[classes==class], bw=bw, from=min(x.range), to=max(x.range))$y)
            }
          )
      } else {
          density.matrix = sapply(levels(classes), function(class) {
            density(x[classes==class], bw=bw, from=min(x.range), to=max(x.range))$y
            }
          )
      }
      if(report == F){
# call the plot   
    plot(c(min(x.range), max(x.range)), range(density.matrix), pch=NA, ...)
      for(i in 1:ncol(density.matrix)){
         polygon(c(x.range[1],x.range,x.range[512]), c(0,density.matrix[,i],0), col=colours[i], lty=0)
       }
      for(i in 1:ncol(density.matrix)){
         lines(seq(min(x.range), max(x.range), length=512), density.matrix[,i], col=colours[i])
       }
       
       } else {
        density.matrix = data.frame(density.matrix)
        colnames(density.matrix) = levels(classes)
        density.matrix[,"x.range"] = x.range
        return(density.matrix)
       }
       
   }
