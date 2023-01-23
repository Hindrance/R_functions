# FOR KDE NBC MODEL feature / class distribution plotting
  plot.model.kdes = function(model, feature, colours=Discrete12){
    x.range = model[["kdes"]][[feature]][,"x.range"]
    density.matrix = model[["kdes"]][[feature]][,model$classes]
    plot(range(x.range), range(density.matrix), pch=NA, xlab="Feature TFIDF", ylab="Likelihood")
    for(i in order(colMeans(model[["kdes"]][[feature]][,model$classes]))){
      class.i = model$classes[i]
      polygon(c(x.range[1],x.range,x.range[512]), c(0,density.matrix[,class.i],0), col=colours[i], lty=0)
    }
    for(i in order(colMeans(model[["kdes"]][[feature]][,model$classes]))){
      class.i = model$classes[i]
      lines(seq(min(x.range), max(x.range), length=512), density.matrix[,i], col=colours[i])
    }   
    legend("topleft", legend=model$classes, col=colours, bty="n", pch=15)
  }
  
