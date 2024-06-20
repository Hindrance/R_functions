 # This function adds a gradient legend to the side of a plot. So far it only includes
 # the option of side 4 (right hand side).


 gradient.legend <- function(data, side=4, colours, scale=1, distance=0, label="", xpd=NA, cex=1, col.axis=par("col.axis"), yjust=0, nticks = 3, ...){
 # some adjustments to ensure that it does not override current par() settings...
# cex.old = par()$cex
 #par(cex=cex)
 
 if(side==4){
  x.lim <- par("usr")[2]*((distance*0.08)+1.05)
  
    colour.scale <- rev(rgb(colorRamp(colours,alpha=T)(normalise(1:200))/255,alpha=(colorRamp(colours,alpha=T)(normalise(1:200))/255)[,4]))
    legend.x <- rep(x.lim,200)#*((distance*0.08)+1.05)
    legend.y <- seq(par("usr")[4]-(1-scale)*diff(c(par("usr")[3],par("usr")[4])), par("usr")[3]+(1-scale)*diff(c(par("usr")[3],par("usr")[4])), length=200)
    legend.y = legend.y + yjust
    axis(side, pos=x.lim*1.02, at=yjust+mean(par("usr")[3:4]), labels=label, tcl = 0, col.ticks="white", xpd=NA, ...) 
    axis(
       side,
       pos=x.lim,
       at=yjust+seq(par("usr")[4]-(1-scale)*diff(c(par("usr")[3],par("usr")[4])),
       par("usr")[3]+(1-scale)*diff(c(par("usr")[3],par("usr")[4])),
       length=nticks),
       labels=signif(seq(max(data), min(data), length=nticks),2),
       padj=-1, cex.axis=cex,
       col.axis=col.axis,
       col.ticks=col.axis,
       col=col.axis,
       col.lab=col.axis,
       xpd=NA,
       ...
     )
    points(legend.x, legend.y, pch=15, col=colour.scale, xpd=NA)
    }
  # reset
# par()$cex = cex.old


#  if(side==1){
#  y.lim <- par("usr")[3]*((distance*0.08)+1.05)
#  
#    colour.scale <- rev(rgb(colorRamp(colours,alpha=T)(normalise(1:200))/255,alpha=(colorRamp(colours,alpha=T)(normalise(1:200))/255)[,4]))
#    legend.y <- rep(y.lim,200)+ yjust#*((distance*0.08)+1.05)
#    legend.x <- seq(par("usr")[2]-(1-scale)*diff(c(par("usr")[1],par("usr")[2])), par("usr")[1]+(1-scale)*diff(c(par("usr")[1],par("usr")[2])), length=200)
#    legend.x = legend.x
#    axis(side, pos=y.lim*1.02, at=yjust+mean(par("usr")[1:2]), labels=label, tcl = 0, col.ticks="white", ...) 
#    axis(side, pos=y.lim,      at=yjust+seq(par("usr")[2]-(1-scale)*diff(c(par("usr")[1],par("usr")[2])), par("usr")[1]+(1-scale)*diff(c(par("usr")[1],par("usr")[2])),length=11), labels=signif(seq(max(data), min(data), length=11),2), padj=-1, cex.axis=cex, col.axis=col.axis, col.ticks=col.axis, col=col.axis, col.lab=col.axis, ...)
#    points(legend.x, legend.y, pch=15, col=colour.scale, xpd=NA)
#    
#    legend(mean(par("usr")[1:2]), distance, legend=0:200, pch=15, col=colour.scale, bty="n", xpd=NA, horiz=T)
#    legend(mean(par("usr")[1:2]), distance-1, pch="|", col=colour.scale, bty="n", xpd=NA)
#    legend(mean(par("usr")[1:2]), distance-2, pch=15, col=colour.scale, bty="n", xpd=NA, horiz=T)
#    
#    }
#    
  }
  

