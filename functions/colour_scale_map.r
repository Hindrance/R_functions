require(RColorBrewer)
# quick colour gradient...
  cam.grade = c(
      "#5000B6",
      cambridge.sunset[c(2,1,6,8,9)],
      "#CF0000"
      )


# Function to create bi-directional maps
  colour.map <- function(x, y = x, col=cam.grade, decimal.places=1, report=F) {
    if(length(unique(as.numeric(na.omit(y / abs(y))))) > 1) {
      gradient = 2
    } else {gradient = 1}
    
    if(gradient == 2){
      bounds = c(-max(abs(range(y))), max(abs(range(y))))
      bounds = round(c(-max(abs(range(y))), max(abs(range(y)))),decimal.places)
      range.col = seq(bounds[1], bounds[2], 1/(10^decimal.places))
    } else {
      bounds = c(min(abs(range(y))), max(abs(range(y))))
      bounds = round(c(min(abs(range(y))), max(abs(range(y)))),decimal.places)
      range.col = seq(bounds[1], bounds[2], 1/(10^decimal.places))
    }
    
    
    col.df = data.frame(
      colorRampPalette(col)(length(range.col)),
      range.col
      )
    
    col2plot = col.df[match(as.character(round(x, decimal.places)), as.character(col.df[,2])),1]

    
    if(report==T){
      return(col.df)
    } else {
      return(col2plot)
    }
  }
  
  
# Sub functions:
# This function generates a gradient between two points (linear gradient, linear line)  
  
  
  gradient.line <- function(x0, y0, x1, y1, length=10, cols=c("white", "black"), ...) {
    l = length
    x.vector <- sapply(0:l, function(i) {x0+diff(c(x0,x1)) * ((0+i) / l)})
    y.vector <- sapply(0:l, function(i) {y0+diff(c(y0,y1)) * ((0+i) / l)})

    x0 = x.vector[(1:length(x.vector))-1]
    x1 = x.vector[2:length(x.vector)]
    y0 = y.vector[(1:length(y.vector))-1]
    y1 = y.vector[2:length(y.vector)]

    segments(x0,y0,x1,y1, col=colorRampPalette(colors=cols)(l), ...)
  }

  
    
  # Legend function...
# Colour.map legend...?
  colour.map.legend = function(x, col=cam.grade, pos1=0.25, pos.scale=0.5, z=3, side=1, line=3, label="scale", cex=0.5, adj=0.033, decimal.places=1, custom.range="n", lwd=8){ 
 
    bounds = c(-max(abs(range(x))), max(abs(range(x))))
    bounds = round(c(-max(abs(range(x))), max(abs(range(x)))),decimal.places)
    range.col = seq(bounds[1], bounds[2], 1/(10^decimal.places))
    if(custom.range[1]!="n"){range.col = seq(custom.range[1], custom.range[2], 1/(10^decimal.places))}
    
    if(side == 1){   
    # Start position, relative to x-axis size   
       a = par("usr")[1] + (pos1 * diff(range(par("usr")[1:2])))

    # Scale, relative to x-axis size (defines end-point of axis)
       b = a + (pos.scale * diff(range(par("usr")[1:2])))
       
       gradient.line(
            a,
            par("usr")[3]-((diff(par("usr")[3:4])*adj*line)),
            b,
            par("usr")[3]-((diff(par("usr")[3:4])*adj*line)),
            100,
            cols = col,
            xpd=NA,
            lwd=lwd,
            lend=3
          )
          
      lines(
            c(a,b),
            rep(par("usr")[3]-((diff(par("usr")[3:4])*adj*line)+(diff(par("usr")[3:4]))*0.005),2),
            xpd=NA,
            lwd=1,
            lend=3
          )
      text(seq(a, b, length = z), rep(par("usr")[3]-((diff(par("usr")[3:4])*adj*line)+(diff(par("usr")[3:4]))*0.01),z), "|", xpd=NA, cex=0.8)
      text(seq(a, b, length = z), rep(par("usr")[3]-((diff(par("usr")[3:4])*adj*line)+(diff(par("usr")[3:4]))*0.025),z), 
        signif(seq(range(range.col)[1], range(range.col)[2], length=z),2), xpd=NA, cex=cex
        )
      text(mean(c(a,b)), par("usr")[3]-((diff(par("usr")[3:4])*adj*line)+(diff(par("usr")[3:4]))*0.05), label, xpd=NA, cex=cex*1.1)
      }

    if(side == 2){   
    # Start position, relative to y-axis size   
       a = par("usr")[4] - (pos1 * diff(range(par("usr")[4:3])))

    # Scale, relative to y-axis size (defines end-point of axis)
       b = a - (pos.scale * diff(range(par("usr")[4:3])))
       
       gradient.line(
            par("usr")[1]-((diff(par("usr")[1:2])*adj*line)),
            b,
            par("usr")[1]-((diff(par("usr")[1:2])*adj*line)),
            a,
            100,
            cols = col,
            xpd=NA,
            lwd=lwd,
            lend=3
          )
          
      lines(
            rep(par("usr")[1]-((diff(par("usr")[1:2])*adj*line)+(diff(par("usr")[1:2]))*0.005),2),
            c(b,a),
            xpd=NA,
            lwd=1,
            lend=3
          )
      text(rep(par("usr")[1]-((diff(par("usr")[1:2])*adj*line)+(diff(par("usr")[1:2]))*0.01),z), seq(b, a, length = z), "|", xpd=NA, cex=0.8, srt=90)
      text(rep(par("usr")[1]-((diff(par("usr")[1:2])*adj*line)+(diff(par("usr")[1:2]))*0.025),z), seq(b, a, length = z), 
        signif(seq(range(range.col)[1], range(range.col)[2], length=z),2), xpd=NA, cex=cex, crt=2
        )
      text(par("usr")[1]-((diff(par("usr")[1:2])*adj*line)+(diff(par("usr")[1:2]))*0.05), mean(c(a,b)), label, xpd=NA, cex=cex*1.1, srt=90)
      }
        


    if(side == 3)
      {   
    # Start position, relative to x-axis size   
       a = par("usr")[1] + (pos1 * diff(range(par("usr")[1:2])))

    # Scale, relative to x-axis size (defines end-point of axis)
       b = a + (pos.scale * diff(range(par("usr")[1:2])))
       
       gradient.line(
            a,
            par("usr")[4]+((diff(par("usr")[3:4])*adj*line)),
            b,
            par("usr")[4]+((diff(par("usr")[3:4])*adj*line)),
            100,
            cols = col,
            xpd=NA,
            lwd=lwd,
            lend=3
          )
          
      lines(
            c(a,b),
            rep(par("usr")[4]+((diff(par("usr")[3:4])*adj*line)+(diff(par("usr")[3:4]))*0.005),2),
            xpd=NA,
            lwd=1,
            lend=3
          )
      text(seq(a, b, length = z), rep(par("usr")[4]+((diff(par("usr")[3:4])*adj*line)+(diff(par("usr")[3:4]))*0.01),z), "|", xpd=NA, cex=0.8)
      text(seq(a, b, length = z), rep(par("usr")[4]+((diff(par("usr")[3:4])*adj*line)+(diff(par("usr")[3:4]))*0.025),z), 
        signif(seq(range(range.col)[1], range(range.col)[2], length=z),2), xpd=NA, cex=cex
        )
      text(mean(c(a,b)), par("usr")[4]+((diff(par("usr")[3:4])*adj*line)+(diff(par("usr")[3:4]))*0.05), label, xpd=NA, cex=cex*1.1)        
      }


        
    if(side == 4){   
    # Start position, relative to y-axis size   
       a = par("usr")[4] - (pos1 * diff(range(par("usr")[4:3])))

    # Scale, relative to y-axis size (defines end-point of axis)
       b = a - (pos.scale * diff(range(par("usr")[4:3])))
       
       gradient.line(
            par("usr")[2]+((diff(par("usr")[1:2])*adj*line)),
            b,
            par("usr")[2]+((diff(par("usr")[1:2])*adj*line)),
            a,
            100,
            cols = col,
            xpd=NA,
            lwd=lwd,
            lend=3
          )
          
      lines(
            rep(par("usr")[2]+((diff(par("usr")[1:2])*adj*line)+(diff(par("usr")[1:2]))*0.005),2),
            c(b,a),
            xpd=NA,
            lwd=1,
            lend=3
          )
      text(rep(par("usr")[2]+((diff(par("usr")[1:2])*adj*line)+(diff(par("usr")[1:2]))*0.01),z), seq(b, a, length = z), "|", xpd=NA, cex=0.8, srt=90)
      text(rep(par("usr")[2]+((diff(par("usr")[1:2])*adj*line)+(diff(par("usr")[2:2]))*0.025),z), seq(b, a, length = z), 
        signif(seq(range(range.col)[1], range(range.col)[2], length=z),2), xpd=NA, cex=cex, crt=2
        )
      text(par("usr")[2]+((diff(par("usr")[3:4])*adj*line)+(diff(par("usr")[3:4]))*0.05), mean(c(a,b)), label, xpd=NA, cex=cex*1.1, srt=90)
      }
        
  }
  
 


