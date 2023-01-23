
#Dependency : RColorBrewer
library(RColorBrewer)

# EDIT - -- -- -- -- -- Two functions - - --- - --- - --
# Taken from github to add alpha values into brewer colour ramps.

# addalpha()
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# colorRampPaletteAlpha()
colorRampPaletteAlpha <- function(colors, n=100, interpolate='linear') {
  # Create the color ramp normally
  cr <- colorRampPalette(colors, interpolate=interpolate)(n)
  # Find the alpha channel
  a <- col2rgb(colors, alpha=T)[4,]
  # Interpolate
	if (interpolate=='linear') {
		l <- approx(a, n=n)
	} else {
		l <- spline(a, n=n)
	}
	l$y[l$y > 255] <- 255 # Clamp if spline is > 255
	cr <- addalpha(cr, l$y/255.0)
	return(cr)
}

line.error.plot <- function(x, line_data, error_data, polygon=T, variable.colour=T,line.colour,plot.new,polygon.colour,polygon.outline,cex,ylim,...){
if(missing(polygon)){polygon=T}
if(missing(plot.new)){plot.new=T}
if(missing(variable.colour)){variable.colour=T}
if(missing(line.colour)){line.colour="red"}
if(missing(polygon.colour)){polygon.colour=c("white", "black")}
if(missing(polygon.outline)){polygon.outline=T}
if(missing(cex)){cex=2}

x.1 = length(line_data)
y = line_data
z = error_data

if(missing(ylim)){ylim=c(min(y-z),max(y+z))}
if(plot.new==T){
par(cex=cex, lend=2)
plot(x, x, ylim=ylim, pch=NA, ...)
}

if(polygon==T){
if(variable.colour==T){
#z.col <- sapply(1:x, function(i) {paste("grey",(round(1-(z[i]-min(z))/max((z-min(z))),2))*100,sep="")})
rbPal <- colorRampPaletteAlpha(polygon.colour)
z.col <- rbPal[as.numeric(cut(z/y,breaks = 100))]
	pgv.x <- c(x,rev(x))
	pgv.y <- c(y+z,(y-z)[x.1:1])
	for(i in 0:(x.1-1)){ # This is a fun polygon filling exercise...
		moo.x <- rep(pgv.x[(i+1):(i+2)],each=2)
		moo.y <- pgv.y[c(
				i+1,                 # 1
				(length(pgv.y)-i),   # 2
				(length(pgv.y)-1-i), # 3
				i+2                  # 4
				)]
		polygon(moo.x, moo.y, col=z.col[i+1], lty=0)
	}
	} else {polygon(c(1:x,x:1), c(y+z,(y-z)[x.1:1]), lwd=0.1, col="grey80")}
	if(polygon.outline==T){polygon(c(x,rev(x)), c(y+z,(y-z)[x.1:1]), lwd=1)}
}



	lines(x,y, col="white", lwd=cex+3)
	lines(x,y, col="black", lwd=cex+1)
	lines(x,y, col=line.colour, lwd=cex-1)

}

