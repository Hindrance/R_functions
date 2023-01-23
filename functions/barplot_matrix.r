# Simple generation of categorical vs categorical matrix to produce barplots

  bar.matrix <- function(var1, var2, plot=T, label="% of cells", colours=Discrete12,  scale=T, leg=T, ...)  { 
    
   
    cell.id.matrix <- matrix(0, length(levels(var1)), length(levels(var2)))
    rownames(cell.id.matrix) <- levels(var1)
    colnames(cell.id.matrix) <- levels(var2)
    
    for(i in levels(var2)){
      cell.id.matrix[names(table(var1[var2==i])),i] <- table(var1[var2==i])
    }
    
    cell.id.matrix[is.na(cell.id.matrix)] = 0
    
    if(scale==T){cell.id.matrix = t(100*t(cell.id.matrix)/colSums(cell.id.matrix))}
    
    if(plot == T){
    #par(fig=c(0,0.8,0,1), mar=c(4,4,4,4), oma=c(0,0,0,0)); 
    temp = barplot(cell.id.matrix,
        col=colours, las=1, yaxt="n", border=NA, cex.axis=0.7,
        horiz=F, ...
    )
    mtext(label, 2, padj=-3)
    axis(2)
    if(leg==T){
    legend.4(legend=levels(var1), 
          fill=colours, bty="n", cex=0.8, y.inters= 0.8, 
          xjust=0, yjust=0.5)
    }
    return(
      list(
        "barx" = temp,
        "bary" = colSums(cell.id.matrix),
        "barF" = signif(table(var2)/sum(table(var2))*100, 3),
        "cell.id.matrix" = cell.id.matrix
      )
    )
    } else {
    
    return(cell.id.matrix)
    
    }
  
  }
  
