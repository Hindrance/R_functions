
  # This is a function to plot seurat objects instead of their normal function... just a simpler version with neater output
  expression.so = function(
    so, 
    md, 
    genes,
    colours = Discrete, 
    leg=F, 
    main=md, 
    cex=0.6, 
#    reduction="umap", 
#    label=F, 
#    RO=1:dim(so)[2], 
    slot.use = "data", 
    assay.use = DefaultAssay(so), 
#    pch=16, 
#    lab.size=2,
#    lab.circle=T,
#    lab.y.offset=0,
#    lab.x.offset=0,
    new=T,
    d.plot=F,
    hm.plot=F,
    ...
    ){
    
      genes = genes[genes %in% rownames(so)]
      
      x.df = GetAssayData(so, assay=assay.use, slot=slot.use)[genes,,drop=F]
      x.df = x.df[rowSums(x.df) > 0 ,]
      d2p = colMeans(x.df)
      x.df = t(sapply(1:nrow(x.df), function(i){x.df[i,] / max(x.df[i,])}))
      y.df = colMeans(x.df)
      
      mem = sapply(levels(so@meta.data[,md]), function(k){mean(y.df[so@meta.data[,md] == k, drop=F])})
      sdem = sapply(levels(so@meta.data[,md]), function(k){sd(y.df[so@meta.data[,md] == k, drop=F])})
      
      if(new==T){par(oma=c(8,1,1,1))}
      if(d.plot != T){  
      boxplot(y~x,
        data.frame(
        "x" = so@meta.data[,md],
        "y" = y.df
        ),
        las=3,
        ylab="mean log2 expression of gene(s)",
        xlab="",
        main=main
      )
      
#      H = barplot(mem, las=3, ylim=c(0, max(mem*1.8)))
#      arrows(H, mem, H, mem+sdem, angle=90, length=0.1)
    }

  if(d.plot == T){ 
   col2plot = c(
     Discrete.U2[c(1:9, 11:19)],
     Discrete.U2[1:19]
     )
     
    # d2p = y.df
    classes = so@meta.data[,md]
    levels(classes) = levels(classes)[order(sapply(levels(classes), function(i){mean(d2p[classes==i])}))]

  #plot.dark()
  par(oma=c(4,10,1,1))
    plot(range(d2p), c(0, length(levels(classes)))+1, pch=NA, ylab="", yaxt="n", xlab="Gene expression (scaled log2 counts + 1)")
   for(i in length(levels(classes)):1){
      class.i = order(sapply(levels(classes), function(i){mean(d2p[classes==i])}))[i]
      
      goi.density = density(d2p[classes==levels(classes)[class.i]], from=range(d2p)[1], to=range(d2p)[2], bw=diff(range(d2p))/50)
      scale.factor = 1.2 / max(goi.density$y)
      polygon(c(goi.density$x[1], goi.density$x, tail(goi.density$x,1)), c(i, i+scale.factor*c(goi.density$y), i), col=colours[class.i])
    }
  
     
    axis(2, at=1:length(levels(classes)), labels=levels(classes), las=2)
    mtext("scaled counts")
    abline(v=0, lty=2)
   } 
  
   if(hm.plot == T){ 
      matrix.plot(mem)
   }
   
      
  }
   

