
  # This is a function to plot seurat objects instead of their normal function... just a simpler version with neater output
  plot.so = function(
    so, 
    md, 
    colours = Discrete, 
    leg=F, 
    main=md, 
    cex=0.6, 
    reduction="umap", 
    label=F, 
    RO=1:dim(so)[2], 
    slot.use = "scale.data", 
    assay.use = DefaultAssay(so), 
    pch=16, 
    lab.size=2,
    lab.circle=T,
    lab.y.offset=0,
    lab.x.offset=0,
    lab.text=1,
    ...
    ){
    
    if(md %in% rownames(GetAssayData(so, assay=assay.use, slot=slot.use))){
    d2p = colSums(GetAssayData(so, assay=assay.use, slot=slot.use)[md,,drop=F])
      RO = order(d2p)
      col2use = plotcols(d2p, colours=colours)[RO]
      plot(Embeddings(so, reduction=reduction)[RO,1:2], pch=pch, col=col2use, cex=cex, main=main, ...)
      if(leg==T){gradient.legend(d2p, colours = colours, cex=cex, scale=0.8, col.axis=par()$fg, label=md)}
    } else {

      if(class(so@meta.data[RO,md]) == "numeric"){
         RO = order(so@meta.data[,md])
         plot(Embeddings(so, reduction=reduction)[RO,1:2], pch=pch, col=plotcols(so@meta.data[,md], colours=colours)[RO], cex=cex, main=main, ...)
         if(leg==T){gradient.legend(so@meta.data[,md], colours = colours, cex=cex, scale=0.8, col.axis=par()$fg, label=md)}
      } else {
         
        plot(Embeddings(so, reduction=reduction)[RO,1:2], pch=pch, col=colours[as.factor(so@meta.data[,md])][RO], cex=cex, main=main, ...)
        if(leg==T) {
          present = levels(as.factor(so@meta.data[RO,md])) %in% as.character(so@meta.data[RO,md])
          legend.4(paste(levels(as.factor(so@meta.data[RO,md]))[present], " - ", table(as.factor(so@meta.data[RO,md]))[present], " (", round(100*table(as.factor(so@meta.data[RO,md]))/sum(table(as.factor(so@meta.data[RO,md]))))[present], " %)", sep=""), col=colours[present], pch=16, bty="n")
        }
         
         
        if(label==T){
        bounds=par("usr")
        usr.width = diff(bounds[1:2])
        usr.height = diff(bounds[3:4])
         pt.mid <<- t(sapply(levels(as.factor(so@meta.data[,md])), function(k) {
              c(mean(Embeddings(so, reduction=reduction)[so@meta.data[RO,md] == k,1]),
                mean(Embeddings(so, reduction=reduction)[so@meta.data[RO,md] == k,2]))
            }))
            if(lab.circle==T){points(pt.mid[,1]+usr.width*lab.x.offset, pt.mid[,2]+usr.height*lab.y.offset, pch=21, cex=lab.size, bg='white')}
            text(pt.mid[,1]+usr.width*lab.x.offset, pt.mid[,2]+usr.height*lab.y.offset, levels(as.factor(so@meta.data[,md])), col="black", cex=lab.text*lab.size/3)
        }
      }
    }
  }
   

