# A function for plotting Dot Plots piped from Seurat FindAllMarkers() into our custom pipeline.
# run extract.top() first to cut the FindAllMarkers() results down.

# Function and arguments
  dot.plot <- function(
    x,                                # Seurat Object (need to gather expression data)
    clustering.results,               # extract.top() results object
    subset.classes = "none",          # vector of factor levels / classes /clusters to subset
    features = "top.gene",            # Default uses top n genes from clustering.results, otherwise specify a custom vector of features.
    re.order = T,                     # re-order the columns (genes) by their grouping in clusters
    n = 5,                            # n top to plot
    n.cluster = n,                    # n genes to use in clustering (default, the same as for the plotting).
    annotation = "seurat_clusters",   # annotation to group by (must be identical to the FindAllMarkers() results
    clustering.variable = "gene.expression",      # annotation to cluster the main annotation by (heirarchical) - you can also choose a metadata annotation if you want
    slot.use = "data",          # 
    assay.use = DefaultAssay(x),      # Default Assay as default
    legend = T,                       # Legend to plot?
    l.padj = 1,                       # Scaling for legend horizontal adjust (multiplicative)
    l.adj = -0.1,                        # Scaling for legend vertical adjust (multiplicative)
    l.grad.adj = -5,
    cex = 1,                          # scaling multiplier
    addon = F,                        # Add onto existing plot
    addon.margins = c(8,5,8,5),       # additional margins
    title = "Unique top markers",     # title
    lwd = 1,                          # Line thickness
    distance = 0.25,                  # legend gradient distance...
    x.labels = T,                     # Add cluster association x axis labels?
    suppress.output=T
  ){
    
    if(subset.classes[1] != "none"){
      x = subset(x, cells=colnames(x)[x@meta.data[,annotation] %in% subset.classes])
      clustering.results = subset.list(clustering.results, subset.classes)
    }
    
    clusters2use = levels(x@meta.data[,annotation])[levels(x@meta.data[,annotation]) %in% x@meta.data[,annotation]]
    
  # Locate all genes available in our Assay for the plotting process
    genes.available = rownames(GetAssayData(x, assay=assay.use, slot=slot.use))
  
  # initial feature grab without ordering  
      featureset = unique(unlist(lapply(clusters2use,
         function(i){clustering.results[[i]]$gene[1:n.cluster]}))
       )
    # Re-assign features based on input argument
      if(features[1] != "top.gene"){featureset = features}
    # Filter out features that don't appear in matrix 
      featureset = featureset[featureset%in%genes.available]

  # grab the matrix that we're plotting
    y = GetAssayData(x, assay=assay.use, slot=slot.use)[featureset,]
    gc()
    
  # mean expression matrix...  
   cluster.mean.expressions = t(sapply(clusters2use, function(k) {
          rowMeans(y[,colnames(x)[x@meta.data[,annotation] == k]])
        }))
        
         
  if(clustering.variable == "gene.expression"){
#    # initial feature grab without ordering  
#      featureset = unique(unlist(lapply(levels(x@meta.data[,annotation]), function(i){clustering.results[[i]]$gene[1:n]})))
#    # Re-assign features based on input argument
#      if(features[1] != "top.gene"){featureset = features}
#    # Filter out features that don't appear in matrix 
#      featureset = featureset[featureset%in%genes.available]

    # Clustering of clusters by mean expression   
      cluster.mean.expressions = t(sapply(clusters2use, function(k) {
          rowMeans(y[,colnames(x)[x@meta.data[,annotation] == k]])
        }))
    # Ordering via clustering... We will stick with only one cluster ordering to make 
    # comparisons easier
      cid.cluster = hclust(dist(cluster.mean.expressions))
      
  } else {
    # We can use the categorical variable provided to cluster by the distributions of whatever metadata annotation we wanted
    # First we need to cluster the results into a meaningful pattern... 
    # here we can use any variable we want. We can stick also with correlations
    # HCC.nbc var
       # var1
          var1 = as.factor(x@meta.data[,clustering.variable])
        # var2
          var2 = as.factor(x@meta.data[,annotation])
          
      cell.id.matrix <- matrix(0, length(levels(var1)), length(levels(var2)))
      rownames(cell.id.matrix) <- levels(var1)
      colnames(cell.id.matrix) <- levels(var2)
      # adult pre
      for(i in levels(var2)){
        cell.id.matrix[names(table(var1[var2==i])),i] <- table(var1[var2==i])
      }
     # Ordering via clustering... We will stick with only one cluster ordering to make 
     # comparisons easier
      cid.cluster = hclust(dist(t(100*t(t(cell.id.matrix)/(colSums(cell.id.matrix)+1)))))
  
  }

        
# Then, using the ordering above, we re-pull out the features by the cluster order (to order the matrix)  
# take top n genes
  featureset = unique(unlist(lapply(clusters2use[cid.cluster$order], function(i){clustering.results[[i]]$gene[1:n]})))
    if(features[1] != "top.gene"){featureset = features}
  # Filter out features that don't appear in matrix 
    featureset = featureset[featureset%in%genes.available]


  # grab the matrix that we're plotting
    y = GetAssayData(x, assay=assay.use, slot=slot.use)[featureset,]
    gc()
    
  # mean expression matrix...  
   cluster.mean.expressions = t(sapply(clusters2use, function(k) {
          rowMeans(y[,colnames(x)[x@meta.data[,annotation] == k]])
        }))
        

# We now split these genes into a list
# 1. either from top n or
# 2. from custom list

  # 1.
  # apply as list to lump into clusters  
#    featureset.list = lapply(clusters2use[cid.cluster$order], function(i){
#      data.frame(
#        "gene" = clustering.results[[i]]$gene[1:n],#[clustering.results[[i]]$gene[1:n]%in%featureset],
#        "cluster" = i
#        )
#      })
#     featureset.list = data.frame("gene"=character(), "cluster"=character())
##     
#      for(i in 1:length(featureset)){
#        featureset.list[i,] = c(featureset[i],names(which.max(cluster.mean.expressions[,featureset[i]])))
#      }
#      
  # 2.      
  # Some silly formatting of features (when custom features are present, we group them by the cluster with their highest position (rank).)      
  #  if(features[1] != "top.gene"){
        
      featureset.list = data.frame("gene"=character(), "cluster"=character())
#      
#      for(i in 1:length(featureset)){
#        feature.located = which(sapply(1:length(clustering.results), function(k){sum(clustering.results[[k]]$gene == featureset[i])})>0)
#        featureset.list[i,] = c(
#          featureset[i],
#          names(clustering.results)[
#            feature.located[
#              which.max( 
#                sapply(feature.located, function(j){
#                  clustering.results[[j]][clustering.results[[j]]$gene == featureset[i],"avg_log2FC"]
#                })
#              ) 
#            ]
#          ]  
#        )
#      }
      # by mean expression
      for(i in 1:length(featureset)){
        featureset.list[i,] = c(featureset[i],names(which.max(cluster.mean.expressions[,featureset[i]])))
      }
      
      featureset.list[!featureset.list[,2] %in% names(clustering.results),2] = "ambiguous"
 #     featureset.list = featureset.list[order(featureset.list[,2]),]
      featureset.list = featureset.list[order(match(featureset.list[,2], clusters2use[cid.cluster$order])),]
 
      clusters.present = (clusters2use[cid.cluster$order])[(clusters2use[cid.cluster$order]) %in% featureset.list[,2]]
      if("ambiguous" %in% featureset.list[,2]){clusters.present = c(clusters.present, "ambiguous")}
    
      featureset.list = lapply(
        clusters.present, 
        function(i){
          featureset.list[featureset.list[,2] == i,1]
          data.frame("gene"=featureset.list[featureset.list[,2] == i,1],"cluster"=featureset.list[featureset.list[,2] == i,2])
        }
      )
    
 #   }

# Order the output of list into a dataframe:   
# Create dataframe of gene by clusters  
  featureset.df = data.frame("gene" = character(), "cluster" = character())
    for(i in 1:length(featureset.list)){featureset.df = rbind(featureset.df,featureset.list[[i]])}
  featureset.df = featureset.df[!duplicated(featureset.df[,1]),]; rownames(featureset.df) = 1:length(featureset.df[,1])
  featureset.df = featureset.df[!is.na(featureset.df[,1]),]
  
# Finally re-order the features by the gene ordering according to clustering results and cluster orders
#  featureset = featureset.df[,1]
  
  # order to original features if planned
  if(re.order==F){featureset.df = featureset.df[match(features, featureset.df[,1]),]}
  
  featureset = featureset.df[,1]
   
# Which clusters are present? After removing clusters that didn't ave any top / unique genes
  clusters.present = (clusters2use[cid.cluster$order])[(clusters2use[cid.cluster$order]) %in% featureset.df[,2]]
       
# now (re-)calculate the mean gene expression within clusters using cluster order and gene order
  cluster.mean.expressions = t(sapply(clusters2use[cid.cluster$order], function(k) {
        rowMeans(y[featureset.df[,1],colnames(x)[x@meta.data[,annotation] == k]])
      })) 
# And percentage of cells expressing the gene within each cluster using cluster order and gene order
  cluster.pct = t(sapply(clusters2use[cid.cluster$order], function(k) {
        rowMeans(y[featureset.df[,1],colnames(x)[x@meta.data[,annotation] == k]] > 0)
      }))
 
# matrices to plot
  mem = cluster.mean.expressions
# point size according to pct  y
  mem.cex = 0.2+cex*cluster.pct
# cluster colour according to mean expression within clusters (normalised by gene (output columns))
  mem.col =  #colorRampPalette(alt.cols)(101)[round(100*normalise(mem))+1]
                  colorRampPalette(alt.cols)(101)[sapply(1:dim(mem)[2],function(i) {round(100*normalise(mem[,i]))+1})]
#  pdf(file.path(script.dir, "figures", "x.markers.pdf"), width=14, height=6.6)

# Final clean of genes to plot dataframe
  featureset.df = featureset.df[featureset.df[,1] %in% colnames(mem),]
  
# self-made heatmap 
  if(addon==T){
    par(mar=addon.margins)
  } else {
    par(oma=c(10,7,2,6))
  }
    plot(c(1, dim(mem)[2]), c(1, dim(mem)[1]), pch=NA, xlab="", ylab="", xaxt="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0,dim(mem)[1]+1), xlim=c(0,dim(mem)[2]+1), bty="n")
     points(rep(1:dim(mem)[2], each=dim(mem)[1]),  rep(dim(mem)[1]:1,dim(mem)[2]),
      cex = mem.cex,
      col = mem.col,
      pch=16
     )
    
    axis(2, at=dim(mem)[1]:1, label = rownames(mem), las = 2, cex.axis=0.6, lwd=lwd)
    #  mtext("cluster", 2, padj=-14)
    axis(4, at=dim(mem)[1]:1, label = rownames(mem), las = 2, cex.axis=0.6, lwd=lwd)
    axis(1, at=1:dim(mem)[2], label = colnames(mem), las = 2, cex.axis=0.6, lwd=lwd)
    axis(3, at=1:dim(mem)[2], label = colnames(mem), las = 2, cex.axis=0.6, lwd=lwd)
    cluster.labels = clusters.present[clusters.present %in% featureset.df[,2]]
    if(x.labels == T){
      for(k in cluster.labels){
        axis(1, at=range(which(featureset.df[,2] == k)), tcl=0.2, labels=c("", ""), line=4, lwd=lwd)
        axis(1, at=mean(range(which(featureset.df[,2] == k))), tcl=-0.5, labels=k, line=4, las=2, lwd=lwd)
      }
    }
    mtext(title, 3, padj=-8)
   
   if(legend==T){
  gradient.legend(c(0,1), colours=alt.cols, scale=min(1,0.6*cex), 
    cex=0.7*cex, label = "Gene-scaled mean log2 expression", cex.axis=0.6, distance=distance, line=2,
    lwd=lwd, yjust=l.grad.adj, xpd=NA
  )
  legend(dim(mem)[2]*1.05*l.padj,dim(mem)[2]*0.28*l.adj, 
    legend=c(0.1, 0.33, 0.75, 1), pch=16, pt.cex=(c(0.1, 0.33, 0.75, 1)+0.2)*cex, xpd=NA, bty="n",
    ncol=1, cex=(0.6*cex), title = "Fraction in cluster"
    ) 
  }
  
  if(suppress.output==F){return(list(cid.cluster,mem))}
  }
  
  
  
##  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #    
## Meta barplots::: 
#  # Locate all genes available in our Assay for the plotting process
#    genes.available = rownames(GetAssayData(x, assay=assay.use, slot=slot.use))
#  
#  # initial feature grab without ordering  
#      featureset = unique(unlist(lapply(levels(x@meta.data[,annotation]), function(i){clustering.results[[i]]$gene[1:n]})))
#    # Re-assign features based on input argument
#      if(features[1] != "top.gene"){featureset = features}
#    # Filter out features that don't appear in matrix 
#      featureset = featureset[featureset%in%genes.available]

#  # grab the matrix that we're plotting
#    y = GetAssayData(x, assay=assay.use, slot=slot.use)[featureset,]
#    
#    
#    # initial feature grab without ordering  
##      featureset = unique(unlist(lapply(levels(x@meta.data[,annotation]), function(i){clustering.results[[i]]$gene[1:n]})))
##    # Re-assign features based on input argument
##      if(features[1] != "top.gene"){featureset = features}
##    # Filter out features that don't appear in matrix 
##      featureset = featureset[featureset%in%genes.available]

#    # Clustering of clusters by mean expression   
#      cluster.mean.expressions = t(sapply(levels(x@meta.data[,annotation]), function(k) {
#          rowMeans(y[,colnames(x)[x@meta.data[,annotation] == k]])
#        }))
#    # Ordering via clustering... We will stick with only one cluster ordering to make 
#    # comparisons easier
#      cid.cluster = hclust(dist(cluster.mean.expressions))
#      
## Top plot panel
## Dendrogram  
#  # First we have to identify the central positions of each cluster...
#    cluster.positions = data.frame(
#      "centre" = numeric(), 
#      "from"   = numeric(),
#      "to"     = numeric()
#    )
#    
#    for(k in cluster.labels){
#      cluster.positions[k,] = c(
#        mean(range(which(featureset.df[,2] == k))),
#        range(which(featureset.df[,2] == k))
#      )
#    }
#   
## The first lines we plot (bottom up approach) are the zero to leaf to first height...
#   lines(cluster.positions[,1], )
#   
#    
#   par(fig=c(0,1,0.81,1), mar=c(3,4,1,0), oma=c(4,1,1,15), cex=0.7); 
#   test = as.dendrogram(cid.cluster)
#   
#   plot(c(1,length(featureset)), c(1,max(cid.cluster$height)), pch=NA)
#   cid.cluster$height
#   cid.cluster$order
#   cid.cluster$height[cid.cluster$order]
# 
#pdf("test.pdf", height=25, width=14)   
#   par(fig=c(0,1,0.92,1), mar=c(3,4,1,0), oma=c(4,1,1,15), cex=1); 
#    plot(as.dendrogram(cid.cluster), axes = T, xaxs = "r", center=F, cex.axis=0.7)
#    
#  
## Second panel
## self-made heatmap / dotplot
#  par(new=T, fig=c(0,1,0.78,0.9), mar=c(3,5.2,1,1.5))
#    plot(c(1, dim(mem)[2]), c(1, dim(mem)[1]), pch=NA, xlab="", ylab="cluster", xaxt="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0,dim(mem)[1]+1), xlim=c(0,dim(mem)[2]+1), bty="n")
#     points(rep(1:dim(mem)[2], each=dim(mem)[1]),  rep(dim(mem)[1]:1,dim(mem)[2]),
#      cex = mem.cex,
#      col = mem.col,
#      pch=16
#     )
#    
#    axis(2, at=dim(mem)[1]:1, label = rownames(mem), las = 2, cex.axis=0.6)
#    axis(4, at=dim(mem)[1]:1, label = rownames(mem), las = 2, cex.axis=0.6)
#    axis(1, at=1:dim(mem)[2], label = colnames(mem), las = 2, cex.axis=0.6)
#    axis(3, at=1:dim(mem)[2], label = colnames(mem), las = 2, cex.axis=0.6)
#    cluster.labels = clusters.present[clusters.present %in% featureset.df[,2]]
#    for(k in cluster.labels){
#      axis(1, at=range(which(featureset.df[,2] == k)), tcl=0.2, labels=c("", ""), line=4.2)
#      axis(1, at=mean(range(which(featureset.df[,2] == k))), tcl=-0.5, labels=k, line=4.2, las=2)
#    }
#    for(k in cluster.labels){
#      axis(3, at=range(which(featureset.df[,2] == k)), tcl=0.2, labels=c("", ""), line=4.2)
#     axis(3, at=mean(range(which(featureset.df[,2] == k))), tcl=-0.5, labels="", line=4.2, las=2)
#    }
#    
#  #  mtext("Unique top gene markers", 3, padj=-8)
#  
#  gradient.legend(c(0,1), colours=alt.cols, scale=0.7*cex, cex=0.7*cex, label = "Gene-scaled mean log2 expression", cex.axis=0.6, distance=0.25)
#  legend(dim(mem)[2]*1.05,dim(mem)[2]*0.28, 
#    legend=c(0.1, 0.33, 0.75, 1), pch=16, pt.cex=(c(0.1, 0.33, 0.75, 1)+0.2)*cex, xpd=NA, bty="n",
#    ncol=1, cex=(0.6*cex), title = "Fraction in cluster"
#  )
#  
#  
## 1. NBC results
## HCC.nbc var
#     # var1
#        var1 = as.factor(x$HCC)
#      # var2
#        var2 = Idents(x)
#        
#    cell.id.matrix <- matrix(0, length(levels(var1)), length(levels(var2)))
#    rownames(cell.id.matrix) <- levels(var1)
#    colnames(cell.id.matrix) <- levels(var2)
#    # adult pre
#    for(i in levels(var2)){
#      cell.id.matrix[names(table(var1[var2==i])),i] <- table(var1[var2==i])
#    }
#   
#par(new=T, fig=c(0,1,0.65,0.75), mar=c(1,4,0,0)); barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], 
#                                                        col=Discrete12, las=3, ylab="% of cells", xaxt="n", border=NA, cex.axis=0.7)
#   legend.4(legend=levels(var1), fill=Discrete12, bty="n", cex=0.8, y.inters= 0.8)    
#  
#  

##  2. Condition
#    # var1
#        var1 = as.factor(x$condition)
#      # var2
#        var2 = Idents(x)
#        
#    cell.id.matrix <- matrix(0, length(levels(var1)), length(levels(var2)))
#    rownames(cell.id.matrix) <- levels(var1)
#    colnames(cell.id.matrix) <- levels(var2)
#    # adult pre
#    for(i in levels(var2)){
#      cell.id.matrix[names(table(var1[var2==i])),i] <- table(var1[var2==i])
#    }
##    barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], col=c("grey",Discrete12[c(1,4,8,6,12,3,10,5,11)]), las=3, ylab="% of cells", xaxt="n")
#   par(new=T, fig=c(0,1,0.55,0.65), mar=c(1,4,0,0)); 
#    barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], 
#      col=c(epi.blue, epicm.purple, cm.red, nccm.orange), 
#      las=3, 
#      ylab="% of cells", 
#      xaxt="n", 
#      border=NA, 
#      cex.axis=0.7
#    )

#   legend.4(legend=levels(var1), fill=c(epi.blue, epicm.purple, cm.red, nccm.orange), bty="n", cex=0.8, y.inters= 0.8)   

## 3. Sample

#    # var1
#        var1 = as.factor(x$sample)
#      # var2
#        var2 = Idents(x)
#        
#    cell.id.matrix <- matrix(0, length(levels(var1)), length(levels(var2)))
#    rownames(cell.id.matrix) <- levels(var1)
#    colnames(cell.id.matrix) <- levels(var2)
#    # adult pre
#    for(i in levels(var2)){
#      cell.id.matrix[names(table(var1[var2==i])),i] <- table(var1[var2==i])
#    }
##    barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], col=c("grey",Discrete12[c(1,4,8,6,12,3,10,5,11)]), las=3, ylab="% of cells", xaxt="n")
#   par(new=T, fig=c(0,1,0.45,0.55), mar=c(1,4,0,0)); 
#    barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], 
#      col=Discrete12[c(1,2,9,10,5,6,7,8)], 
#      las=3, 
#      ylab="% of cells", 
#      xaxt="n", 
#      border=NA, 
#      cex.axis=0.7
#    )

#   legend.4(legend=levels(var1), fill=Discrete12[c(1,2,9,10,5,6,7,8)], bty="n", cex=0.8, y.inters= 0.8)   

#  
#dev.off()



##  1 NBC - plot the barplot for cell.id.matrix
#  par(new=T, fig=c(0,1,0.63,0.84), mar=c(1,4,0,0)); barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], 
#                                                        col=Discrete12, las=3, ylab="% of cells", xaxt="n", border=NA, cex.axis=0.7)
#   legend.4(legend=levels(all.subsample.integrated$HCC.nbc), fill=Discrete12, bty="n", cex=0.8, y.inters= 0.8)
## Repeat for other categoricals  
# 
### Stage 
##    # var1
##        var1 = as.factor(all.subsample.integrated$stage)
##      # var2
##        var2 = Idents(all.subsample.integrated)
##        
##    cell.id.matrix <- matrix(0, length(levels(var1)), length(levels(var2)))
##    rownames(cell.id.matrix) <- levels(var1)
##    colnames(cell.id.matrix) <- levels(var2)
##    # adult pre
##    for(i in levels(var2)){
##      cell.id.matrix[names(table(var1[var2==i])),i] <- table(var1[var2==i])
##    }
###    barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], col=c("grey",Discrete12[c(1,4,8,6,12,3,10,5,11)]), las=3, ylab="% of cells", xaxt="n")
##   par(new=T, fig=c(0,1,0.575,0.655), mar=c(1,4,0,0)); barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], 
##                                                        col=Discrete, las=3, ylab="% of cells", xaxt="n", border=NA)
##   legend.4(legend=levels(factor(all.subsample.integrated$stage)), fill=Discrete, bty="n", cex=0.6, y.inters= 0.8)   

##  2 Cell.type (adult HCA classification)  
#    # var1
#        var1 = as.factor(all.subsample.integrated$cell.type)
#      # var2
#        var2 = Idents(all.subsample.integrated)
#        
#    cell.id.matrix <- matrix(0, length(levels(var1)), length(levels(var2)))
#    rownames(cell.id.matrix) <- levels(var1)
#    colnames(cell.id.matrix) <- levels(var2)
#    # adult pre
#    for(i in levels(var2)){
#      cell.id.matrix[names(table(var1[var2==i])),i] <- table(var1[var2==i])
#    }
##    barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], col=c("grey",Discrete12[c(1,4,8,6,12,3,10,5,11)]), las=3, ylab="% of cells", xaxt="n")
#   par(new=T, fig=c(0,1,0.42,0.63), mar=c(1,4,0,0)); barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], 
#                                                        col=c("grey",Discrete12[c(1,4,8,6,12,3,10,5,11)]), las=3, ylab="% of cells", xaxt="n", border=NA, cex.axis=0.7)
#   legend.4(legend=levels(factor(all.subsample.integrated$cell.type)), fill=c("grey",Discrete12[c(1,4,8,6,12,3,10,5,11)]), bty="n", cex=0.8, y.inters= 0.8)   

## 3
## Cell.state (adult HCA high-resolution classification)  
#    # var1
# #       var1 = as.factor(all.subsample.integrated$cell.states)
#        var1 = factor(all.subsample.integrated$cell.states[all.subsample.integrated$stage!="foetal"])
#      # var2
#        var2 = Idents(all.subsample.integrated)[all.subsample.integrated$stage!="foetal"]
#        
#    cell.id.matrix <- matrix(0, length(levels(var1)), length(levels(var2)))
#    rownames(cell.id.matrix) <- levels(var1)
#    colnames(cell.id.matrix) <- levels(var2)
#    # adult pre
#    for(i in levels(var2)){
#      cell.id.matrix[names(table(var1[var2==i])),i] <- table(var1[var2==i])
#    }
##    barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], col=c("grey",Discrete12[c(1,4,8,6,12,3,10,5,11)]), las=3, ylab="% of cells", xaxt="n")
#  par(new=T, fig=c(0,1,0.21,0.42), mar=c(1,4,0,0)); barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], 
#                                                        col=c(Discrete.U,Discrete.U), las=3, ylab="% of adult cells", xaxt="n", border=NA, cex.axis=0.7) 
#   legend.4(legend=levels(var1), fill=c(Discrete.U,Discrete.U), bty="n", ncol=3, cex=0.6, y.inters= 0.8) 
# 
## 4 Donors
## Donor
#    # var1
# #       var1 = as.factor(all.subsample.integrated$cell.states)
#        var1 = as.factor(all.subsample.integrated$donor)
#      # var2
#        var2 = Idents(all.subsample.integrated)
#        
#    cell.id.matrix <- matrix(0, length(levels(var1)), length(levels(var2)))
#    rownames(cell.id.matrix) <- levels(var1)
#    colnames(cell.id.matrix) <- levels(var2)
#    # adult pre
#    for(i in levels(var2)){
#      cell.id.matrix[names(table(var1[var2==i])),i] <- table(var1[var2==i])
#    }
##    barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], col=c("grey",Discrete12[c(1,4,8,6,12,3,10,5,11)]), las=3, ylab="% of cells", xaxt="n")
#  par(new=T, fig=c(0,1,0,0.21), mar=c(1,4,0,0)); barplot((100*t(t(cell.id.matrix)/colSums(cell.id.matrix)))[,cid.cluster$order], 
#                                                        col=c(colorRampPalette(Earth[1:4])(5),colorRampPalette(Earth[5:9])(7)), las=3, ylab="% of cells", border=NA, cex.axis=0.7) 
#   legend.4(legend=levels(var1), fill=c(colorRampPalette(Earth[1:4])(5),colorRampPalette(Earth[5:9])(7)), bty="n", ncol=1, cex=0.8, y.inters= 0.8) 
# 
# 
# 


##  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #   
#   
#  
#  
#  boxplot(y~x,
#    data.frame(
#    "x" = x@meta.data[,annotation],
#    "y" = GetAssayData(x, assay=assay.use, slot=slot.use)["TNNT1",]
#    )
#  ) 
#  
#  boxplot(y~x,
#    data.frame(
#    "x" = x@meta.data[,annotation],
#    "y" = GetAssayData(x, assay="RNA", slot="data")["TNNT1",]
#    )
#  ) 
#  
#  clustering.results[["10"]]["TNNT1",]
#  clustering.results[["20"]]["TNNT1",]
#  
#  
#  x$allvs20 = c("all", "20")[(x$seurat_clusters =="20")+1]
#  
#  Idents(x) = x$allvs20
#  x20.markers = FindAllMarkers(x)
#  x20.markers2 = FindAllMarkers(x, slot="counts")
#  
#  x20.markers2[x20.markers2$gene=="TNNT1",]
#  x20.markers[x20.markers$gene=="TNNT1",]
#  exp.all.cut.integrated.sct.markers[exp.all.cut.integrated.sct.markers$gene=="TNNT1",]
#  
#  z = exp.all.cut.integrated.sct
#  z$allvs20 = c("all", "20")[(z$seurat_clusters =="20")+1]
#  Idents(z) = z$allvs20

#  z20.markers = FindAllMarkers(z)
#  z20.markers2 = FindAllMarkers(z, slot="counts")
#  z20.markers[z20.markers$gene=="TNNT1",]
#  z20.markers2[z20.markers2$gene=="TNNT1",]
#  
#  y = exp.all.cut.integrated.sct
#  y = NormalizeData(y)
#  y$allvs20 = c("all", "20")[(y$seurat_clusters =="20")+1]
#  Idents(y) = y$allvs20

#  y20.markers = FindAllMarkers(y)
#  y20.markers2 = FindAllMarkers(y, slot="counts")
#  y20.markers[y20.markers$gene=="TNNT1",]
#  y20.markers2[y20.markers2$gene=="TNNT1",]
# 

#  boxplot(y~x,
#    data.frame(
#    "x" = x@meta.data[,"allvs20"],
#    "y" = GetAssayData(x, assay="RNA", slot="counts")["TNNT1",]
#    )
#  ) 

#  dev.new()
#    boxplot(y~x,
#    data.frame(
#    "x" = y@meta.data[,"allvs20"],
#    "y" = GetAssayData(y, assay="RNA", slot="counts")["TNNT1",]
#    )
#  ) 

#  dev.new()
#    boxplot(y~x,
#    data.frame(
#    "x" = z@meta.data[,"allvs20"],
#    "y" = GetAssayData(z, assay="RNA", slot="data")["TNNT1",]
#    )
#  ) 


#  clustering.results = extract.top(exp.all.cut.integrated.sct.markers,500)
#  
#  xx = exp.all.cut.integrated.sct.markers
#  
#  clustering.results[["10"]]["TNNT1",]
#  clustering.results[["20"]]["TNNT1",]
#  
#  
#  
#  
#  
#  boxplot(y~x,
#    data.frame(
#    "x" = x@meta.data[,annotation],
#    "y" = GetAssayData(x, assay="RNA", slot="counts")["NUF2",]
#    )
#  ) 

  
    
