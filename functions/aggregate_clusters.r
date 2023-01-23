  
# a = the class vector of length samples.
# b = the matrix of [variable,cluster] to run cluster correlations on
# c = the correlation cut off (join clusters together if sharing one cluster correlation of > this value
# method = "meta" IS A WEIRD VERSION OF THIS, PREFER TRUE FOR MERGING ALL WITH > CUT

 
  
# Correlation   ##  
# function stuff
     aggregate.clusters <- function(clusters2reduce, clusters.matrix, cor.cut=0.9, method="true"){
       correlation.matrix = cor(clusters.matrix)
       cor.clusters = lapply(colnames(correlation.matrix), function(i) {names(which(correlation.matrix[,i] > cor.cut))})
#      locate meta clusters - inclusive (meta clusters will be grouped with other meta clusters when the adjoining meta cluster does not 
#      explicitly contain ALL other clusters in the meta cluster.

      if(method == "true"){
 # LOOP METHOD      
      cor.clusters = lapply(colnames(correlation.matrix), function(i) {names(which(correlation.matrix[,i] > cor.cut))})
      for(j in 1:length(cor.clusters)){
          k2go = which(sapply(1:length(cor.clusters), function(i){sum(cor.clusters[[j]] %in% cor.clusters[[i]]) > 0}))
        for(k in k2go){
          cor.clusters[[k]] = sort(
            unique(
              unlist(
                    sapply(k2go, function(z){cor.clusters[[z]]})
             )
            )
          )
        }
      }
    
      new.levels = sapply(1:length(cor.clusters), function(i){paste(sort(unique(cor.clusters[[i]])), collapse="_")})
    }
      
   if(method == "meta"){
#  # METACLUSTER METHOD:      
       metacluster.matrix = sapply(1:length(cor.clusters), function(j) {
            sapply(1:length(cor.clusters), function(i){sum(cor.clusters[[j]] %in% cor.clusters[[i]]) > 0})
       })
    # RENAME to include the cluster names "_"
       colnames(metacluster.matrix) = sapply(1:ncol(metacluster.matrix), function(i) {
        paste(colnames(clusters.matrix)[which(metacluster.matrix[,i])], collapse="_")
       })
      levels(clusters2reduce) = colnames(metacluster.matrix)
     new.levels = colnames(metacluster.matrix)
     }
##    # Random multiplier for colSums... easy right? pragmatic approach
##      ranMult = round(runif(length(metacluster.matrix[,1]), 0, 10),6)
##        while(sum(duplicated(ranMult))>1){
##          ranMult = round(runif(length(metacluster.matrix[,1]), 0, 10),6)
##        }
##     
##    # levels(clusters2reduce) = colSums(metacluster.matrix * ranMult)
##    # levels(clusters2reduce) = order(unique(colSums(metacluster.matrix * ranMult)))
##      
     levels(clusters2reduce) = new.levels
     return(clusters2reduce)
     
    }
  
###    
    
