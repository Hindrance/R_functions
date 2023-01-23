# A simple function to extract top markers from the results of Seurat function "FindAllMarkers()"
# The result is a cluster-named list of top markers, either up, down or 
   
   
   
# Function and arguments   
  extract.top = function(x, n=100, alpha = 1e-10, l2fc.cut=0.5, direction = "both"){
    
    


  # ADULT DEGS
    markers.cut <- x[x$p_val_adj < alpha,]


  # Run function for up
    if(direction == "up"){
      top.gene = lapply(levels(markers.cut$cluster), function(k) {
          head(markers.cut[markers.cut$cluster==k,][markers.cut[markers.cut$cluster==k,"avg_log2FC"] > l2fc.cut,],n)
      }); names(top.gene) = levels(markers.cut$cluster)
    }

  # Run function for both up and down...
    if(direction == "down"){
      top.gene = lapply(levels(markers.cut$cluster), function(k) {
          head(markers.cut[markers.cut$cluster==k,][markers.cut[markers.cut$cluster==k,"avg_log2FC"] < -l2fc.cut,],n)
      }); names(top.gene) = levels(markers.cut$cluster)
    }

  # Run function for both up and down...
    if(direction == "both"){
      top.gene = lapply(levels(markers.cut$cluster), function(k) {
        rbind(
          head(markers.cut[markers.cut$cluster==k,][markers.cut[markers.cut$cluster==k,"avg_log2FC"] > l2fc.cut,],n),
          head(markers.cut[markers.cut$cluster==k,][markers.cut[markers.cut$cluster==k,"avg_log2FC"] < -l2fc.cut,],n)
        )
      }); names(top.gene) = levels(markers.cut$cluster)
    }
  # output
    return(top.gene)
  }
  
  
  
