 # Some matrices, cellranger v3 made for example, appear to have duplicated features HGNC symbols. This function sums them up and spits out a new matrix.

 aggregate.duplicate.genes <- function(Genes_Cells_matrix){
    # And... removing duplicated features (how on earth does this happen????)
      # Not the most efficient of code... whatever.
      genes <- rownames(Genes_Cells_matrix)
      if(sum(duplicated(genes)) > 0){
        dup.features <- genes[duplicated(genes)][order(genes[duplicated(genes)])]
        
        dup.features.names <- genes[genes%in%dup.features][order(genes[genes%in%dup.features])]
        dup.features.all <- which(genes%in%dup.features)[order(genes[genes%in%dup.features])]
        
        dup.features.keep <- dup.features.all[which(!duplicated(dup.features.names))]
        dup.features.keep.names <- dup.features.names[which(!duplicated(dup.features.names))]
        dup.features.delete <- dup.features.all[which(duplicated(dup.features.names))]
              
     # Quick loop to sum up the duplicated gene features    
        for(i in 1:length(dup.features.keep)){
          Genes_Cells_matrix[dup.features.keep[i],] = colSums(Genes_Cells_matrix[genes == dup.features.keep.names[i],,drop=F])
        }
      
    # Remove the rows we summed the counts from    
      Genes_Cells_matrix <- Genes_Cells_matrix[-dup.features.delete,]
      }
    return(Genes_Cells_matrix)
  }
  
