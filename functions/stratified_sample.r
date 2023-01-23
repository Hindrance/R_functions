 
######################################
#### Basic stratified subsampling ####
######################################

#  This function was developed to evenly sample cells from an unique ID vector across two different factors or stratifications (for example, 1) "cluster ID" and 2) "donor ID"),
#  and produce a new evenly distributed but smaller unique ID vector. 


# required function

######################################################
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  

# For the case of simple stratifications, we can combine factors into one factor for sampling, this is efficient and simple.
# Function
# ID_vector is unique cells
# class_factor is the classes to sample from
# n is the number from each level to sample
# override means that samples from levels will be taken even if the minimum level size is less than n.
#    otherwise this function will sample evenly at the minimum level size
  sample.equal = function(ID_vector, class_factor, n, override = F){
    n2sub <-  min(table(class_factor))
    if(n < n2sub){a = n} else {a = n2sub}
    
    equal.sample.vector <- unlist(lapply(levels(class_factor), function(k) {
        if(override==T){sample(ID_vector[class_factor==k], min(c(length(ID_vector[class_factor==k]), n)), replace=F)} else {
          sample(ID_vector[class_factor==k], a, replace=F)
        }
      }))
    
    return(equal.sample.vector)  
  }


#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
######################################################


######################################################
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  
# complex function
# Arguments:

# clusters - the cluster labels - with the groups shared within each cluster k
# donors - the vector of class / sample labels (not the levels), or groups shared within clusters
# cell_IDs - the vector of sample names, essentially column names of a [genes, cells] matrix
# N - the number of samples to take from each cluster in total... typically it could be the smallest cluster size
# for generalisation, I call clusters and donors f1 and f2 (factor 1 and factor 2)

  stratified_subsample <- function(f1, f2, cell_IDs, N, seed=1){
   
   ### For scRNAseq dataset, the principle was to reduce clusters to equal sizes, equally distributed by donors... 
   # f1 == clusters
    clusters = f1
   # f2 = donors
    donors = f2
   
   class.cluster.table = sapply(levels(clusters), function(K){table(donors[clusters==K])})

# re-seed (resetting the Seurat global seed fix in analysis)
   set.seed(1)
   rm(.Random.seed, envir=globalenv())
   set.seed(seed)
   
  # smallest cluster = n
    samples.clusters = lapply(levels(clusters), function(K) { 
  #  for each cluster...
   # Sample the minimum of N / class samples                     
     sample.loop = sample.equal(# SAMPLE FROM the cells in that cluster K
         ID_vector = cell_IDs[clusters == K], 
       # donors associated with cells in cluster == K
         class_factor = donors[clusters == K],
       # Sample the minimum of the minimum cluster size / number of donors in each K cluster
         n = min(round(N/length(levels(donors[clusters == K]))), min(class.cluster.table[,K]))
     )
      # OMIT the names already sampled (a hacky "without replacement")
       sample.omit = !cell_IDs[clusters == K] %in%  sample.loop
       
     # RESAMPLE to make up to ~ N
     while(
           # Total clusters  >  # Clusters remaining after omitting already sampled samples
           length(unique(donors[clusters == K])) >= length(unique(donors[clusters == K][sample.omit])) & 
           length(sample.loop) < N &
           sum(sample.omit) > 0   # FINISH LOOP AND RETURN IF CLUSTER K HAS ALL BEEN SAMPLED
           ){
         # New TOTAL number to sample...
          new.target = N - length(sample.loop) 
         # Sample the minimum length, adding onto our sample.loop vector                  
          sample.loop = c(sample.loop, 
          
            sample.equal(
         # cell_IDs, omitting the already sampled samples
           ID_vector = cell_IDs[clusters == K][sample.omit], 
         # donors associated with cells in cluster == K
           class_factor = as.factor(as.character(donors[clusters == K][sample.omit])),
         # Sample the minimum of the minimum cluster size / number of donors in each K cluster
           n = min(
              ceiling(new.target/length(unique(donors[clusters == K][sample.omit]))), 
              min(table(as.factor(as.character(donors[clusters == K][sample.omit]))))
              )
            ) 
          )
          # discard sampled items again, and repeat loop if conditions are still met...
           sample.omit = !(cell_IDs[clusters == K] %in%  sample.loop)
           
     }
     
   # Remove duplicated items - This occurs if sample number chosen is above the number of samples in cluster and the class does not round into N
   return(sample.loop)   
  }
  )

  return(unlist(samples.clusters))
  }
  
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
######################################################  
 
