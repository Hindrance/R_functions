 
 
 
# clusters - the cluster labels - with the groups shared within each cluster k
# classes - the vector of class / sample labels (not the levels), or groups shared within clusters
# names2sample - the vector of sample names, essentially column names of a [genes, cells] matrix
# n - the number of samples to take from each cluster in total... typically it could be the smallest cluster size

  cluster_sub_sample <- function(clusters, classes, names2sample, N){
   
   class.cluster.table = sapply(levels(clusters), function(K){table(classes[clusters==K])})
#   classes = as.character(classes)
   set.seed(1)
   rm(.Random.seed, envir=globalenv())
   
   
  # smallest cluster = n
    samples.clusters = lapply(levels(clusters), function(K) { 
  #  for each cluster...
   # Sample the minimum of N / class samples                     
     sample.loop = sample.equal(# SAMPLE FROM the cells in that cluster K
         vector = names2sample[clusters == K], 
       # classes associated with cells in cluster == K
         classes = classes[clusters == K],
       # Sample the minimum of the minimum cluster size / number of classes in each K cluster
         n = min(round(N/length(levels(classes[clusters == K]))), min(class.cluster.table[,K]))
     )
      # OMIT the names already sampled (a hacky "without replacement")
       sample.omit = !names2sample[clusters == K] %in%  sample.loop
       
     # RESAMPLE to make up to ~ N
     while(
           # Total clusters                       >  # Clusters remaining after omitting already sampled samples
           length(unique(classes[clusters == K])) >= length(unique(classes[clusters == K][sample.omit])) & 
           length(sample.loop) < N &
           sum(sample.omit) > 0   #   FINISH LOOP AND RETURN IF CLUSTER K HAS ALL BEEN SAMPLED
           ){
         # New TOTAL number to sample...
          new.target = N - length(sample.loop) 
         # Sample the minimum length, adding onto our sample.loop vector                  
          sample.loop = c(sample.loop, 
          
            sample.equal(
         # names2sample, omitting the already sampled samples
           names2sample[clusters == K][sample.omit], 
         # classes associated with cells in cluster == K
           classes = as.factor(as.character(classes[clusters == K][sample.omit])),
         # Sample the minimum of the minimum cluster size / number of classes in each K cluster
           n = min(
              ceiling(new.target/length(unique(classes[clusters == K][sample.omit]))), 
              min(table(as.factor(as.character(classes[clusters == K][sample.omit]))))
              )
            ) 
          )
          # discard sampled items again, and repeat loop if conditions are still met...
           sample.omit = !(names2sample[clusters == K] %in%  sample.loop)
           #table(as.character(classes[clusters == K][sample.omit]))
           
     }
     
   # Remove duplicated items - This occurs if sample number chosen is above the number of samples in cluster and the class does not round into N
   return(sample.loop)   
  }
  )

  return(unlist(samples.clusters))
  }
  
  

 
 
# 
## clusters - the cluster labels - with the groups shared within each cluster k
## classes - the vector of class / sample labels (not the levels), or groups shared within clusters
## names2sample - the vector of sample names, essentially column names of a [genes, cells] matrix
## n - the number of samples to take from each cluster in total... typically it could be the smallest cluster size


#  cluster_sub_sample <- function(clusters, classes, names2sample, n){
#   
#   class.cluster.table = sapply(levels(clusters), function(k){table(classes[clusters==k])})
##   classes = as.character(classes)
#   
#   rm(.Random.seed, envir=globalenv())
#   
#   
#  # smallest cluster = n
#    samples.clusters = lapply(levels(clusters), function(k) { 
#  #  for each cluster...
#   # Sample the minimum of n / class samples                     
#     sample.loop = sample.equal(# SAMPLE FROM the cells in that cluster k
#      vector = names2sample[clusters == k], 
#        # classes associated with cells in cluster == k
#      classes = as.factor(classes[clusters == k]),
#         # Sample the minimum of the minimum cluster size / number of classes in each K cluster
#     n = min(round(n/length(levels(as.factor(classes[clusters == k])))), min(class.cluster.table[,k])))
#    # RESAMPLE to make up to ~ n
#     sample.omit = !names2sample[clusters == k] %in%  sample.loop
# 
#     while(
#           length(levels(as.factor(classes[clusters == k]))) > length(levels(as.factor(classes[clusters == k][sample.omit]))) & 
#           length(sample.loop) < n &
#           sum(sample.omit) > 0   #   FINISH LOOP AND RETURN IF CLUSTER k HAS ALL BEEN SAMPLED
#           ){
#          new.target = n - length(sample.loop)    
#          # Sample the minimum length                      
#             sample.loop = c(sample.loop,sample.equal(# SAMPLE FROM the cells in that cluster k
#      names2sample[clusters == k][sample.omit], 
#        # classes associated with cells in cluster == k
#      as.factor(classes[clusters == k][sample.omit]),
#        # Sample the minimum of the minimum cluster size / number of classes in each K cluster
#     min(
#        ceiling(new.target/length(levels(as.factor(classes[clusters == k][sample.omit])))), 
#        min(table(as.factor(classes[clusters == k][sample.omit])))
#        )
#        )
#        )
#          # discard sampled items
#           sample.omit = !names2sample[clusters == k] %in%  sample.loop
#     }
#   return(sample.loop)   
#  }
#  )

#  return(unlist(samples.clusters))
#  }
#  
#  
   
    
 
