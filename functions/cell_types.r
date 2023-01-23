######################################################
######################################################
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

 #  This is a script hoping to identify potential cell types.
 #  Basically, an effort to collate evidence-based identifying markers of selected cell types. 
 #  I will start, perhaps, with the ones that we're interested in.
 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##  
######################################################
######################################################


# Search for genes in ALEXSC
# ALEXSC_SEARCH_GENES
#AlexSC_RDS <- readRDS('/home/vincent/Documents/Projects/D_AlexSC/output/v_1/clustering/results/DE_cluster_results.rds')
#AlexSC_TSNE <- read.csv('/home/vincent/Documents/Projects/D_AlexSC/output/v_1/tsne/data/tsneIterationsRes_KOEACBPS_83145396.csv')
#AlexSC_CLUSTER <- readRDS('/home/vincent/Documents/Projects/D_AlexSC/rf_optim_results_HDBDSCAN_minPts_10.rds')
  asc.search <- function(gene = "GAPDH"){
    asc.res <- as.data.frame(sapply(1:length(AlexSC_RDS), function(i){AlexSC_RDS[[i]][AlexSC_RDS[[i]][,"gene"] == gene,]}))
    p.val.cols <- paste("grey",sep="",sapply(1:length(asc.res), function(i){which.min(abs(log10(seq.log.basic(1e-5,1, length=100))-log10(asc.res[3,i][[1]])))}))
    par(mfrow=c(2,1))
    barplot(unlist(asc.res[2,]), col=p.val.cols, names=paste("cluster", 0:7, "\nP=", signif(unlist(asc.res[3,]),2)), ylab="log2FC",ylim=range(unlist(asc.res[2,]))*1.1)
    plot(AlexSC_TSNE)
  }


# EPICARDIAL GENES (mixed mice and human?) Need to assign a weighting for each gene... i.e - the predictive power?
###################################################### 
  sig.epi.genes <- c(
    "WT1",      # ALEXSC    10.1038/s41586-019-1414-x   10.1186/1423-0127-18-67   10.1242/dev.119271
    "PODXL",    # ALEXSC    
    "TBX18",    # ALEXSC    10.1038/s41586-019-1414-x   10.1186/1423-0127-18-67   10.1242/dev.119271
    "TCF21",    # ALEXSC    10.1242/dev.119271
    "RARRES2",  # ALEXSC    
    "ALDH1A2",  # ALEXSC    10.1242/dev.067041
    "SFRP1",    # 10.1038/s41586-019-1414-x
    "SFRP5",    # ALEXSC    
    "UPK3B",    # 10.1038/s41586-019-1414-x
    "SPARC",    # 10.1038/s41586-019-1414-x
    "BNC1",     #
    "BNC2",     # ALEXSC
    "THY1",     
    "GATA6",    
    "KRT19",    
    "KRT8",     
    "UPK1B"   
  )

  # EPICARDIAL GENES but Lineage A
    sig.epi.A.genes <- c(
      "WT1",      # ALEXSC    10.1038/s41586-019-1414-x   10.1186/1423-0127-18-67   10.1242/dev.119271
      "PODXL",    # ALEXSC    
      "TBX18",    # ALEXSC    10.1038/s41586-019-1414-x   10.1186/1423-0127-18-67   10.1242/dev.119271
      "TCF21",    # ALEXSC    10.1242/dev.119271
      "RARRES2",  # ALEXSC    
      "ALDH1A2",  # ALEXSC    
      "SFRP1",    # 10.1038/s41586-019-1414-x
      "SFRP5",    # ALEXSC    
      "UPK3B",    # 10.1038/s41586-019-1414-x
      "SPARC",    # 10.1038/s41586-019-1414-x
      "BNC1"      # 
    )
    
   # EPICARDIAL GENES but Lineage B 
    sig.epi.B.genes <- c(
      "WT1",      # ALEXSC    10.1038/s41586-019-1414-x   10.1186/1423-0127-18-67   10.1242/dev.119271
      "PODXL",    # ALEXSC
      "TBX18",    # ALEXSC    10.1038/s41586-019-1414-x   10.1186/1423-0127-18-67   10.1242/dev.119271
      "TCF21",    # ALEXSC    10.1242/dev.119271
      "RARRES2",  # ALEXSC
      "ALDH1A2",  # ALEXSC
      "SFRP1",    # 10.1038/s41586-019-1414-x
      "SFRP5",    # ALEXSC
      "UPK3B",    # 10.1038/s41586-019-1414-x
      "SPARC",    # 10.1038/s41586-019-1414-x
      "BNC1"      # 
    
    )
  
  sig.cardiacFibroblast.genes <- c(
    "POSTN",      # 10.1161/CIRCRESAHA.119.315491   
    "DDR2",       # 10.1161/CIRCRESAHA.119.315491   
    "VIM",        # 10.1161/CIRCRESAHA.119.315491   
    "COL1A1",     # 10.1161/CIRCRESAHA.119.315491   
    "TCF21"       # 10.1038/s41467-017-00840-w      
  )
  
#  sig.cardiac
  
  wnt.markers <- c(
    "",
  )
  
  bmp.markers <- c(
    "",
  )
  
  ret.markers <- c(
    "",
  )
  
  
  
