# rapid ens2hgnc conversions using cellranger output features.tsv files

  g <- function(gene, conversion.table = "/mnt/LocalHDD/raw_data/N_Foetal_Heart/Mummery_data/MT2/filtered_feature_bc_matrix/features.tsv"){
    
    if(!exists("ens2gene")){
      ens2gene <<- read.csv(conversion.table, sep="\t", head=F)
    }
    
    a = ens2gene[,1:2]
    
    if(length(grep("ENS", gene)) > 0){
      c = a[a[,1]==gene,2]
    } else {
      c = a[a[,2]==gene,1]
    
    }
   return(c)
  }
