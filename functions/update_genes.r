# Updating genes lists to biomaRt most recent reference

 update.genes <- function(old.rownames, df=F){
    if(!exists("mart")){
    cat("Accessing biomaRt")
      mart <<- useMart("ensembl",  "hsapiens_gene_ensembl")
    }
    
    if(!exists("label.df")){
    cat("downloading gene sysonyms lists")
      label.df <<- getBM(mart=mart, attributes=c("ensembl_gene_id", "external_gene_name", "hgnc_symbol", "chromosome_name", "external_synonym"), useCache=F)
    }
 
     map.df.3 <- data.frame("query.v" = old.rownames,
      # match with external synonyms - write as gene symbol
        "ext" = label.df[match(old.rownames, label.df[,"external_synonym"]),"external_gene_name"], 
      # match with external gene name (HGNC usually) - write as gene symbol
        "symbol" = label.df[match(old.rownames, label.df[,"external_gene_name"]),"external_gene_name"],
      # match with external synonyms - write as gene symbol
        "ext_ensemblID" = label.df[match(old.rownames, label.df[,"external_synonym"]),"ensembl_gene_id"], 
      # match with gene symbol... - write as ensemblID
        "symbol_ensemblID" = label.df[match(old.rownames, label.df[,"external_gene_name"]),"ensembl_gene_id"],
        stringsAsFactors=F
      )
     
      # create new column of ensemblID
        map.df.3[,"new.ensemblIDs"] = map.df.3[,"symbol_ensemblID"]
      # Where new.ensemblID == NA, replace with external synonym mapping gene...
        map.df.3[is.na(map.df.3[,"new.ensemblIDs"]),"new.ensemblIDs"] = map.df.3[is.na(map.df.3[,"new.ensemblIDs"]),"ext_ensemblID"]
      # map new ensemblIDs along to the MART's gene symbol
        map.df.3[,"new.rownames"] = label.df[match(map.df.3[,"new.ensemblIDs"], label.df[,"ensembl_gene_id"]),"external_gene_name"]
      # replace new rownames that are NA with old rownames... 
        map.df.3[is.na(map.df.3[,"new.rownames"]),"new.rownames"] = map.df.3[is.na(map.df.3[,"new.rownames"]),"query.v"] 

    if(df == F){
      return(map.df.3[,"new.rownames"])
    } else {
      return(map.df.3)
    }  
  }
  
