
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##   
# Model?


# How do we generate a KDE model?
# Generate KDEs
  generate.kdes <- function(input.data, classes, mc.cores=8, ...){
    res = mclapply(rownames(input.data), function(feature) {
        plot.density(input.data[feature,], classes, PDF=T, report=T, ...)
      }, mc.cores=mc.cores
    )
    names(res) = rownames(input.data)
    return(res)
  }

# Combine the functions - generate a model object
  kde.nbc.model = function(data.train, classes, priors=table(classes)/length(classes), ...){
    kdes = generate.kdes(data.train, classes, ...)
    model <- list(
      "priors"   = priors,
      "classes"  = names(priors),
      "kdes"     = kdes,
      "features" = names(kdes)
    )
    return(model)
  }
  
# With test data... we can create a very funky class - expression - probability matrix
  nbc.probability.matrix <- function(data.test, model, mc.cores=16){
    
    bFUN = function(x, d){
      return(kde.height(a=x, d=d)$y.int)
    }
    
  # MUCH FASTER!!!
    features = model$features[model$features %in% rownames(data.test)]
    pb <- txtProgressBar(min=0 ,max=length(model$classes), style=3)
    kde.probability.matrix <- mclapply(model$classes, function(class.C){
    
      return(t(
        sapply(features, function(gene){
            cat(paste("(", which(features == gene), "/", length(features), ")", class.C, "-", gene, "\n", sep=""))
            apply(data.test[gene,, drop=F], 2, FUN = bFUN, d=model$kdes[[gene]][,c(class.C,"x.range")])
           }
          )
        ))
      }, 
      mc.cores=mc.cores
    )
  # Rename!?
    names(kde.probability.matrix) = model$classes
    return(kde.probability.matrix)
}

# And use a simple function to predict the cells 
# Predictions! 
  predict.kde.nbc.from.matrix <- function(data.test, kde.probability.matrix, model){
  output <- sapply(
    model$classes, function(class.C) {
      return(colSums(log(kde.probability.matrix[[class.C]]+1)) + log(model$priors[class.C]))
    }
  )
  
  output <- as.data.frame(output)
  rownames(output) = colnames(data.test)
  colnames(output) = model$classes
        #  output.numbers = output[,model$classes]
        #  output.noiseless = output[,model$classes[model$classes!="0"]]
        # 
          output$prediction = factor(colnames(output)[sapply(rownames(output), function(j) {which.max(output[j,])})], levels=model$classes)
        #    noise.sub <- (output[rownames(noise.sub),"0"] - output.noiseless[rownames(noise.sub),])
        #  output$prediction.noiseSub = output$prediction
        #  output[rownames(noise.sub),prediction.noiseSub] = sapply(1:length(noise.sub[,1]), function(i) {names(which.min(noise.sub[i,]))})
        #    # DISTANCE OF 1st GUESS FROM 2nd GUESS
        #  output$prediction.dist = sapply(length(output[,1]), function(i) {output[i,output$prediction[i]] - max(output[i,model$classes[model$classes!=output$prediction[i]]])})
        #    # DISTANCE OF 2nd GUESS FROM 3RD (excluding noise)
        #  output$prediction.noiseSub.dist = sapply(length(output[,1]), function(i) {output[j,output$prediction[j]] - max(output[j,model$classes[model$classes!=output$prediction[j]]])})
        #    # 
        #  # IDEAS FOR CERTAINTY CALCULATIONS?? Sub-sample the probability matrix features and repeat the MLE step for k times. 
        #  # Create a distribution of probabilities for the guesses and run statistical testing to determine which distribution
        #  # is greater than the average.
          
  return(output)
  }
  
