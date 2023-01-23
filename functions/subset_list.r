# Function to subset a list from another list...


  subset.list = function(x, items){
    output = lapply(items, function(k){x[[k]]})
    if(class(items) == "character"){names(output) = items}
    return(output)
  }
