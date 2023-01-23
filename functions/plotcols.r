# Simple numeric scale to colour grade...


plotcols = function(data, colours = alt.cols){
    colorRampPalette(color=colours)(101)[as.numeric(cut(data,101))]
  }
  
