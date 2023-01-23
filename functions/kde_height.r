 # Function for estimating the height of KDE curves (or in fact, any x, y, curve).  
 # Useful for likelihood in naive bayes classifiers
  kde.height = function(a, d) {
   colnames(d)[!colnames(d) == "x.range"] = "y"
  
    # Within d[,"x.range"] bounds (default)
      # x boundaries surrounding x
        x1=tail(d[,"x.range"][d[,"x.range"] < a],1)
        x2=head(d[,"x.range"][d[,"x.range"] > a],1)
      # y boundaries surrounding y[x]  
        y1=tail(d$y[d[,"x.range"] < a],1)
        y2=head(d$y[d[,"x.range"] > a],1)
        
      # Gradient calculation
        m = (y2-y1) / (x2-x1)
      
      # find height y for the intercept (x1) + gradient * value to estimate
        y.int = m * (a-x1) + y1 
           
           
    # Above d[,"x.range"] bounds
      if(a >= max(d[,"x.range"])){
      # x boundaries surrounding x
        x1=tail(d[,"x.range"][d[,"x.range"] < a],1)
        x2=tail(d[,"x.range"][d[,"x.range"] < a],1)
      # y boundaries surrounding y[x]    
        y1=tail(d$y[d[,"x.range"] < a],1)
        y2=tail(d$y[d[,"x.range"] < a],1)
      # height y PDF === y2 when x is out of bounds
        y.int = y2
      }
    
    # Below d[,"x.range"] bounds
      if(a <= min(d[,"x.range"])){
      # x boundaries surrounding x
        x1=head(d[,"x.range"][d[,"x.range"] > a],1)
        x2=head(d[,"x.range"][d[,"x.range"] > a],1)
      # y boundaries surrounding y[x]    
        y1=head(d$y[d[,"x.range"] > a],1)
        y2=head(d$y[d[,"x.range"] > a],1)
      # height y PDF === y2 when x is out of bounds
        y.int = y1
      }
       
      
      return(data.frame(a, y.int, x1, x2, y1, y2))
      # three blind mice
  }
