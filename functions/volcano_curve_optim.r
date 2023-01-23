
# Edited for F1-score

volcano.curve.optim <- function(x,y,positive.vector.rows, negative.vector.rows, report=F, bar=F, seq.len=1000){

#       x = lf2cs
#       y = -log10 pvalues
#       positive.vector.rows = positions (rows) in data frame which are true positive 
#                (for example, using a random forest or through a pvalue cutoff).
#       report = F generates only the final (optimal C value) and best F1 score.
#       report = T generates a large results list object which has all iterations.


# set up intial vectors etc
        l2fcs   <- x
        pvalues <- y
          pvalues[which(pvalues==Inf)] <- max(pvalues[which(pvalues!=Inf)])
        z.all <- 1:length(l2fcs)
        z <- positive.vector.rows
        zN <- (1:length(l2fcs))[-z]
# calculate standard deviation of the x values
        x0 = sd(l2fcs)
# rearranged the equation to derive initial values...
# y = c / (x - x0)
# y * (x - x0) = c

  c.init <- min(na.omit(pvalues)) * (min(abs(na.omit(l2fcs))) - x0)
  c.max <- max(na.omit(pvalues)) * (max(abs(na_markers <- na.omit(l2fcs))) - x0)

# Sequence (roughly, crude optimisation intervals)
     #   c.seq <- seq(c.max,c.init, length=seq.len) # EDIT - changed to log scaling using my seq.log function
        c.seq = seq.log.basic(c.max, 1e-3, length=seq.len)
        c.res <- numeric(seq.len)
# results lists
        TP.res <- list()
        TN.res <- list()
        FP.res <- list()
        FN.res <- list()
        conf.matrix <- list()
# progress bar
        if(bar==T){pb <- txtProgressBar(0,length(c.seq), style=3)}

# Start up our optimisation loop
        for(i in 1:length(c.seq)){
	        c = c.seq[i]
          # Find values above line on LHS 
                # LHS - bound
                  lhs = (c / (-l2fcs - x0)); lhs[l2fcs > -x0] = Inf
	        sgv1 <- which(pvalues > lhs)
          # ... and RHS
                # RHS - bound
                    rhs = (c / (l2fcs - x0)); rhs[l2fcs < x0] = Inf
	        sgv2 <- which(pvalues > rhs)
           # Find values below line
	        sgv3 <- which(pvalues < lhs & pvalues < rhs)

	   if(report!=T) {
      plot(l2fcs, pvalues, pch=16, cex=0.7)
      points(l2fcs, rhs, col="blue", cex=0.5, pch=16)
      points(l2fcs, lhs, col="blue", cex=0.5, pch=16)
      points(l2fcs[sgv1], pvalues[sgv1], col="yellow", cex=0.9, lwd=2)
      points(l2fcs[sgv2], pvalues[sgv2], col="yellow", cex=0.9, lwd=2)
      points(l2fcs[sgv3], pvalues[sgv3], col="black", cex=0.9, lwd=2)
      points(l2fcs[positive.vector.rows], pvalues[positive.vector.rows], col="green3", cex=0.6, pch=16)
      points(l2fcs[negative.vector.rows], pvalues[negative.vector.rows], col="red", cex=0.6, pch=16)
     }
                # are these positive or negative?
                        # find positions of all z and mark with T
	                        t.real <- rep(FALSE, length(z.all)); t.real[z] <- TRUE
                        # find positions of z above the line and mark with T
	                        t.predicted <- rep(FALSE, length(z.all)); t.predicted[c(sgv1, sgv2)] <- TRUE
	               
          
          # set up confusion matrix for ith value of c
	        conf.matrix[[i]] <- table(factor(t.real, levels=c("TRUE", "FALSE")), factor(t.predicted, levels=c("TRUE", "FALSE")))
               # Here's our stats
	        TP.res[[i]] <- which(t.real & t.predicted)
	        TN.res[[i]] <- which(!(t.real & t.predicted))
	        FP.res[[i]] <- which(t.predicted & !t.real)
	        FN.res[[i]] <- which(t.real & !t.predicted)


          # Here's our PPV and other scores (I chose to comment out because F1 score is great.
	        # positive recall maximising - c.res[i] <- conf.matrix["TRUE", "TRUE"]/(conf.matrix["TRUE", "TRUE"]+ conf.matrix["TRUE", "FALSE"])
	        # accuracy maximising -	c.res[i] <- (conf.matrix["TRUE", "TRUE"]+conf.matrix["FALSE", "FALSE"])/(length(z.all))
	        # positive predictive value (PPV) * absolute true-positive rate (A.TPR)
        #		c.res[i] <- ((conf.matrix[[i]]["TRUE", "TRUE"])/(conf.matrix[[i]]["TRUE", "TRUE"] + conf.matrix[[i]]["FALSE", "TRUE"]))*length(TP.res[[i]])
	        # Secondary deciding factor. Use recall as the factor ONLY if the c.res has more than one maximal point.


                # F-score
           # Here's our F1 score
                c.res[i] <- 2*conf.matrix[[i]]["TRUE", "TRUE"] / 
                (2*conf.matrix[[i]]["TRUE", "TRUE"] + conf.matrix[[i]]["FALSE", "TRUE"] + conf.matrix[[i]]["TRUE", "FALSE"])
                
                
                
     
           
              ## NEGATIVE CONTROL EDIT   ---   using a TRUE NEGATIVE set of genes...    
	                      if(!missing(negative.vector.rows)) {
                          # find positions of all negative.vector.rows and mark with T
	                          n.real <- rep(FALSE, length(z.all)); n.real[negative.vector.rows] <- TRUE
                          # find positions of z BELOW the line and mark with T
	                          n.predicted <- rep(FALSE, length(z.all)); n.predicted[c(sgv3)] <- TRUE
	                          
	                       TP.res[[i]] <- which(t.real & t.predicted)
	                       TN.res[[i]] <- which(n.real & n.predicted)
	                       FP.res[[i]] <- which(n.real & t.predicted)
	                       FN.res[[i]] <- which(t.real & n.predicted)
	                       conf.matrix[[i]] <- confusionMatrix(
	                        data = factor(c(
	                         c("NEG", "POS")[as.integer(t.predicted[z])+1],
	                         c("POS", "NEG")[n.predicted[negative.vector.rows]+1]
	                         ), levels=c("NEG", "POS")),
	                         reference = factor(rep(c("POS","NEG"), c(length(z), length(negative.vector.rows))),levels=c("NEG", "POS"))
	                        )
	                        
	                        # c.res[i] <- as.numeric(conf.matrix[[i]]$byClass["F1"])
	                        c.res[i] <- as.numeric(conf.matrix[[i]]$overall[1]) ### Overall accuracy (two - class)
                        }

                if(bar==T){setTxtProgressBar(pb,i)}
           }
     

        # This was some data handling thing
	        remove.me <- which(c.res == "NaN")
	        c.res[remove.me] <- 0

        # Output report if flagged in function (used for optimisation and plotting the scoring)
        # Otherwise you can use the function to only output the BEST c value and SCORE
if(report==T){
	return(list("TP" = TP.res, "TN" = TN.res, "FP" = FP.res, "FN" = FN.res, "F1" = c.res, 
			"recalls" = sapply(1:seq.len, function(i) {length(TP.res[[i]])/length(z)}),
			"best.res" = which(c.res == max(c.res)),
			"best.res.recalls" = sapply(which(c.res == max(c.res)), function(i) {length(TP.res[[i]])/length(z)}),
			"c" = c.seq,
			"best.c" = c.seq[which.max(c.res)],
			"best.c.recall" = c.seq[which(c.res == max(c.res))[which.max(sapply(which(c.res == max(c.res)), function(i) {length(TP.res[[i]])/length(z)}))]]
			)
	)
} else {
	return(cbind("c.value" = c.seq[which(c.res == max(c.res))[which.max(sapply(which(c.res == max(c.res)), function(i) {length(TP.res[[i]])/length(z)}))]], "max.PPV.ATPR" = c.res[which(c.res == max(c.res))[which.max(sapply(which(c.res == max(c.res)), function(i) {length(TP.res[[i]])/length(z)}))]]))
}
cat("\n")
}

