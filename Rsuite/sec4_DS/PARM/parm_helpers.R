#!/bin/Rscript
# helper functions for PARM, pre-compiled for speed. Probbaly.

arrm.find.breakpoints <- function(r_sq, wind_len, start=1){
  #'make sure that the breakpoint finding code is reproducable between
  #'the two breakpoint-finding steps
  mean_rsq <- mean(r_sq, na.rm=T)
  sdev_rsq <- sqrt(var(r_sq, na.rm=T))
  k.out <- rep(NA, 3) #ouptut with the breakpoints
  k.i <- 1            #write index of current breakpoints
  s.index <- start-1  #index of first point to start writing to; is 0 for second case.

  for (p in 1:3){ #Find up to 6 break points total,or 3 per call to this fxn
    na.vec <- is.na(r_sq)
    found.min=F
    min_rsq = suppressWarnings(min(r_sq, na.rm=T))
    for (j in 1:length(r_sq)){
      if(sum(is.na(r_sq))!=length(r_sq)){
        if(!na.vec[j] & r_sq[j]==min_rsq &
           (r_sq[j] <= (mean_rsq))){
          #The above criteria match at most one point in the vector. However,
          #that one point is about to get removed as part of the next step
          #carried out.
          #The above was edited 11-23-16, to account for the fact that sdev_rsq
          #is sometimes NA when other parts of the vector AREN'T.
          k.out[k.i] <- j
          break
        }
      }#end check for all missing r_sq
    }
    #if a breakpoint was found
    if(!is.na(k.out[k.i])){
      for(j in 1:k.i){ #for all current breakpoints
        window.index <- round(wind_len[k.out[j]+s.index])
        #cut out section of 'window' surrounding the found break point
        if(k.out[j] < window.index ){
          r_sq[0:(k.out[j]+window.index)]<- NA    #break point is close to beginning of vector...
        }
        if ((k.out[j]>=window.index) & (k.out[j]<=(length(r_sq)-window.index))){
          r_indices <- (k.out[j]-window.index):(k.out[j]+window.index)
          r_sq[r_indices] <- NA
        }
        if (k.out[j]>(length(r_sq)-window.index)){
          r_sq[(k.out[j]-window.index):length(r_sq)] <- NA    #break point is close to end of vector
        }
      }
    }else{
      #If there is no breakpoint found, k.out[k.i] is already missing
    }
    k.i <- k.i + 1
  }#end for p in 1:3
  k <- k.out[!is.na(k.out)]
  if(length(k)==0){
    k <- NA
  }
  return(list(breakpoints=k, r_sq=r_sq))
}

arrm.find.bad.slopes <- function(values, slopes, xvals, run=T){
  #Finds bad breakpoints based on the ciriteria described in the
  #ARRM paper.
  # find negative/flat slopes and the value of corresponding breakpoints
  # note: if negative slope is first segment, breakpoint to the right is removed
  #       if negative slope is any other segment, breakpoint to the left is removed
  # values2 is list of breakpoints corresponding to bad slopes
  # slopes2 is list of bad slopes
  # breakpoints2 is list of points (not temp values) where bad breakpoints are located
  #CEW: replacing with list of indices corresponding to points to get booted, one for
  #each of the quantities listed above.
  bad.value.indices <- rep(F, length(values))
  bad.break.indices <- rep(F, length(slopes))
  bad.slope.indices <- rep(F, length(slopes))
  if(run){
    #print("Printing slopes and values")
    for(j in 1:length(slopes)){
      if (is.na(slopes[j])){
        #First, check for missing values.
        #This can legit happen with pr values,
        #if you do not include an offset factor.
        bad.value.indices[j] <- T
        bad.break.indices[j] <- T
        bad.slope.indices[j] <- T
      }else{
        if(slopes[j]<=0.1 & j==1){
          bad.value.indices[j] <- T
          bad.break.indices[j] <- T
          bad.slope.indices[j] <- T
        }
        if(slopes[j]<=0.1 & j==length(slopes)){
          bad.value.indices[j-1] <- T
          bad.break.indices[j-1] <- T
          bad.slope.indices[j] <- T
        }
        #Okay, this is the specific line that's leading
        #ot an issue in the main code when working with pr
        #without an offset in addition to the jitter function
        if((slopes[j]<=0.1) & (j!=1) & (j!=length(slopes))
           & ((xvals[j+1]-xvals[j])>5)){
          bad.value.indices[j-1] <- T
          bad.break.indices[j-1] <- T
          bad.slope.indices[j] <- T
        }
        if((slopes[j]>=(-0.1)) & (slopes[j]<=0.1) & (j!=1)
           & (j!=length(slopes)) & ((xvals[j+1]-xvals[j])>2)){
          bad.value.indices[j] <- T
          bad.break.indices[j] <- T
          bad.slope.indices[j] <- T
        }
      }
    }
  }
  return(list('bad.value' = bad.value.indices, 'bad.break' = bad.break.indices,
              'bad.slope' = bad.slope.indices))
}

calc_parm_rsquared <- function(start, end, by, hist_pred, hist_targ, window, dir='forward'){
  #' @param jseq sequence of values for which the r-squared in hist pred and hist targ will be calculated
  #' @param jstart start of jseq
  #' @param rsq vector in which to put the r-squared values. In ivializing out-of-loop b/c
  #' it might be easier to iterate on a sparse vector and compress later
  #' @param hist_pred MH values
  #' @param hist_targ OH values
  #' @param window length of the window over which the r-squared values are going to be calculated
  #' @param dir the direcction in which the r-squared calculations go. can be one of 'forward' or 'backward'
  #'
  #' @return rsq, vector of r-squared values
  #
  #rsq <- rep(NA, length(jseq))
  n <- start
  jseq <- seq(start, end, by=by)
  rsq <- rep(NA, (abs(start-end) + 1))
  for (j in jseq){
    if (dir == 'forward'){
      j.val <- (j-start + 1)
      n_start <- n-(window[j]-1)
      n_end <- n
    }else if (dir == 'backward'){
      j.val <- (start-j)+1
      n_start <- n
      n_end <- n+window[j]-1
    }
    sm <- hist_targ[n_start:n_end]
    mm <- hist_pred[n_start:n_end]
    ix=!is.na(sm)
    sm=sm[ix]
    mm=mm[ix]
    qrd=qr(matrix(c(rep(1,sum(ix)),mm),ncol=2,byrow=F))
    yhat=qr.fitted(qrd,sm)
    mss=sum((yhat-mean(yhat))^2)
    rss=sum((sm-yhat)^2)
    rsq[j.val] <- mss/(mss+rss)
    n <- n + by
  }
  return(rsq)
}
