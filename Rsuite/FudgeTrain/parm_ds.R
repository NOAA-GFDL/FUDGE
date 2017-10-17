#'Draft code for starting on the modifications needed to get ARRM
#'working in the current FUDGE framework. 
#'
#'
#'expected args for a downscaling function: 
#'
#'pred, targ, new: the hist_pred, targ, and fut_pred datasets. 
#'hist_pred and new are currently indexed by rip; targ is not. 
#'Since this doesn't seem to support multivariate, 
#'it's prob. safe to assume that there's only going to be a single 
#'rip in the hist_pred dataset for now. 
#'

callPARM <- function(hist_pred, hist_targ, fut_pred, args, 
                     missval.args, tmasks, train_qcmask=F){
  #'Control the looping by rip and missval removal from within this function. 
  #'Calls R's lm() function using targ as a function
  #'of linear variability of one or more variables in 
  #'pred, and then uses the data in new to predict
  #'the future results. 
  #'Currently takes 2 arguments to downscaling function: 
  #' subsample_pct, specifying width of windows used for determining knots of
  #'   best fit in percentage points (defaults to 5)
  #' round, specifying if and how to round the output in accordance with prior
  #'   arrm teachnique (defaults to FALSE)
  
  library(splines) #needed for the training, but might be present in base install now.
  #any general pooling steps go in here - keep in mind, 
  #the aarm training code assumes a single vector for the training
  #and future data. 
  
  #add check for presence of round option
  if(!'round'%in%names(args)){
    round.ds <- FALSE
  }else{
    #assume any string argument is false
    if(!is.numeric(args$round)){round.ds<-F}else{round.ds<-T}
  }
  ds.args <- args
  if(!'regression_subsample_method'%in%names(args)){
    stop(paste("Error in PARM: reg_sample_method not included"))
    ds.args$reg_sample_method = 'climvar_range'
  }else{
    reg_sample_method_choices <- c('total_sample_size', 'climate_variable_range')
    if(!(args$regression_subsample_method%in%reg_sample_method_choices)){
      stop(paste("Error in PARM: reg_sample_method must be one of",
                 paste(reg_sample_method_choices, collapse=","),
                 "but was", args$regression_subsample_method))
    }
    #print(reg_sample_method)
  }
  #Add check for the definition of the tails/whether to taper the tails at ends of dist
  if(!'tail_taper_pct'%in%names(args)){
    ds.args$tail_taper_pct <- 0.1
  }else{
    ds.args$tail_taper_pct <- as.numeric(args$tail_taper_pct)/100
    #print('dividng tail_taper_pct')
    #prints are added to deal with something that may have been leading to 
    #tail_taper_pct being divided by 100 repeatedly.
  }
  #Add check for presence of window option
  if(!'subsample_pct'%in%names(args)){
    ds.args$subsample_pct <- 0.05
  }else{
    ds.args$subsample_pct <- as.numeric(args$subsample_pct)/100
    #print('dividing subsample_pct')
  }
  
  #Added as a precipitation adjustment
  jitter.names <- c('scale_factor', 'offset_factor')
  if (any(jitter.names %in% names(args))){
    jitter <- T
    scale_factor <- 1
    offset_factor <- 0
    if ('scale_factor' %in% names(args)){
      scale_factor <- args$scale_factor
    }
    if ('offset_factor' %in% names(args)){
      offset_factor <- args$offset_factor
    }
    pr_jitter_fxn <- function(x){
      x <- x * scale_factor
      x <- x + offset_factor 
      return(x)
    }
    pr_unjitter_fxn <- function(x){
      x <- x - offset_factor
      x <- x/scale_factor
      return(x)
    }
  }else{
    jitter <- F
  }
  #Pre-initialize output
  out.data <- array(NA, dim=dim(fut_pred[[1]]))
  nrips = dim(out.data)[4]
  #Attempting switch to index masking data 
  #on 12-15-16; this is the way we used to do it, 
  #but I thought there was a clever way around it.
  #   if (tmasks$time_mask_overlap){
  #     index.mask <- tmasks$trim$masks[[w]]
  #     olap=T
  #   }else{
  #     index.mask <- tmasks$MF$masks[[w]]
  #     olap=F
  #   }
  #Revising above on 12-17-16. We need to undo the trimming/qm indexing
  #at this step in order to use the full data in the s3/s5 adjustments.
  index.mask <- tmasks$MF$masks[[w]]
  olap <- F
  
  if(train_qcmask){
    if(tmasks$apply_tmasks){
      qc <- mapply(rep, tmasks$MF$masks[[w]], nrips)
    }else{
      qc <- rep(1, dim(MF[[1]]))
    }
    dim(qc) <- dim(out.data)
  }else{
    qc <- NULL
  }
  
  #print(summary(as.vector(hist_pred[[1]])))
  if (all.identical(remove.missvals(hist_pred[[1]]))){
    print("MH data all identical; returning MF data for DS")
    if(train_qcmask){
      qc <- array(1, dim(out.data))
    }
    return(list(ds=fut_pred[[1]], qc=qc))
  }else{
    #Synchronize missing values across OH and MH datasets
    missval.out <- remove.FUDGE.missvals(OH=hist_targ, MH=hist_pred, MF=fut_pred,
                                         missval.args$minsamplesize, 
                                         missval.args, 
                                         missval.method = 'distributional')
    hist_pred = missval.out$MH
    fut_pred  = missval.out$MF
    hist_targ = missval.out$OH #added 11-8-16; trying to track lm diffs
    
    if (length(hist_pred) != length(hist_targ) ){
      #In this case, you will not be able to downscale
      stop(paste("Error in parm_ds: length of OH and MH datasets after removing", 
                 "missing values should be identical!"))
    } 
    
#     print(summary(as.vector(hist_pred[[1]])))
#     print(summary(as.vector(hist_targ[[1]])))
#     print(summary(as.vector(fut_pred[[1]])))
    
    #Added 2017-06-27; all same case causes errors for one subsample method, but
    #returns MF data unchaged for the other
    
    if (missval.out$ds.possible){
      #Now, call the training code
      hist.pred.tmp <- as.vector(hist_pred[[1]]) #unlist(hist_pred) #[!is.na(unlist(hist_pred))]
      hist.targ.tmp <- as.vector(hist_targ[[1]]) #unlist(hist_targ) #[!is.na(unlist(hist_targ))]
      if(jitter){
        hist.pred.tmp <- pr_jitter_fxn(hist.pred.tmp)
        hist.targ.tmp <- pr_jitter_fxn(hist.targ.tmp)
      }
      
      #Okay, pairwise settings go in here
      hist.targ.tmp <- sort(hist.targ.tmp, na.last=FALSE)
      hist.pred.tmp <- sort(hist.pred.tmp, na.last=FALSE)
      oh.len <- length(hist.targ.tmp)
      oh.pres <- which(!is.na(hist.targ.tmp))[1]
      
      #print(summary(hist.pred.tmp[oh.pres:oh.len]))
      #print(summary(hist.targ.tmp[oh.pres:oh.len]))
      
      parm.train.data <- parm.train(hist.pred.tmp[oh.pres:oh.len], 
                                    hist.targ.tmp[oh.pres:oh.len], 
                                    ds.args)
      #10-13: The default behavior of sort is to remove NA values.
      #If that's the case, you might be able to get away with removing
      #the indexing calls in the call to parm.train
      parm.model <- parm.train.data$ds.model
      parm.targ <- parm.train.data$sta_mon_save
      #should be a list of one element - ds.model
      for(n in 1:nrips){
        MF.slice <- fut_pred[[1]][,,,n,,]
        MF.idx <- !is.na(MF.slice)
        MF.slice <- MF.slice[MF.idx]
        if( length(MF.slice) != 0){
          rip.len <- sum(MF.idx)
          rip.rank <- rank(MF.slice, ties='first')
          model_combine_data <- sort(MF.slice)
          if (jitter){
            model_combine_data <- pr_jitter_fxn(model_combine_data)
          }
          out.tmp <- parm.downscale(model_combine_data, 
                                    parm.targ, #maintain orig order and orig placement of missvals
                                    parm.model, ds.args)
          rip.tmp <- as.vector(unlist(out.tmp$fut_obs))
          out <- rep(NA, length(MF.idx))
          out[MF.idx] <- rip.tmp[rip.rank][1:rip.len]
          #print("summary of PARM output:")
          #print(summary(as.vector(out)))
          if (jitter){
            out <- pr_unjitter_fxn(out)
          }
          # previously ouyt
          out.data[,,,n,,] <- index.qm(index.mask, out, olap)
        } #Don't need to do anything otherwise - just return all missing
        #qcmask
        if(missval.out$mv.retry && train_qcmask){
          qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap) 
        }
      }
    }else{
      if(train_qcmask){
        #out.data is already set; qc needs to be replaced
        qc <- array(NA, dim(out.data))
      }
    }
    return(list(ds=out.data, qc=qc))
  }
}

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
    for (j in 1:length(r_sq)){
      if(sum(is.na(r_sq))!=length(r_sq)){
        if(!na.vec[j] & r_sq[j]==min(r_sq, na.rm=T) & 
             #(r_sq[j] <= (mean_rsq-0*sdev_rsq))){
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
          #print(0:(k.out[j]+window.index))
        }
        if ((k.out[j]>=window.index) & (k.out[j]<=(length(r_sq)-window.index))){
          r_indices <- (k.out[j]-window.index):(k.out[j]+window.index)
          r_sq[r_indices] <- NA
          # print(r_indices)
        }
        if (k.out[j]>(length(r_sq)-window.index)){
          r_sq[(k.out[j]-window.index):length(r_sq)] <- NA    #break point is close to end of vector
          #print((k.out[j]-window.index):length(r_sq))
        }
      }
    }else{
      #If there is no breakpoint found, k.out[k.i] is already missing
      #message(paste('no breakpoints found on loop', p))
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

parm.train <- function(hist_pred, hist_targ, args){
  #'Assume for now that the dataset merging is done up in a prior step - 
  #'all that you have to worry about in this step is a single 
  #'vector for each dataset. 
  #'Also assume that this data is pre-sorted by rank; the restoration of calendar
  #'order can be done in the main method of the ds method.
  #'
  #'Modification of the fudge-adapted ARRM code to fit the current requirments of the 
  #'PARM method - i.e. strippign out everything that the ESD team does not need
  #'in order to downscale with anomaly data.
  
  
  #message('Starting PARM steps 2 and 3: find first and second set of breakpoints')
  fixed_window_width = args$subsample_pct
  #Assume that the tails are always going to be symmetric (for now)
  pmin = args$tail_taper_pct
  pmax = 1-(args$tail_taper_pct)
  #min_window_width = 2  #2 degrees celsius or 10 points, whichever is larger
  #pr uses 5 % of mean or 10 points, whichever is larger
  min_window_points = 10 #There have to be at least this many points in the sample
  #...max breakpoints is going to be a bit trickier to set (for one, it needs
  #to be divisible by 2), but can also be set if that is desired.
  max_breakpoints=6/2
  
  # DETERMINE START POINT FOR FIRST LOOK FOR BREAKPOINTS (40th percentile)
  hist.pred.len <- length(hist_pred)
  start <- round(0.4*hist.pred.len)
  
  # DETERMINE HIGHEST AND LOWEST VALUES FOR X|P<0.1 AND X|P>0.9 FOR EACH MONTH
  p_low <- round(pmin*hist.pred.len)
  p_high <- round(pmax*hist.pred.len)
  
  # determine length of moving window (5% of data length), diff for each month, and decreasing towards the tails
  #CEW note: this changes to 10% in what I think is the cross-validated code. 
  #window_temp <- (0.05*(max(hist_pred) - min(hist_pred)))
  
  #Cutting out about 100 lines of code related to finding the temperature-based windows
  if(args$regression_subsample_method=='total_sample_size'){
    if(args$tail_taper_pct==0){
      #If not tapering on the tails, however they are defined
      window_len = hist.pred.len * fixed_window_width
      window1 <- rep(window_len, hist.pred.len)
      window2 <- rep(window_len, hist.pred.len)
    }else{
      #If tapering on the tails
      window_len = hist.pred.len * fixed_window_width
      window_samples = rep(window_len, hist.pred.len)
      for(j in 1:p_low){
        window_samples[j] <- j/p_low*window_len
      }
      j.val <- p_high
      p_val = hist.pred.len - p_high + 1
      for(j in 1:(p_val)){
        window_samples[j.val] <- (p_val-(j+1))/p_val*window_len
        j.val <- j.val + 1
      }
      window_samples[window_samples < min_window_points] <- min_window_points
      window1 <- window_samples
      window2 <- window_samples
    }
  }else if(args$regression_subsample_method=='climate_variable_range'){
    if(args$tail_taper_pct==0){
      #If allowing the range to taper naturally on the tails
      var.range = range(hist_pred)
      window_delta = fixed_window_width * (var.range[2] - var.range[1])
      window1 <- rep(NA, (hist.pred.len-start+1))
      for (j in start:hist.pred.len){
        j.val <- (j-start + 1)
        q <- 0
        val <- hist_pred[j]
        q <- which( hist_pred > (val-window_delta) )[1] - 1
        window1[j.val] <- (j-q)
      }
      window1 <- c(rep(NA,(start-1)),window1)
      window2 <- rep(NA, hist.pred.len)
      for(j in hist.pred.len:1){
        q <- 0
        val <- hist_pred[j]
        q <- which( hist_pred > (val-window_delta) )[1]-1
        window2[(hist.pred.len-j)+1] <- (j-q)
      }
    }else{
      #If controlling the taper of the tails
      window_temp <- fixed_window_width*(max(hist_pred,na.rm=T)-min(hist_pred,na.rm=T))
      #min_window <- 2
      window_temp_new <- rep(NA, hist.pred.len)
      for (j in 1:p_low){       #for the low tail
        window_temp_new[j] <- j/p_low*(window_temp)
      }
      for (j in (p_low+1):(p_high-1)){       #for most of the distribution
        window_temp_new[j] <- window_temp
      }
      j.val <- p_high
      p_val = hist.pred.len - p_high + 1
      for(j in 1:(p_val) ){       #for the high tail
        window_temp_new[j.val] <- (p_val-j+1)/(p_val)*(window_temp)
        j.val <- j.val + 1
      }  
      if(any(is.na(window_temp_new))){
        stop(paste("Error when making windows: check your inputs!"))
      }    
      #find length of window so that it is always the same span in temperature
      window1 <- rep(NA, (hist.pred.len-start+1))
      for (j in start:hist.pred.len){
        j.val <- (j-start + 1)
        q <- 0
        val <- hist_pred[j]
        q <- which( hist_pred > (val-window_temp_new[j]) )[1] - 1
        window1[j.val] <- (j-q)
      }
      if(any(is.na(window1))){
        stop('something is off about this add')
      }
      window1[which(window1<min_window_points)] <- min_window_points
      window1 <- c(rep(NA,(start-1)),window1)
      window2 <- rep(NA, hist.pred.len)
      for(j in hist.pred.len:1){
        q <- 0
        val <- hist_pred[j]
        q <- which( hist_pred > (val-window_temp_new[j]) )[1]-1
        window2[(hist.pred.len-j)+1] <- (j-q)
      }
      window2[which(window2<min_window_points)] <- min_window_points
    }
  }
  #Although they are called the same thing, they are each used slightly differently
  #when calculating the windows. See diagram.
  
  #calculate r squared of linear regressions the length of 'window1'
  #CEW: reference line is 550
  r_sq1 <- rep(NA, hist.pred.len-start)
  n <- start
  #window1 issues started around here
  for(j in start:hist.pred.len){
    j.val <- (j-start+1)
    sm <- hist_targ[(n-(window1[j]-1)):n]
    ix=which(!is.na(sm))
    sm=sm[ix]
    mm <- hist_pred[(n-(window1[j]-1)):n]
    mm=mm[ix]
    qrd=qr(matrix(c(rep(1,length(ix)),mm),ncol=2,byrow=F))
    yhat=qr.fitted(qrd,sm)
    mss=sum((yhat-mean(yhat))^2)
    rss=sum((sm-yhat)^2)
    r_sq1[j.val] <- mss/(mss+rss)
    n <- n+1
  }
  r_sq1_save <- r_sq1
  
  ###Modification 12-18-15 to make sure that precisely the same
  ###code is being used for both breakpoint calculations
  bp.1 <- arrm.find.breakpoints(r_sq1, window1, start)
  k <- bp.1$breakpoints
  r_sq1 <- bp.1$r_sq
  
  r_sq_new1 <- r_sq1_save[k]
  k <- (k + start - 1)
  k_old <- k #why are they calling this k_old if they modify the vals in k?
  
  #CEW: I have no idea what this bit of code is supposed to accomplish, and I 
  #can think of one case where if fails spectacularly (the single NA breakpoint case)
  if(!is.na(k[1])){
    for(j in 1:length(k)){
      k[j] <- k[j] - floor(1/2*window1[k[j]])
    }
  }
  #AARM 3: find second set of breakpoints in the R code
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 646, more or less  
  #message('Starting PARM step 3: find second set of breakpoints')
  nas.3 <- is.na(k[1])
  if(!is.na(k[1])){
    #print('deriving k from the min of the previous breakpoints')
    start2 <- min(k,na.rm=T) #most common case
  }else{
    start2 <- start
  }
  if(start2 > hist.pred.len){
    #print('setting second start based on hist.pred.len')
    start2 <- hist.pred.len
  }
  
  #calculate r squared of linear regressions the length of 'window2'
  r_sq2 <- rep(NA, start2)
  n <- start2
  for(j in start2:1){
    sm <- hist_targ[n:(n+(window2[j]-1))]
    ix=which(!is.na(sm))
    sm=sm[ix]
    mm <- hist_pred[n:(n+(window2[j]-1))]
    mm=mm[ix]
    qrd=qr(matrix(c(rep(1,length(ix)),mm),ncol=2,byrow=F))
    yhat=qr.fitted(qrd,sm)
    mss=sum((yhat-mean(yhat))^2)
    rss=sum((sm-yhat)^2)
    r_sq2[(start2-j)+1] <- mss/(mss+rss)
    n <- n-1
  }  
  #reverse the r_sq2 vectors
  r_sq2 <- rev(r_sq2)
  r_sq2_save <- r_sq2
  #reverse the window2 vectors
  window2 <- rev(window2)
  #remove values around the first break point found in part 1
  r_sq2[( length(r_sq2)-round(0.5*window2[start2]) ):length(r_sq2)] <- NA
  
  ###Modification 12-18 to make sure that precisely the same
  ###code is being used for both breakpoint calculations
  bp.2 <- arrm.find.breakpoints(r_sq2, window2) #no start
  k2 <- bp.2$breakpoints
  r_sq2 <- bp.2$r_sq
  
  #Addin index correction (line 668)
  if(!is.na(k2[1])){
    for(j in 1:length(k2)){
      k2[j] <- k2[j] + floor(1/2*window2[k2[j]])
    }
  }  
  #save vector with only those months that have breakpoints
  #CEW: I'm not sure that I see the point of this bit of the code. 
  #Check later to see if if has an effect again, after more of the calcs are complete?
  r_sq_new2 <- r_sq2_save[k2] #prev. k.2
  
  #AARM 4: combine the breakpoints calculated in parts 1 and 2
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 795, more or less
  #message('Starting PARM step 4: combining the lists of breakpoints')
  #cut off break point if it is in the first 20 points or the last 5
  breakpoints0 = rep(NA, 6)
  
  k.indices <- k[!is.na(k) & k > 20 & (k < hist.pred.len-4)]
  breakpoints0 <- k.indices
  r_sq1 <- r_sq_new1[k.indices]
  #check second vector of breakpoints
  k2.indices <- k2[!is.na(k2) & k2 > 20 & (k2 < hist.pred.len-4)]
  breakpoints0 <- c(breakpoints0, k2.indices)
  r_sq2 <- r_sq_new2[k2.indices]
  
  if(length(breakpoints0)==0){ #there was a second check for nas.2 and nas.4 in here, but I'm not sure it's relevant any more
    breakpoints0 <- NA
  }
  
  breakpoints <- sort(breakpoints0, na.last=T)
  
  #   if(length(breakpoints)==6){
  #     print('there is at least one case where you get all 6 breakpoints')
  #   }
  rank <- rank(breakpoints0, na.last=T, ties='first')
  #get the actual values of temp/precip where the break is
  if(length(breakpoints) > 0){
    #print('current breakpoints:')
    #print(breakpoints)
    #CEW edit 12-14-16:
    #making sure breakpoitns are unique
    #because that seems to have been leading to squirrely PARM results
    breakpoints <- unique(breakpoints)
    values <- hist_pred[breakpoints]
  }else{
    print("No breakpoints found")
    values <- NA
    breakpoints <- NA
  }
  
  #AARM 5: Compute the regression model using the breakpoints
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 860, more or less
  #message('Starting PARM step 5: compute regression model using the breakpoints')
  #perform regressions
  mod=hist_pred
  sta=hist_targ
  ds <- data.frame(mod=hist_pred, sta=hist_targ)
  #CEW code added 11-22-2016 for the pr case; returns a lot of identical breakpoints
  #values <- unique(values)
  
  #Boundary.knots was -999,999 for all calculations up to this point.
  #However, this may be leading to values of boundary.knots of ~1000 in./day
  #before the tail adjusts are applied; we're working on a replacement paramter.
  boundary.knots <- get.knot.range(c(hist_pred, hist_targ))
  
  if(!is.na(breakpoints[1])  && length(unique(values)) > 1 ){
    print(paste('Computing piecewise regression with knots', paste(values, collapse=",")))
    regr_piecewise <- lm(sta ~ bs(mod, knots=values,deg=1, Boundary.knots=boundary.knots #c(-999,999)
    ),data=ds)
    
    ### More diagnostic prints 2017-06-27
#     print("first set of breakpoints:")
#     print(format(values, digits=10), sep="  ")
    slopefun <- function(x.vals, y.vals){
      n.slope <- length(x.vals) - 1
      slope.vals <- rep(NA, n.slope)
      for (n in 1:n.slope){
        slope.vals[n] <- (x.vals[n+1] - x.vals[n]) / (y.vals[n+1] - y.vals[n])
      }
      return(slope.vals)
    }
    b.knot.vals <- predict(regr_piecewise, data.frame(mod=boundary.knots))
    b.pred.vals <- predict(regr_piecewise, data.frame(mod=values))
    b.slopes <- slopefun(c(boundary.knots[1], values, boundary.knots[2]), 
                         c(b.knot.vals[1], b.pred.vals, b.knot.vals[2]))
#     print("first set of slopes")
#     print(format(b.slopes, digits=10, sep="  "))
    
  }else{
    print("No breakpoints or all identical breakpoints found; activating simple lm prediction")
    regr_piecewise <- lm(sta ~ mod, data=ds)
    breakpoints <- NA
  }
  
  #AARM 6: Go through the model and remove slopes close to 0
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 880, more or less
  #message('Starting PARM step 6: removing model slopes close to 0')
  
  # find model values (xvals) and obs values (yvals) of start point, breakpoints, and end point to calculate slopes
  xvals <- c(min(hist_pred),values,max(hist_pred))
  yvals <- c(regr_piecewise$fitted[1],regr_piecewise$fitted[breakpoints],
             regr_piecewise$fitted[length(regr_piecewise$fitted)])
  # use values found above to calculate slopes of each separate segment in the piecewise regression
  slopes <- rep(NA, length(xvals)-1)
  for(j in 1:(length(xvals)-1) ){
    slopes[j] <- (yvals[j+1]-yvals[j])/(xvals[j+1]-xvals[j]+0.0001)
  }
  
  # find negative/flat slopes and the value of corresponding breakpoints 
  # note: if negative slope is first segment, breakpoint to the right is removed
  #       if negative slope is any other segment, breakpoint to the left is removed
  # values2 is list of breakpoints corresponding to bad slopes
  # slopes2 is list of bad slopes
  # breakpoints2 is list of points (not temp values) where bad breakpoints are located
  #CEW: replacing with list of indices corresponding to points to get booted, one for 
  #each of the quantities listed above.
  
  #CEW 12-14-16: replacing xvals (which had been values + range) with which(hist_pred %in% values)
  bad.indices <- arrm.find.bad.slopes(values, slopes, c(1,breakpoints,hist.pred.len), #xvals, 
                                      run=!is.na(breakpoints[1]))
  # slope_rank is the rank of bad slopes, so the lowest can be removed first
  slope_rank <- rank(slopes[bad.indices$bad.slopes], ties='first') #lowest bad slope
  values2 <- values[bad.indices$bad.values]                        #all bad values
  breakpoints2 <- breakpoints[bad.indices$bad.breaks]              #all bad breaks
  values_new1 <- values
  breakpoints3 <- breakpoints
  
  for (k in 1:5){# do loop up to 5 times (assuming that one month cannot possibly have that many negative slope segments)
    #if(sum(bad.slope.indices)>=1){
    if(sum(bad.indices$bad.slopes)>=1){
      #print('there is at least one point where the bad slope indices are activated')
      if(length(values_new1)==1)  values_new1 <- NA
      else{
        #Store all points but the single bad value of interest
        #CEW edit 12-13: == not working for values removal; adding .eps for now
        eps = .Machine['double.eps'][[1]]*2
        values_new1 <- values_new1[abs(values_new1 - values2[slope_rank==1]) <= eps] #closest equivalent point
        breakpoints3 <- breakpoints3[breakpoints3!=breakpoints2[slope_rank==1]]
      }
      #Perform regressions again with the first slope taken out
      ds <- data.frame(mod=hist_pred, sta=hist_targ)
      if(!is.na(breakpoints[1])){
        #print("Beginning piecewise linear regression")
        regr_piecewise2 <- lm(sta ~ bs(mod, knots=values_new1,deg=1,
                                       #Boundary_knots has been set for our PARM analysis, 
                                       #but it might be leading to weird settings for precip.
                                       #Like, 10000-inches-per-day weird.
                                       # Boundary.knots=c(-999,999)),
                                       Boundary.knots=boundary.knots),
                              #),
                              data=ds)
      }else{
        #print("No breakpoints found; activating simple lm prediction")
        regr_piecewise2 <- lm(sta ~ mod, data=ds)
      }
      # recalculate slopes
      xvals2 <- c(min(hist_pred),values_new1,max(hist_pred))
      yvals2 <- c(regr_piecewise2$fitted[1],
                  regr_piecewise2$fitted[breakpoints3],
                  regr_piecewise2$fitted[length(regr_piecewise2$fitted)])
      #remove missing values
      x.dx <- which(is.na(xvals2))
      y.dx <- which(is.na(yvals2))
      if (length(c(x.dx,y.dx))>0){
        xvals2 <- xvals2[-c(x.dx,y.dx)]
        yvals2 <- yvals2[-c(x.dx,y.dx)]
      }
      slopes <- rep(NA, (length(xvals2)-1)) #added to address indexing error
      for(j in 1:(length(xvals2)-1)){
        slopes[j] <- (yvals2[j+1]-yvals2[j])/(xvals2[j+1]-xvals2[j]+0.0001)
      }
      bad.indices <- arrm.find.bad.slopes(values, slopes, c(1,breakpoints,hist.pred.len), #xvals 
                                          run=!is.na(breakpoints[1]))
      if(sum(bad.indices$bad.slopes) >= 1){
        slope_rank <- rank(slopes[bad.indices$bad.slopes], ties='first')
      }
      #Added 12-22, to try and catch the breakpoint differences
      ##removed 1-11, to try and figure out why points are failing to match
      ##values2 <- values[bad.indices$bad.values]
      ##breakpoints2 <- breakpoints[bad.indices$bad.breaks]
      ##regr_piecewise <- regr_piecewise2 #regression model
      ##values <- values_new1             #values of hist_pred at those points
    }#end check for any present bad values; commented out 1-11 to better match the  original arrm code
  }#end k loop
  return(list(ds.model=regr_piecewise, sta_mon_save=hist_targ))
  
}#end arrm.train

parm.downscale <- function(fut_pred, hist_targ, regr_piecewise, 
                           args){
  
  #'Modified version of ARRM step 6, modified to fit into the parm framework
  #'(i.e. stripping out everything not related to downscaling.)
  
  #AARM 6: Run the regression on the future predictor data.
  #assume that looping by rip is taking place in the ds function's main method.
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 1030, more or less
  
  message('Starting PARM step 7: predicting future observations')
  if (length(fut_pred) > 0){
    fut_obs <- predict(regr_piecewise, data.frame(mod=fut_pred))
  }else{
    fut_obs <- c()
  }
  regr <- regr_piecewise
  model_future_fit <- fut_obs
  return(list(fut_obs=fut_obs, fit=NULL))
}#end arrm.downscale

get.knot.range <- function(dataset){
  #'Gets parameters for the Boundary.knots
  #'with some wiggle room, since data may not be same shape
  #' in the future.
  data.range <- range(dataset, na.rm = T, finite = T)
  #data.dev <- sd(dataset, na.rm = T)
  #KD, D1A conversation 2017-06-20 suggested wider range
  data.dev <- abs(diff(data.range))
  if (is.na(data.dev)){
    #As of now, this case is only likely to happen with pr
    #and TBH, it's sort of vanishingly unlikely
    #     if (!is.na(data.dev[1])){
    #       print("Too few samples for boundary.knots calculation; defaulting to 5 percent")
    #       data.dev <- 0.05 * dataset
    #     }else{
    #       stop("Error in get.knot.range: passed dataset")
    #     }
    stop(paste("Error in get.knot.range: returned diff(data.range) was NA!"))
  }
  #return(c(data.range[1] - 2*data.dev, data.range[2]+2*data.dev))
  return(c(data.range[1] - data.dev, data.range[2] + data.dev))  
}
