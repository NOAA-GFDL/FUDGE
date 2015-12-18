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

arrm.main <- function(hist_pred, hist_targ, fut_pred, args, ds.var, att_table){
  #'Control the looping by rip and missval removal from within this function. 
  #'Calls R's lm() function using targ as a function
  #'of linear variability of one or more variables in 
  #'pred, and then uses the data in new to predict
  #'the future results. 
  #'Does not currently take any arguments
  
  library(splines) #needed for the training, but might be present in base install now.
  #names(pred) <- lapply(att_table$hist.pred, '[[', 'var')
  #any general pooling steps go in here - keep in mind, 
  #the aarm training code assumes a single vector for the training
  #and future data. 
  
  #Now, call the aarm training code
  #CEW: the missing value removal step might not be neccessary. 
  hist.pred.tmp <- unlist(hist_pred)[!is.na(unlist(hist_pred))]
  hist.targ.tmp <- unlist(hist_targ)[!is.na(unlist(hist_targ))]
  arrm.model <- arrm.train(sort(hist.pred.tmp), 
                           sort(hist.targ.tmp), args, ds.var)
  #should be a list of one element - ds.model
  
  rip.list <- lapply(att_table$fut.pred, '[[', 'rip')
  rips <- unique(rip.list)  
  out.list <- list()
  for(rip in rips){
    out.var <- paste(ds.var, rip, sep=".")
    #Get true/false vector to index out the data that you want
    rip.index <- (rip.list==rip)
    rip.data <- fut_pred[rip.index]
    #And to make sure that everything has the same names as
    #earlier, rename the list in the selected dataset
    names(rip.data) <- lapply(att_table$fut.pred[rip.index], '[[', 'var')
    rip.order = order(unlist(rip.data))
    out.tmp <- arrm.downscale(sort(unlist(rip.data)), ds.var, 
                              arrm.model, calc_bias)
    out.list[[out.var]] <- out.tmp$fut_obs[rip.order]
  }
  #return(predict.lm(lm.model, newdata=new))
  return(out.list)
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
             (r_sq[j] <= (mean_rsq-0*sdev_rsq))){
          #The above criteria match at most one point in the vector. However,
          #that one point is about to get removed as part of the next step 
          #carried out. 
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
      #...but there should perhaps be a step to make this more obvious?
      message(paste('no breakpoints found on loop', p))
    }
    k.i <- k.i + 1
  }#end for p in 1:3
  k <- k.out[!is.na(k.out)]
  if(length(k)==0){
    k <- NA
  }
  return(list(breakpoints=k, r_sq=r_sq))
}

arrm.train <- function(hist_pred, hist_targ, args, ds.var){
  #'Assume for now that the dataset merging is done up in a prior step - 
  #'all that you have to worry about in this step is a single 
  #'vector for each dataset. 
  #'Also assume that this data is pre-sorted by rank; the restoration of calendar
  #'order can be done in the main method of the ds method.
  
  #AARM 1: remove bad pairs from the input data based on mismatches in rank
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 428, more or less
  #(not currently on the table for implemenation)
  
  message('Starting AARM step 2: find first set of breakpoints')
  #AARM 2: Piecewise linear regression
  #ref line: 502
  # DETERMINE START POINT FOR FIRST LOOK FOR BREAKPOINTS (40th percentile)
  hist.pred.len <- length(hist_pred)
  start <- round(0.4*hist.pred.len)
  # DETERMINE HIGHEST AND LOWEST VALUES FOR X|P<0.1 AND X|P>0.9 FOR EACH MONTH
  p_low <- round(0.1*hist.pred.len)
  p_high <- round(0.9*hist.pred.len)
  
  # determine length of moving window (5% of data length), diff for each month, and decreasing towards the tails
  #CEW note: this changes to 10% in what I think is the cross-validated code. 
  #window_temp <- (0.05*(max(hist_pred) - min(hist_pred)))
  #CEW: this is not what is present in the ARRM code, but it matches teh intent of the comment
  window_temp <- (0.05*(hist.pred.len)) #Seriously, I don't know what is up with this line
  print(paste("window_temp:"))
  print(summary(window_temp))
  min_window <- 2
  #CEW: okay, they split this into 3 sections: one for quantiles 0-0.1, 
  #one for the middle ('good') range, and one for the quantiles 0.9-1.0
  window_temp_new <- rep(NA, hist.pred.len)
  #for the low tail
  for(j in 1:p_low){
    window_temp_new[j] <- c(min_window + j/p_low*(window_temp-min_window))
  }
  #for most of the distri
  for (j in seq((p_low+1), (p_high-1))){
    window_temp_new[j] <- window_temp
  }
  #   for (j in 1:(hist.pred.len)-p_high[i]+1)){
  #     window_temp_new[j] <- (hist.pred.len-p_high+1)-j+1)/(hist.pred.len-p_high+1)*(window_temp - min_window) + min_window)
  #   }
  
  #   for (j in 1:(length(model_mon[[i]])-p_high[i]+1)){
  #     window_temp_new[[i]] <- c(window_temp_new[[i]],
  #                               ((length(model_mon[[i]])-p_high[i]+1)-j+1)/(length(model_mon[[i]])-p_high[i]+1)*(window_temp[i] - min_window[i]) + min_window[i])
  #   }
  for(j in seq(p_high, hist.pred.len)){
    j.val <- (p_high-j + 1) #should steadily get larger
    #window_temp_new[j] <- ((hist.pred.len-p_high+1)-j.val+1)/(hist.pred.len-p_high+1)*(window_temp - min_window) + min_window
    window_temp_new[j] <- ((hist.pred.len-p_high+1)-j.val+1)/(hist.pred.len-p_high+1)*(window_temp - min_window) + min_window
  }
  
  print('summary of window_temp_new:')
  print(summary(window_temp_new))
  #  stop('are there missing valyes at this point?')
  #for most of the distribution:
  #   window_temp_new[seq((p_low+1), (p_high-1))] <- window_temp
  #   #for the high tail
  #   for(j in seq(p_high, hist.pred.len)){
  #     j.val <- (p_high-j + 1) #should steadily get larger
  #     #window_temp_new[j] <- ((hist.pred.len-p_high+1)-j.val+1)/(hist.pred.len-p_high+1)*(window_temp - min_window) + min_window
  #     window_temp_new[j] <- ((hist.pred.len-p_high+1)-j.val+1)/(hist.pred.len-p_high+1)*(window_temp - min_window) + min_window
  #   }
  print('final call on window_tmp_new')
  print(summary(window_temp_new))
  if(any(is.na(window_temp_new))){
    stop(paste("Error when making windows: check your inputs!"))
  }
  
  #find length of window so that it is always the same span in temperature
  window1 <- rep(NA, (hist.pred.len-start))
  print(summary(start:hist.pred.len))
  #for (j in start:hist.pred.len){
  for (j in 1:(hist.pred.len-start)){
    q <- 0
    val <- hist_pred[j]
    q <- which( hist_pred > (val-window_temp_new[j]) )[1] - 1
    window1[j] <- (j-q)
  }
  print('summary of window1 after first call')
  print(summary(window1))
  print(length(window1))
  #CEW: look at what this does in more detail later; I'm not sure how the resets work 
  window1[which(window1<10)] <- 10
  window1 <- c(rep(NA,(start-1)),window1) #...okay, this makes no sense. missvals give error later.
  #These values don't get referenced - they just seem to be here for padding to make sure
  #that the other indices are in the right place.
  
  #calculate r squared of linear regressions the length of 'window1'
  #CEW: reference line is 550
  r_sq1 <- rep(NA, hist.pred.len-start)
  n <- start
  #window1 issues started around here
  for(j in 1:(hist.pred.len-start)){
    j.win <- (j+start-1)
    sm <- hist_targ[n-(window1[j.win]-1):n]
    ix=which(!is.na(sm))
    sm=sm[ix]
    mm <- hist_pred[n-(window1[j.win]-1):n] #same index as earlier
    mm=mm[ix]
    qrd=qr(matrix(c(rep(1,length(ix)),mm),ncol=2,byrow=F))
    yhat=qr.fitted(qrd,sm)
    mss=sum((yhat-mean(yhat))^2)
    rss=sum((sm-yhat)^2)
    r_sq1[j] <- mss/(mss+rss)
    n <- n+1
    if(is.na(r_sq1[j])){
      print(j)
      stop("This should not be missing!")
    }
  }
  r_sq1_save <- r_sq1
  
  ###Modification 12-18 to make sure that precisely the same
  ###code is being used for both breakpoint calculations
  print(summary(r_sq1))
  print('finding breakpoints')
  bp.1 <- arrm.find.breakpoints(r_sq1, window1, start)
  k <- bp.1$breakpoints
  r_sq1 <- bp.1$r_sq
  
  print('summaries of r_sq1_save')
  print(summary(r_sq1_save)) #note: these are both 1, rather than a lowercase 'l'
  
  print('summary of k:')
  print(k)
  
  
  r_sq_new1 <- r_sq1_save[k]
  k <- k + start - 1
  k_old <- k #why are they calling this k_old if they modify the vals in k?
  
  #CEW: I have no idea what this bit of code is supposed to accomplish, and I 
  #can think of one case where if fails spectacularly (the single NA breakpoint case)
  if(is.na(k[1])){
    for(j in 1:length(k)){
      k[j] <- k[j] - floor(1/2*window1[k[j]])
    }
  }
  
  #AARM 3: find second set of breakpoints in the R code
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 646, more or less
  #################################################################################################
  ###                                                                                           ###
  ### PART 2: THIS PART STARTS AT THE LOWEST BREAK POINT FOUND IN PART 1 AND WORKS ITS WAY DOWN ###
  ###                                                                                           ###
  #################################################################################################
  
  message('Starting AARM step 3: find second set of breakpoints')
  nas.3 <- is.na(k[1])
  if(!nas.3){
    start2 <- min(k,na.rm=T) #most common case
  }else{
    start2 <- start
  }
  if(start2 > length(hist.pred.len)){
    print('setting second start based on hist.pred.len')
    start2 <- hist.pred.len
  }
  
  #Determine windows going in the opposite direction
  window2 <- rep(NA, hist.pred.len)
  for(j in hist.pred.len:1){
    q <- 0
    val <- hist_pred[j]
    q <- which(hist_pred > (val-window_temp_new[j]))[1]-1
    window2[(hist.pred.len-j)+1] <- (j-q)
  }
  window2[which(window2<10)] <- 10
  
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
  
  print('summaries of r_sq2_save')
  print(summary(r_sq2_save))
  print('summary of k2:')
  print(k2)
  
  #save vector with only those months that have breakpoints
  #CEW: I'm not sure that I see the point of this bit of the code. 
  #Check later to see if if has an effect again, after more of the calcs are complete?
  r_sq_new2 <- r_sq2_save[k2] #prev. k.2
  
  #####################
  ###               ###
  ### END OF PART 2 ###
  ###               ###
  #####################
  
  ###There is still more to go on the training section - we haven't got to the 
  ###downscaling yet. 
  
  #AARM 4: combine the breakpoints calculated in parts 1 and 2
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 795, more or less
  
  ##################################################
  ###                                            ###
  ### COMBINE BREAK POINTS FOUND IN PART 1 AND 2 ###
  ###                                            ###
  ##################################################
  
  #cut off break point if it is in the first 20 points or the last 5
  breakpoints0 = rep(NA, 6)
  
  k.indices <- k[!is.na(k) & k > 20 & (k < hist.pred.len-4)]
  breakpoints0 <- k.indices
  r_sq1 <- r_sq_new1[k.indices]
  #check second vector of breakpoints
  k2.indices <- k2[!is.na(k2) & k2 > 20 & (k2 < hist.pred.len-4)]
  r_sq2 <- r_sq_new2[k2.indices]
  
  if(length(breakpoints0)==0){ #there was a second check for nas.2 and nas.4 in here, but I'm not sure it's relevant any more
    breakpoints0 <- NA
  }
  
  breakpoints <- sort(breakpoints0, na.last=T)
  print('summary of breakpoints:')
  print(breakpoints)
  rank <- rank(breakpoints0, na.last=T, ties='first')
  #get the actual values of temp/precip where the break is
  if(length(breakpoints) > 0){
    values <- hist_pred[breakpoints]
  }else{
    values <- NA
    breakpoints <- NA
  }
  
  #AARM 5: Compute the regression model using the breakpoints
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 860, more or less
  
  ###################################################
  ###                                             ###
  ### COMPUTE REGRESSION MODELS WITH BREAK POINTS ###
  ###                                             ###
  ###################################################
  
  #perform regressions
  ds <- data.frame(mod=hist_pred, sta=hist_targ)
  if(!is.na(breakpoints[1])){
    print('Computing piecewise regression')
    regr_piecewise <- lm(sta ~ bs(mod, knots=values,deg=1,Boundary.knots=c(-999,999)),data=ds)
  }else{
    print("No breakpoints found; activating simple lm prediction")
    regr_piecewise <- lm(sta ~ mod, data=ds)
  }
  
  #AARM 5: Compute the regression model using the breakpoints
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 880, more or less
  
  ############################################################################################
  ###                                                                                      ###
  ### CALCULATE SLOPES OF EACH REGR SEGMENT AND TAKE OUT BREAKPOINTS WHEN SLOPES ARE < 0.1 ###
  ###                                                                                      ###
  ############################################################################################
  
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
  
  bad.value.indices <- rep(F, length(values))
  bad.break.indices <- rep(F, length(slopes))
  bad.slope.indices <- rep(F, length(slopes))
  if(!is.na(breakpoints[1])){
    for(j in 1:length(slopes)){
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
  # slope_rank is the rank of bad slopes, so the lowest can be removed first
  slope_rank <- rank(slopes[bad.slope.indices], ties='first')
  values2 <- values[bad.value.indices]
  breakpoints2 <- breakpoints[bad.break.indices]
  
  values_new1 <- values
  breakpoints3 <- breakpoints
  
  for (k in 1:5){# do loop up to 5 times (assuming that one month cannot possibly have that many negative slope segments)
    if(sum(bad.slope.indices)>=1){
      if(length(values_new1)==1)  values_new1 <- NA
      else{
        #Store all points but the single bad value of interest
        values_new1 <- values_new1[values_new1!=values2[slope_rank==1]]
        breakpoints3 <- breakpoints3[breakpoints3!=breakpoints2[slope_rank==1]]
      }
      #Perform regressions again with the first slope taken out
      ds <- data.frame(mod=hist_pred, sta=hist_targ)
      if(!is.na(breakpoints[1])){
        print("Beginning piecewise linear regression")
        regr_piecewise2 <- lm(sta ~ bs(mod, knots=values_new1,deg=1,Boundary.knots=c(-999,999)),data=ds)
      }else{
        print("No breakpoints found; activating simple lm prediction")
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
      for(j in 1:length(xvals2)-1){
        slopes[j] <- (yvals2[j+1]-yvals2[j])/(xvals2[j+1]-xvals2[j]+0.0001)
      }
      bad.value.indices <- rep(F, length(values))
      bad.break.indices <- rep(F, length(slopes))
      bad.slope.indices <- rep(F, length(slopes))
      if(!is.na(breakpoints[1])){
        for(j in 1:length(slopes)){
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
      slope_rank <- rank(slopes[bad.slope.indices], ties='first')
      regr_piecewise <- regr_piecewise2 #regression model
      values <- values_new1             #values of hist_pred at those points
    }#end check for any present bad values
  }#end k loop
  
  ##################
  ###            ###
  ### END SLOPES ###
  ###            ###
  ##################
  
  return(ds.model=regr_piecewise)
  
}#end arrm.train

arrm.downscale <- function(fut_pred, ds.var, regr_piecewise, 
                           calc_bias=FALSE){
  
  #AARM 6: Run the regrssion on the future predictor data.
  #assume that looping by rip is taking place in the ds function's main method.
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 1030, more or less
  
  print('calling predict')
  fut_obs <- predict(regr_piecewise, data.frame(mod=fut_pred))
  regr <- regr_piecewise
  model_future_fit <- fut_obs
  
  ### FIX HIGH PRECIP VALUES BY BIAS FACTOR
  calc_bias=F
  if(calc_bias){
    downsc_train0 <- sort(regr_piecewise$fitted)
    downsc_rank0 <- rank(regr_piecewise$fitted)
    model_future_fit2 <- sort(model_future_fit)
    model_future_fit2_rank <- rank(model_future_fit)
    
    downsc_train <- downsc_train0
    sta_mon_train <- sta_mon_save
    model_future_fit_new <- model_future_fit2
    
    sta_mon_save_new <- sta_mon_train
    q1 <- seq(0.05,0,by=(-0.005))
    
    rank_train <- matrix(NA,nrow=length(q1),ncol=length(downsc_train))
    rank_fut <- matrix(NA,nrow=length(q1),ncol=length(model_future_fit_new))
    for (j in 1:length(q1)){
      rank_train[j,] <- rank(downsc_train)
      downsc_train <- sort(downsc_train)
      rank_fut[j,] <- rank(model_future_fit_new)
      model_future_fit_new <- sort(model_future_fit_new)
      q_val <- quantile(sta_mon_save_new,probs=q1[j])
      if (round(q_val,digits=1)!=round(max(sta_mon_train),digits=1))  q_point <- which(sta_mon_save_new>=q_val)[1]
      if (round(q_val,digits=1)==round(max(sta_mon_train),digits=1))  q_point <- which(sta_mon_save_new<=q_val)[length(which(sta_mon_save_new<=q_val))]
      if (sta_mon_train[q_point]!=0){
        bias <- (downsc_train[q_point] - sta_mon_train[q_point])/sta_mon_train[q_point]}
      if (sta_mon_train[q_point]==0){
        bias <- (downsc_train[q_point] - sta_mon_train[q_point])/downsc_train[q_point]}
      q_val2 <- downsc_train[q_point]
      q_val3 <- which.min(abs(model_future_fit_new-q_val2))
      if ((is.na(bias)==F) & ((bias>0.03) | (bias<(-0.03)))){
        print(paste('q =',q1[j],'; bias =',bias,sep=' '))
        downsc_train[1:q_point] <- downsc_train[1:q_point] / (1+bias)
        model_future_fit_new[1:q_val3] <- model_future_fit_new[1:q_val3] / (1+bias)
      }}
    for (k in 1:length(q1)){
      downsc_train <- downsc_train[rank_train[k,]]
      model_future_fit_new <- model_future_fit_new[rank_fut[k,]]
    }
    
    q2 <- seq(0.95,1.00,by=0.005)
    #    print(names(downsc_train)[[i]])
    rank_train <- matrix(NA,nrow=length(q2),ncol=length(downsc_train))
    rank_fut <- matrix(NA,nrow=length(q2),ncol=length(model_future_fit_new))
    for (j in 1:length(q2)){
      rank_train[j,] <- rank(downsc_train)
      downsc_train <- sort(downsc_train)
      rank_fut[j,] <- rank(model_future_fit_new)
      model_future_fit_new <- sort(model_future_fit_new)
      q_val <- quantile(sta_mon_save_new,probs=q2[j])
      if (round(q_val,digits=1)!=round(max(sta_mon_train),digits=1))  q_point <- which(sta_mon_save_new>=q_val)[1]
      if (round(q_val,digits=1)==round(max(sta_mon_train),digits=1))  q_point <- which(sta_mon_save_new<=q_val)[length(which(sta_mon_save_new<=q_val))]
      if (sta_mon_train[q_point]!=0){
        bias <- (downsc_train[q_point] - sta_mon_train[q_point])/sta_mon_train[q_point]}
      if (sta_mon_train[q_point]==0){
        bias <- (downsc_train[q_point] - sta_mon_train[q_point])/downsc_train[q_point]}
      q_val2 <- downsc_train[q_point]
      q_val3 <- which.min(abs(model_future_fit_new-q_val2))
      if ((is.na(bias)==F) & ((bias>0.015) | (bias<(-0.015)))){
        print(paste('q2 =',q2[j],'; bias =',bias,sep=' '))
        downsc_train[q_point:length(downsc_train)] <- downsc_train[q_point:length(downsc_train)] / (1+bias)
        model_future_fit_new[q_val3:length(model_future_fit_new)] <- model_future_fit_new[q_val3:length(model_future_fit_new)] / (1+bias)
      }}
    for (k in 1:length(q2)){
      downsc_train <- downsc_train[rank_train[k,]]
      model_future_fit_new <- model_future_fit_new[rank_fut[k,]]
    }
    
    model_future_fit3 <- model_future_fit_new
    model_future_fit <- model_future_fit3[model_future_fit2_rank]
    ##INSERT TMASK CODE
    ##SPECIAL CASE FOR TEMP
    if( variable != 'pr'){
      if(tmask =='on'){
        model_future_fit_copy <- model_future_fit
        model_future_fit_copy[which(model_future_fit_copy<(min(sta_mon_save,na.rm=T)-10))] <- NA
        model_future_fit_copy[which(model_future_fit_copy>(max(sta_mon_save,na.rm=T)+10))] <- NA
      }
    }
  }else{
   model_future_fit_copy=NULL
  } #end calc_bias
  return(list(fut_obs=fut_obs, fit=model_future_fit_copy))
}#end arrm.downscale