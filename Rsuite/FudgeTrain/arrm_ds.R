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
  
  #check for presence of check_bias in args
  if(!'calc_bias'%in%names(args)){
    args$calc_bias <- FALSE
  }else{
    if(args$calc_bias=='on'){args$calc_bias<-T}else{args$calc_bias<-F}
  }
  #Add similar check for the beginning qc steps
  if(!'remove_points'%in%names(args)){
    args$remove_points <- FALSE
  }else{
    if(args$remove_points=='on'){args$remove_points<-T}else{args$remove_points<-F}
  }
  #add check for presence of round option
  if(!'round'%in%names(args)){
    round.ds <- FALSE
  }else{
    #assume any string argument is false
    if(!is.numeric(args$round)){round.ds<-F}else{round.ds<-T}
  }
  if(!'pool_predictors'%in%names(args)){
    args$pool_predictors <- TRUE
  }else{
    if(args$pool_predictors=='on'){args$pool_predictors<-T}else{args$pool_predictors<-F}
  }
  if(!'bias_method'%in%names(args)){
    args$bias_method <- 'old'
  }
  
  #Now, call the aarm training code
  #CEW: the missing value removal step might not be neccessary. 
  #Removing on 12-24 to see if it solves the pm2weeks predictor
  #returning output identical to the 
  hist.pred.tmp <- unlist(hist_pred) #[!is.na(unlist(hist_pred))]
  hist.targ.tmp <- unlist(hist_targ) #[!is.na(unlist(hist_targ))]
  arrm.train.data <- arrm.train(sort(hist.pred.tmp[!is.na(hist.pred.tmp)]), 
                           sort(hist.targ.tmp[!is.na(hist.targ.tmp)]), args, ds.var)
  arrm.model <- arrm.train.data$ds.model
  #arrm.targ <- arrm.train.data$hist.pred
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
    rip.tmp <- unlist(rip.data)
    rip.len = length(rip.tmp[!is.na(rip.tmp)]) 
    if(args$pool_predictors){
      model_combine_data <- c(rip.tmp[!is.na(rip.tmp)], 
                              hist.pred.tmp[!is.na(hist.pred.tmp)])
    }else{
      model_combine_data <- rip.tmp[!is.na(rip.tmp)]
    }
    rip.rank <- rank(model_combine_data)
    model_combine_data <- sort(model_combine_data)
    #call downscaling function
    out.tmp <- arrm.downscale(model_combine_data, 
                              #hist.targ.tmp[!is.na(hist.targ.tmp)],
                              sort(hist.targ.tmp[!is.na(hist.targ.tmp)]), 
                              #arrm.train.data$hist.pred,
                              ds.var,
                              arrm.model, args)
    if(!args$calc_bias){
      #out.list[[out.var]] <- out.tmp$fut_obs[rip.rank]
      rip.tmp <- out.tmp$fut_obs
    }else{
      #out.list[[out.var]] <- out.tmp$fit[rip.rank]
      rip.tmp <- out.tmp$fit
      print('modified values returned')
    }
    #round reference: close to line 1180
    if(round.ds){
      message('rounding downscaled values')
      out.list[[out.var]] <- round(rip.tmp[rip.rank][1:rip.len], digits=args$round)
    }else{
      out.list[[out.var]] <- rip.tmp[rip.rank][1:rip.len]
    }
  }
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
  message('starting ARRM step 1: removing bad target/predictor pairs based on rank')
  remove_points=args$remove_points
  if(remove_points){
    hist.pred.len <- length(hist_pred)
    bad_pairs <- rep(F, hist.pred.len)
    dt_model_list_begin <- rep(NA_real_, hist.pred.len)
    dt_sta_list_begin <- rep(NA_real_, hist.pred.len)
    
    for(j in 1:100){
      dt_mod <- hist_pred[j+1] - hist_pred[j]
      dt_sta <- hist_targ[j+1] - hist_targ[j] + 0.01  #add 0.01 otherwise two days with equal value will have dT[sta]=0, 
      #and any dT[mod] will be infinitely larger
      #CEW: why is this more likely in the obs data than the GCM data?
      if (dt_mod > (50 * dt_sta)){
        bad_pairs[j] <- T
        dt_model_list_begin[j] <- dt_mod
        dt_sta_list_begin[j] <- dt_sta
      }
    }
    
    dt_model_list_end <- rep(NA_real_, hist.pred.len)
    dt_sta_list_end <- rep(NA_real_, hist.pred.len)
    
    for(j in hist.pred.len:(hist.pred.len-99)){
      dt_mod <- hist_pred[j] - hist_pred[j-1]
      dt_sta <- hist_targ[j] - hist_targ[j-1] + 0.01  #add 0.01 otherwise two days with equal value will have dT[sta]=0, 
      if (dt_mod > (50 * dt_sta)){
        bad_pairs[j] <- T
        dt_model_list_end[j] <- dt_mod
        dt_sta_list_end[j] <- dt_sta
      }
    }
    if ((hist_pred[hist.pred.len]-hist_pred[hist.pred.len-1])>5){
      bad_pairs[hist.pred.len] <- T
    }
    
    #...not sure I understand the point of these lines at the moment
    if (length(which(bad_pairs==1)>1)){
      bad_pairs[-which(bad_pairs==1)[1]]
    }
    if (length(which(bad_pairs==length(hist_targ))>1)){
      bad_pairs[-which(bad_pairs==length(hist_targ))[1]]
    }
    #finally, remove the 'bad' values
    if(sum(bad_pairs) > 0){
      print(paste('removing', sum(bad_pairs), 'bad pairs from input sample'))
      hist_targ <- hist_targ[-bad_pairs]
      hist_pred <- hist_pred[-bad_pairs]
    }
  }

  message('Starting AARM step 2: find first set of breakpoints')
  #AARM 2: Find first set of breakpoints for the piecewise linear regression
  #ref line: 502
  
  #According to the paper (near start of section 2.4), there are 4 fixed quantities
  #that the method uses. These may need to be changed depending on the variable
  #being downscaled. 
  fixed_window_width = 0.05 #was set to 0.05 for temperature and 0.1 for pr
  pmin = 0.1; pmax=0.9 #min and max probabilities over which a fixed window width is used
  min_window_width = 2  #2 degrees celsius or 10 points, whichever is larger
                        #pr uses 5 % of mean or 10 points, whichever is larger
  min_window_points = 10 #There have to be at least this many points in the sample
  #...max breakpoints is going to be a bit trickier to set (for one, it needs
  #to be divisible by 2), but can also be set if that is desired.
  max_breakpoints=6/2
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
  #window_temp <- (0.05*(hist.pred.len)) #Seriously, I don't know what is up with this line
  window_temp <- (0.05*(max(hist_pred,na.rm=T)-min(hist_pred,na.rm=T)))
  min_window <- 2
  #CEW: okay, they split this into 3 sections: one for quantiles 0-0.1, 
  #one for the middle ('good') range, and one for the quantiles 0.9-1.0
  window_temp_new <- rep(NA, hist.pred.len)
  #for the low tail
  for (j in 1:p_low){
    window_temp_new[j] <- c(min_window + j/p_low*(window_temp-min_window))
  }
  #for most of the distribution
  for (j in (p_low+1):(p_high-1)){
    window_temp_new[j] <- window_temp
  }
  #for the high tail
  j.val <- p_high
  for(j in 1:(hist.pred.len-p_high+1) ){
  #for(j in seq(p_high, hist.pred.len)){
    #window_temp_new[j] <- ((hist.pred.len-p_high+1)-j.val+1)/(hist.pred.len-p_high+1)*(window_temp - min_window) + min_window
    #window_temp_new[j] <- ((hist.pred.len-p_high+1)-j.val+1)/(hist.pred.len-p_high+1)*(window_temp - min_window) + min_window
    window_temp_new[j.val] <- ((hist.pred.len-p_high+1)-j+1)/(hist.pred.len-p_high+1)*(window_temp - min_window) + min_window
    j.val <- j.val + 1
  }  
#    print('summary of window_temp_new:')
#    print(summary(window_temp_new))
#   print(which(window_temp_new == max(window_temp_new)))
#   print(length(window_temp_new))
  if(any(is.na(window_temp_new))){
    stop(paste("Error when making windows: check your inputs!"))
  }
  
  #find length of window so that it is always the same span in temperature
  window1 <- rep(NA, (hist.pred.len-start+1))
  #for (j in start:hist.pred.len){
  for (j in start:hist.pred.len){
    j.val <- (j-start + 1)
    q <- 0
    val <- hist_pred[j]
    q <- which( hist_pred > (val-window_temp_new[j]) )[1] - 1
    window1[j.val] <- (j-q)
  }
  #CEW: look at what this does in more detail later; I'm not sure how the resets work 
#   print('window1')
#   print(summary(window1))
  if(any(is.na(window1))){
    stop('something is off about this add')
  }
  window1[which(window1<10)] <- 10
  window1 <- c(rep(NA,(start-1)),window1) #...okay, this makes no sense. missvals give error later.
  #These values don't get referenced - they just seem to be here for padding to make sure
  #that the other indices are in the right place.

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
  
  ###Modification 12-18 to make sure that precisely the same
  ###code is being used for both breakpoint calculations
  print('finding breakpoints')
#   print('summary of r_sq1')
#   print(summary(r_sq1))
  bp.1 <- arrm.find.breakpoints(r_sq1, window1, start)
  k <- bp.1$breakpoints
  r_sq1 <- bp.1$r_sq
  
#   print('summary of r_sq1_save')
#   print(summary(r_sq1_save)) #note: these are both 1, rather than a lowercase 'l'

  r_sq_new1 <- r_sq1_save[k]
  k <- (k + start - 1)
  k_old <- k #why are they calling this k_old if they modify the vals in k?
  
  #CEW: I have no idea what this bit of code is supposed to accomplish, and I 
  #can think of one case where if fails spectacularly (the single NA breakpoint case)
#   print('summary of k before adjust:')
#   print(k)

  if(!is.na(k[1])){
    for(j in 1:length(k)){
      k[j] <- k[j] - floor(1/2*window1[k[j]])
    }
  }
#   print('summary of k:')
#   print(k)
  
  #AARM 3: find second set of breakpoints in the R code
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 646, more or less  
  message('Starting AARM step 3: find second set of breakpoints')
  nas.3 <- is.na(k[1])
  if(!is.na(k[1])){
    print('deriving k from the min of the previous breakpoints')
    start2 <- min(k,na.rm=T) #most common case
  }else{
    start2 <- start
  }
  if(start2 > hist.pred.len){
    print('setting second start based on hist.pred.len')
    start2 <- hist.pred.len
  }
  print(paste('k2 starts from', start2))
  
  #Determine windows going in the opposite direction
  window2 <- rep(NA, hist.pred.len)
  for(j in hist.pred.len:1){
    q <- 0
    val <- hist_pred[j]
    q <- which(hist_pred > (val-window_temp_new[j]))[1]-1
    window2[(hist.pred.len-j)+1] <- (j-q)
  }
  window2[which(window2<10)] <- 10
#   print('summary of window2:')
#   print(summary(window2))
  
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
  
    #print('summary of k2 before the adjust:')
    #print(k2) 
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
  message('Starting AARM step 4: combining the lists of breakpoints')
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
  #print('summary of breakpoints:')
  #print(breakpoints)
  if(length(breakpoints)==6){
    print('there is at least one case where you get all 6 breakpoints')
  }
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
  message('Starting AARM step 5: combining the lists of breakpoints')
  #perform regressions
  ds <- data.frame(mod=hist_pred, sta=hist_targ)
  if(!is.na(breakpoints[1])){
    print(paste('Computing piecewise regression with knots', paste(values, collapse=",")))
    regr_piecewise <- lm(sta ~ bs(mod, knots=values,deg=1,Boundary.knots=c(-999,999)),data=ds)
  }else{
    print("No breakpoints found; activating simple lm prediction")
    regr_piecewise <- lm(sta ~ mod, data=ds)
  }
  
  #AARM 6: Go through the model and remove slopes close to 0
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 880, more or less
  message('Starting AARM step 6: removing model slopes close to 0')
  
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
      print('there is at least one point where the bad slope indices are activated')
      if(length(values_new1)==1)  values_new1 <- NA
      else{
        #Store all points but the single bad value of interest
        #CEW edit 12-13: == not working for values removal; adding .eps for now
        eps = .Machine['double.eps'][[1]]*2
        values_new1 <- values_new1[abs(values_new1 - values2[slope_rank==1]) <= eps]
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
      slopes <- rep(NA, (length(xvals2)-1)) #added to address indexing error
      for(j in 1:(length(xvals2)-1)){
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
      #Added 12-22, to try and catch the breakpoint differences
      values2 <- values[bad.value.indices]
      breakpoints2 <- breakpoints[bad.break.indices]
      
      regr_piecewise <- regr_piecewise2 #regression model
      values <- values_new1             #values of hist_pred at those points
    }#end check for any present bad values
  }#end k loop
  
  # Code for diagnostic plotting; no longer used
#  hist.predict <- predict(regr_piecewise, data.frame(mod=hist_pred))
#   ybounds=range(c(hist.predict, hist_targ))
#   plot(seq(1:hist.pred.len), 
#        hist_pred, col='orange', main="DS vs fitted vs fut model", cex=2, ylim=ybounds)
#   lines(seq(1:hist.pred.len), hist_targ, col='red', lwd=5)
#  lines(seq(1:hist.pred.len), hist.predict, type='l', col='blue')
#   print('length of fit:')
#   print(length(regr_piecewise$fit))
  return(list(ds.model=regr_piecewise, sta_mon_save=hist_targ))
  
}#end arrm.train

arrm.downscale <- function(fut_pred, hist_targ, ds.var, regr_piecewise, 
                           args){
  
  #AARM 6: Run the regression on the future predictor data.
  #assume that looping by rip is taking place in the ds function's main method.
  #orig file: '~/Code/fudge/main/code/ARRM_2013/tasmax/noxval/Rcode/gfdl.temp.grid.autoinput.dscode.r'
  #ref: line 1030, more or less
  
  print('calling predict')
  #points(seq(1:length(fut_pred)), fut_pred, col='red')
  fut_obs <- predict(regr_piecewise, data.frame(mod=fut_pred))
  regr <- regr_piecewise
  model_future_fit <- fut_obs
  
  ### FIX HIGH PRECIP VALUES BY BIAS FACTOR
  if(args$calc_bias){
    print('fixing high values via bias correction')
    downsc_train <- sort(regr_piecewise$fitted)
    downsc_rank <- rank(regr_piecewise$fitted)
    model_future_fit2 <- sort(model_future_fit)
    model_future_fit2_rank <- rank(model_future_fit)
    
    #sta_mon_train <- hist_targ
    #model_future_fit_new <- model_future_fit2
    
    #sta_mon_save_new <- sta_mon_train
    
    #new_bias_method=T
    q1 <- seq(0.05,0,by=(-0.005))
    q2 <- seq(0.95, 1.00, by=0.005)
    bias.low.max = 0.03
    bias.low.min = -0.03
    bias.high.max = 0.015
    bias.high.min = -0.015
    tmp.out <- arrm.bias.correct(downsc_train, downsc_rank, 
                                 model_future_fit2, model_future_fit2_rank, 
                                 q1, #sort(hist_targ), 
                                 hist_targ, bias.low.max, bias.low.min, 'low')
    tmp.out <- arrm.bias.correct(sort(tmp.out$train), rank(tmp.out$train), 
                                 sort(tmp.out$ds), rank(tmp.out$ds), 
                                 q2, hist_targ, 
                                 #sort(hist_targ), 
                                 bias.high.max, bias.high.min, 'high')
    model_future_fit_copy = tmp.out$ds[model_future_fit2_rank]

    model_future_fit3 <- model_future_fit_new
    model_future_fit <- model_future_fit3[model_future_fit2_rank]
    model_future_fit_copy <- model_future_fit
    ##INSERT TMASK CODE
    ##SPECIAL CASE FOR TEMP
    if( ds.var != 'pr'){
      tmask = 'on' #I'm not sure what this parameter does
      if(tmask =='on'){
        model_future_fit_copy <- model_future_fit
        model_future_fit_copy[which(model_future_fit_copy<(min(hist_targ,na.rm=T)-10))] <- NA
        model_future_fit_copy[which(model_future_fit_copy>(max(hist_targ,na.rm=T)+10))] <- NA
      }
    }
    #lines(seq(1:length(fut_pred)), model_future_fit, type='l', col='violet')
  }else{
   model_future_fit_copy=NULL
  } #end calc_bias
  return(list(fut_obs=fut_obs, fit=model_future_fit_copy))
}#end arrm.downscale

arrm.bias.correct <- function(trained_vals, trained_rank, ds_vals, ds_rank, qtile_vals, hist_targ, 
                              bias.max, bias.min, tail='low'){
  #'trained_vals, trained_rank: the rank and downscaled values, respectively, 
  #'  of the downscaled training data, returned from a model
  #'ds_vals, ds_rank: the rank and downscaled values of the new esd.gen/future
  #'  predictor data, returned from using the model on new data
  #'  
  #'hist_targ: the historic target data that was used for the training step.  

  #set looped vals
  rank_train <- matrix(NA,nrow=length(qtile_vals),ncol=length(trained_vals))
  rank_ds <- matrix(NA,nrow=length(qtile_vals),ncol=length(ds_vals))
  
  for(j in 1:length(qtile_vals)){
    rank_train[j,] <- rank(trained_vals)
    trained_vals <- sort(trained_vals)
    rank_ds[j,] <- rank(ds_vals)
    ds_vals <- sort(ds_vals)
    #
    q_val <- quantile(hist_targ, probs=qtile_vals[j])
    if(round(q_val, 1)!=round(max(hist_targ), digits=1)){
      q_point <- which(hist_targ >= q_val)[1] #first element
    }else{ #If they are equal
      q_point <- tail(which(hist_targ <= q_val), n=1) #tail selects last element
      #q_point <- which(hist_targ <= q_val)[length(which(hist_targ <= q_val))]
    }
    if(hist_targ[q_point]!=0){
      bias <- (trained_vals[q_point] - hist_targ[q_point])/hist_targ[q_point]
    }else{
      bias <- (trained_vals[q_point] - hist_targ[q_point])/trained_vals[q_point]
    }
    q_val2 <- trained_vals[q_point]
    q_val3 <- which.min(abs(ds_vals-q_val2))
    if(!is.na(bias) & (bias > bias.max | bias < bias.min)){ #Check moved up further to keep seq creation from giving error for this case
      if(tail=='low'){
        tr_seq <- 1:q_point
        ds_seq <- 1:q_val3
      }else if(tail=='high'){
        tr_seq <- length(trained_vals):q_point
        ds_seq <- length(ds_vals):q_val3
      }
      #if(){ #moved missing bias check
        #print(paste("q =", qtile_vals[j], "; bias =", bias))
        trained_vals[tr_seq] <- trained_vals[tr_seq] / (1+bias)
        ds_vals[ds_seq] <- ds_vals[ds_seq] /(1+bias)
      #}
    }else{
      message('Bias of NA; no change made') #...surprisingly common error.
    }
  }
  for(k in 1:length(qtile_vals)){
    #successive rank steps to restore the new order
    trained_vals <- trained_vals[rank_train[k,]]
    ds_vals <- ds_vals[rank_ds[k,]]
  }
  return(list('train'=trained_vals, 'ds'=ds_vals))
}