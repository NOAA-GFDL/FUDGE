#delta_ds.R
#Main and helper functions for downscaling
#with DeltaSD

callDeltaSD <- function(OH,MH,MF,args, train_qcmask, status=0, tmasks=list()){
  #'@author carlos.gaitan@noaa.gov
    #'@description The script uses the Delta Method to downscale coarse res. climate variables  
    #over a single variable
    #'@param OH: Local Historical (a.k.a. observations)
    #'@param MH: Coarse Historical (a.k.a. GCM historical)
    #'@param MF: Coarse Future (a.k.a GCM future)
    #'@param args: A list containing three arguments: 
    #'  @param deltatype, one of 'mean' or 'median', which will be used to determine 
    #'  what single value will be used for the difference between the MH and MF
    #'  @param deltaop, one of 'ratio' or 'add', which will be used to determine 
    #'  the mthod for calculating the delta
    #'  @param ignore0.Delta, a value of 'on' or 'off' that controls whether to use all days for
    #'  calculating the delta ('off'), or use only those days for which the delta was greater
    #'  than zero ('on'). At this point it's a legacy setting; the main ignore0 setting in 
    #'  the precipiation options sets 0 to missing 
    #'Uses the difference (ratio or subtraction) between MF and MH means or medians to calculate
    #' a delta that is applied to the OH (observational) data
    #'@return SDF: Downscaled Future (Local)
    ########################################
    #Note: preferred behavior is to truncate vectors
    #rather than randomly sampling iff too short.
    
    #Use this rather than unlist - unlist removes some dimension data
    OH = OH[[1]]
    MH = MH[[1]]
    MF = MF[[1]]
    out.data <- array(NA, dim=dim(MF))
    nrips = dim(out.data)[4]
    #Are we returning a mask of those points where delta beahved as expected?
    if (train_qcmask){
      qc.data <- array(1, dim=dim(MF))
    }else{
      qc.data <- NULL
    }
    # Obtain options
    if(!is.null(args$deltatype)){
      deltatype <- args$deltatype
    }else{
      stop(paste("DeltaSD Downscaling Error: deltatype not found in args"))
    }
    if(!is.null(args$deltaop)){
      deltaop <- args$deltaop
    }else{
      stop(paste("DeltaSD Downscaling Error: deltaop not found in args"))
    }
    if(!is.null(args$ignore0.Delta)){
      ignore0.Delta <- ifelse(args$ignore0.Delta=='on', TRUE, FALSE)
    }else{
      stop(paste("DeltaSD Downscaling Error: ignore0.Delta not found in args"))
    }
    if(!tmasks$time_mask_overlap){
      index.mask <- tmasks$MF$masks[[w]]
    }else{
      index.mask <- tmasks$trim$masks[[w]]
    }
    start <- 1
    oh_longer <- length(MF[,,,1,,]) <= length(OH)
    for (n in 1:nrips){
      #Determine data to use when indexing against OH
      out.status <- 0
      MF.data <- MF[,,,n,,]
      if (oh_longer){
        #Speciciations for this were created in main driver script when delta ds is used
        MF.idx <- 1:length(MF)
        if ((tmasks$hist.year.div[tmasks$nyears_fut*n]) <= tmasks$hist.year.div[length(tmasks$hist.year.div)]){
          print(tmasks$nyears_fut)
          end <- start + tmasks$nyears_fut - 1
          OH.idx <- start:tmasks$hist.year.div[tmasks$nyears_fut * (n)]
          start <- 1 + tmasks$hist.year.div[tmasks$nyears_fut * (n)]
        }else{
          #You have an odd modulo; wrap around to the beginning of OH
          #...try to find a case that checks this logic.
#           print('the weird case is triggered')
#           print(n)
#           print(start)
#           print(start + hist.year.div[nyears_fut * n])
          year_diff = tmasks$nyears_hist - start
          OH.idx <- c(tmasks$hist.year.div[1]:tmasks$hist.year.div[year_diff], 
                      tmasks$hist.year.div[start]:tmasks$hist.year.div[length(tmasks$hist.year.div)])
        }
        delta <- delta.train(MH, MF.data, deltatype, deltaop, ignore0.delta, status)
        ds.out <- delta.downscale(delta, OH[OH.idx], deltaop, status)
        #ds is now indexed by a subset of the OH timeseries; if using non-noleap
        #calendars in any dataset, need to adjust the two for concurrent leapdays
        #note:leapday masks are already in global environment as part of setup
      }else{
        #Otherwise, if the vectors are uneven then calculate n+1 deltas, 
        #where n=length(MF.data)/length(OH)
        write.len <- 1
        out.len <- length(MF.data)
        in.len <- length(OH)
        OH.idx <- c()
        ds.out <- c()
        #vector comparisons take a long time relative to other things
        while(write.len <= out.len){
          if((write.len + in.len) <= out.len){
            MF.idx <- write.len:(write.len+in.len)
            loop.delta <- delta.train(MH, MF.data[MF.idx], 
                                      deltatype, deltaop, ignore0.Delta, status)
            #Insert status check here
            loop.ds <- delta.downscale(loop.delta, OH, deltaop, status)
            OH.idx <- c(OH.idx, 1:length(OH))
            ds.out <- c(ds.out, loop.ds)
            #Insert second status check here?
          }else{
            len.diff <- out.len - write.len + 1
            hist.idx <- 1:len.diff
            MF.idx <- write.len:out.len
            #This needs an elese case for the case where there is *more* OH data
            #than MF data at a given point.
            #...though maybe not if we are using entire vectors rather than compressed
            in.len <- len.diff
            loop.delta <- delta.train(MH[hist.idx], MF.data[MF.idx], 
                                      deltatype, deltaop, ignore0.Delta, status)
            #Insert status check here
            loop.ds <- delta.downscale(loop.delta, OH[hist.idx], deltaop, status)
            OH.idx <- c(OH.idx, hist.idx)
            ds.out <- c(ds.out, loop.ds)
            #Insert second status check here?
          }
        }
      }
      out.data[,,,n,,] <- index.delta(tmasks$hist.leapmask[OH.idx], 
                                      tmasks$fut.leapmask, 
                                      tmasks$OH$masks[[w]][OH.idx],
                                      index.mask,
                                      ds.out) 
                                      #MF.data)
      if(out.status > 0){
        #As of 10-14-16, replacing status setting with qcmask
        if (train_qcmask){
          #the below used to store the status return; for now, to keep 
          #consistency with the other, more commonly-used training mask
          #methods, we're replacing that with NA
          qc.data[,,,n,,] <- NA 
        }
      }
    }
    return(list(ds=out.data, qc=qc.data))
}

delta.train <- function(delta.hist, delta.fut, deltatype, deltaop, ignore0.Delta, status){
  #'Separates out the training step of delta downscaling (calculating deltas)
  #'As of 2016-10, used to call delta training once per future rip available
  #'Calls to ignore0.Delta are bing removed, since that option is now included
  #'as part of the precipitation processing options
  #'
  if (all(is.na(delta.hist)) || all(is.na(delta.fut))){
    print("Insufficient data to calculate a delta at this point; returning OH data")
    if (deltaop=='add'){
      return(0)
    }else{
      return(1)
    }
  }
  
  if(deltaop=='add'){
    #Downscale by difference delta
    delta<-do.call(deltatype, list(delta.fut, na.rm=TRUE))-do.call(deltatype, list(delta.hist, na.rm=TRUE))
  }else if(deltaop=='ratio'){
    #Downscale by percentage delta (never negative, but occasionally NaN)
    delta<-do.call(deltatype, list(delta.fut,na.rm=TRUE))/do.call(deltatype, list(delta.hist,na.rm=TRUE))
    #Loud warning message for divide-by-0 case
    if(is.nan(delta)||is.infinite(delta)|| is.na(delta)){
      message(paste("Warning in delta.downscale: Calculated delta with deltaop", deltaop, 
                    "is either NaN or Inf and cannot be processed normally.",
                    "Returning delta of 1 for the calculation."))
      #Add a value flagging operation at this point
      delta <- 1
      #       out <- delta.targ
      #       attr(out, 'status') <- 1
      #       return(out)
    }
  }
  return(delta)
}

delta.downscale <- function(delta, delta.targ, deltaop, status){
  #Separates out the downscaling function for the delta method.
  #Adds or multiples targ by delta; passed-in status have separate 
  #effects.
  if (status > 0){
    #somethign went wrong with training or minsamplesize; return OH unchanged
    return(delta.targ)
  }
  if (deltaop=='add'){
    ds = delta.targ + delta
  }else if (deltaop=='ratio'){
    ds = delta.targ*delta
  }
  return(ds)
}

reconcile.hist.fut.leapdays <- function(hist_mask, fut_mask, hist_data, fut_data){
  #Using pre-calcualted masks of Febrary 29ths, 
  #adds historical data to the same calendar pattern
  #as the future data, removing leapdays in hist when they
  #do ot synMH and doubling Feb. 28 when there are leapdays
  #in future not present in hist
  #Assumes that missing values have not been removed from vectors
  #and that your historic and future datasets can be resolved to the
  #same length
  out <- fut_data
  #get normal days in both datasets
  hist_days <- (!is.na(hist_mask) & !is.na(hist_data))
  fut_days <- (!is.na(fut_mask) & !is.na(fut_data))
  #select elapdays in future
  fut_leapdays <- which(is.na(fut_mask))
  #   print('before summary calls:')
  #   print(summary(hist_data))
  #   print(summary(hist_days))
  #   print('hist days selection:')
  #   save(file="/home/cew/Code/testing/test_out.R", 
  #        list=c('hist_data', 'fut_data','hist_mask', 'fut_mask', 'hist_days', 'fut_days'))
  print(summary(hist_data[hist_days]))
  print(sum(!is.na(hist_data)))
  print(sum(!is.na(fut_data)))
  print('length of hist days and fut days:')
  print(rle(as.numeric(hist_days))$lengths)
  print(rle(as.numeric(fut_days))$lengths)
  print(length(hist_days))
  print(length(fut_days))
  day_diff = sum(hist_days) - sum(fut_days)
  if (day_diff!=0){
    #If there are uneven numbers of leap days in the sample
    print(sum(hist_days))
    print(sum(fut_days))
    print(mode(hist_days))
    print(mode(fut_days))
    print(day_diff)
    stop('examine current output!')
    if (day_diff < 0){
      #if there are more nonleap days in future, repeat last sample in historical
      hist_data <- c(hist_data, tail(hist_data, abs(day_diff)))
    }else{
      #If there are fewer nonleap days in future, remove last sample in historical
      hist_data <- hist_data[1:(length(hist_data)-day_diff)]
    }
  }
  out[fut_days] <- hist_data[hist_days]
  #Repeat Feb.28 for Feb. 29
  print(length(fut_leapdays))
  print(length(out[fut_leapdays]))
  print(length(out[(fut_leapdays-1)]))
  # print(summary(out))
  out[fut_leapdays] <- out[(fut_leapdays-1)]
  # print(summary(out))
  # stop('examine out')
  return(out)
}

delta.downscale.old <- function(delta.targ, delta.hist, delta.fut, deltatype, deltaop, 
                                ignore0.Delta=FALSE, status=0){
  #Calculates a delta after removing NAs and applies it to a target vector.
  #Helper method for callDeltaSD, but might be used elsewhere.
  if(status > 0){
    if(status==120){ #check remove.delta.missvals for this value
      message('returning OH data without delta')
      return(delta.targ[1:length(delta.fut)])
    }
  }  
  #Make sure that there are no NA values in the current vector
  delta.fut.orig.len <- length(delta.fut)
  delta.fut <- delta.fut[!is.na(delta.fut)]
  delta.targ <- delta.targ[!is.na(delta.targ)]
  delta.hist.temp <- delta.hist[!is.na(delta.hist)]
  if(ignore0.Delta){
    delta.fut <- delta.fut[delta.fut!=0]
    delta.targ <- delta.targ[delta.targ!=0]
    delta.hist.temp <- delta.hist.temp[delta.hist.temp!=0]
  }
  if(length(delta.hist)==0){
    if(deltaop=='ratio'){
      #Return the OH data
      message(paste("Insufficent historical predictor (MH) data when used with the ratio deltatype;", 
                    "returning target values without the delta"))
      out <- delta.targ
      attr(out, 'status') <- 1
      return(out)
      #When the options are enabled properly, flag this and return a qc mask
    }else{
      #message("Adding ignore0.Delta correction for MH data: zero returned as MH value for delta calculation")
      delta.hist <- 0
    }
  }else{
    delta.hist <- delta.hist.temp
  }
  if(length(delta.fut)==0){
    #No need to check for all missing values on this; the effect is the same in either case
    #message("Adding ignore0.Delta correction for the MF data: zero returned as MF for delta calculation")
    delta.fut <- 0
  }
  #The na.rms are important in this step - it's possible, if there are more OH than MH samples in the case
  #where the future time period is longer than the historical time period, for missing values to get passed
  #into the MH dataset. 
  if(deltaop=='add'){
    #Downscale by difference delta
    delta<-do.call(deltatype, list(delta.fut, na.rm=TRUE))-do.call(deltatype, list(delta.hist, na.rm=TRUE))
    out <- delta.targ + delta #MHANGED FROM delta.fut
  }else if(deltaop=='ratio'){
    #Downscale by percentage delta (never negative, but occasionally NaN)
    delta<-do.call(deltatype, list(delta.fut,na.rm=TRUE))/do.call(deltatype, list(delta.hist,na.rm=TRUE))
    #Loud warning message for divide-by-0 case
    if(is.nan(delta)||is.infinite(delta)|| is.na(delta)){
      message(paste("Warning in delta.downscale: Calculated delta with deltaop", deltaop, 
                    "is either NaN or Inf and cannot be processed normally.",
                    "Returning target data values without delta adjustment applied."))
      #Add a value flagging operation at this point
      out <- delta.targ
      attr(out, 'status') <- 1
      return(out)
    }
    out <- delta.targ*delta 
  }else{
    stop(paste("delta.downscale Downscaling Error: deltaop", deltaop, "is not one of 'ratio' or 'add'"))
  }
  return(out)
}