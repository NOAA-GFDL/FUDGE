#delta_ds.R
#Main and helper functions for downscaling
#with DeltaSD

callDeltaSD <- function(OH,MH,MF, args, train_qcmask, tmasks=list()){
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
    #'  @param oh_offset: by how many years to offset Oh between ensemble members.
    #'  Setting to greater than 0 keeps weather patterns diffrent between ens
    #'  members
    #'Uses the difference (ratio or subtraction) between MF and MH means or medians to calculate
    #' a delta that is applied to the OH (observational) data
    ########################################
    #Note: preferred behavior is to truncate vectors
    #rather than randomly sampling iff too short.

    status=0
    out.data <- array(NA, dim=dim(MF[[1]]))
    nrips = dim(out.data)[4]
    #Are we returning a mask of those points where delta beahved as expected?
    if (train_qcmask){
      if(tmasks$apply_tmasks){
        #This is going to have to be revised for use in the workflow
        qc.data <- mapply(rep, tmasks$MF$masks[[w]], nrips)
      }else{
        qc.data <- rep(1, dim(MF[[1]]))
      }
      dim(qc.data) <- dim(out.data)
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
    if(!tmasks$time_mask_overlap){
      index.mask <- tmasks$MF$masks[[w]]
    }else{
      print("Using non-overlapping index mask")
      index.mask <- tmasks$trim$masks[[w]]
      #index.mask <- tmasks$MF$masks[[w]]
    }
    #Nevermind the offset. KD suggested we pass this thingy in from the XML.
    pr_start <- 1
  if (!('oh_offset' %in% names(args))){
    stop(paste("Error in Delta: As of icing-testing-20180212, oh_offset required in XML!"))
  }else{
    oh_offset <- args$oh_offset
  }
#CEW: 2 successive bugfixes 2018-01-18 ; length(OH[[1]]) instead of length(OH), 
#and no longer over-sampling MH when MF is longer than or equal to MH.
    oh_longer <- length(MF[[1]][,,,1,,]) <= length(OH[[1]])
    for (n in 1:nrips){
      #Determine data to use when indexing against OH
      out.status <- 0
      MF.slice <- lapply(MF, '[', ,,,n,, , drop=F)
      missval.data <- remove.FUDGE.missvals(OH, MH, MF.slice, 
                                            missval.args$minsamplesize, 
                                            missval.args, 
                                            missval.method='distributional')
      OH.loop <- missval.data$OH[[1]]
      MH.loop <- missval.data$MH[[1]]
      MF.loop <- missval.data$MF[[1]]
      #With delta, DS is always effectively possible - all missing in one 
      #dataset does not neccessarily mean that ds can't take place
      if (oh_longer){
        #Speciciations for this were created in main driver script when delta ds is used
        idx <- get.OH.time.index(tmasks$nyears_hist, tmasks$nyears_fut, 
                                 tmasks$hist.year.div, tmasks$fut.year.div, 
                                 start_year= (pr_start + (n-1) * oh_offset) %% tmasks$nyears_hist )
        OH.idx <- idx$index
        pr_start <- idx$end
        delta <- delta.train(MH.loop, MF.loop, deltatype, deltaop, status) #ignore0.Delta
        ds.out <- delta.downscale(delta$delta, OH.loop[OH.idx], deltaop, status)
        #ds is now indexed by a subset of the OH timeseries; if using non-noleap
        #calendars in any dataset, need to adjust the two for concurrent leapdays
        #note:leapday masks are already in global environment as part of setup
      }else{
        idx <- get.OH.time.index(tmasks$nyears_hist, tmasks$nyears_fut, 
                                 tmasks$hist.year.div, tmasks$fut.year.div, 
                                 start_year=1 + (n-1) * oh_offset)
        OH.idx <- idx$index
        start <- idx$end
        delta <- delta.train(MH.loop, MF.loop, deltatype, deltaop, status) #
        ds.out <- delta.downscale(delta$delta, OH.loop[OH.idx], deltaop, status)
      }
      #Note that index.delta includes a step applying the time-trimming mask
      ##print(str(tmasks))
      tmp.out <- index.delta(tmasks$hist.leapmask[OH.idx], 
                             tmasks$fut.leapmask, 
                             tmasks$OH$masks[[w]][OH.idx],
                             index.mask,
                             ds.out,
                             idx$year_div, tmasks$fut.year.div) 
      #For both cases, final case is the same: tmp.out indexed back in
      out.data[,,,n,,] <- tmp.out
      if (train_qcmask){
        qc.out <- rep(1, length(ds.out)) * delta$delta
        tmp.out <- index.delta(tmasks$hist.leapmask[OH.idx], 
                               tmasks$fut.leapmask, 
                               tmasks$OH$masks[[w]][OH.idx],
                               index.mask,
                               qc.out,
                               idx$year_div, tmasks$fut.year.div) 
        qc.data[,,,n,,] <- tmp.out        
      }      
#       if (train_qcmask & missval.data$mv.retry){
#         #the below used to store the status return; for now, to keep 
#         #consistency with the other, more commonly-used training mask
#         #methods, we're replacing that with NA
#         qc.out <- ds.out * NA
#         tmp.out <- index.delta(tmasks$hist.leapmask[OH.idx], 
#                                tmasks$fut.leapmask, 
#                                tmasks$OH$masks[[w]][OH.idx],
#                                index.mask,
#                                qc.out,
#                                idx$year_div, tmasks$fut.year.div) 
#         qc.data[,,,n,,] <- tmp.out
#       }
      #}
    }
    return(list(ds=out.data, qc=qc.data))
}

delta.train <- function(delta.hist, delta.fut, deltatype, deltaop, status){
  #'Separates out the training step of delta downscaling (calculating deltas)
  #'As of 2016-10, used to call delta training once per future rip available
  #'Calls to ignore0.Delta are bing removed, since that option is now included
  #'as part of the precipitation processing options
  #'
  delta.status <- 0
  if (all(is.na(delta.hist)) || all(is.na(delta.fut))){
    print("Insufficient data to calculate a delta at this point; returning OH data")
    delta.status <- 1
    if (deltaop=='add'){
      delta <- 0
    }else{
      delta <- 1
    }
  }else{
    if(deltaop=='add'){
      #Downscale by difference delta
      delta<-do.call(deltatype, list(delta.fut, na.rm=TRUE))-do.call(deltatype, list(delta.hist, na.rm=TRUE))
    }else if(deltaop=='ratio'){
      #original, simpler form - need some checks if we're running without ignore0
      #delta<-do.call(deltatype, list(delta.fut,na.rm=TRUE))/do.call(deltatype, list(delta.hist,na.rm=TRUE))
      #Downscale by percentage delta (never negative, but occasionally NaN)
      delta.fut <- remove.missvals(delta.fut)
      delta.hist <- remove.missvals(delta.hist)
      if (all(delta.hist == 0)){
        print("All 0 in historical; unable to divide, returning delta of 1.")
        delta <- 1
      }
      if (deltatype == 'mean'){
        delta <- (sum(delta.fut) / length(delta.fut)) / (sum(delta.hist) / length(delta.hist)) 
      }else if (deltarype == 'median'){
        stop(paste("Error in Delta: median is not currently considered for "))
      }
      #Loud warning message for divide-by-0 case
      if(is.nan(delta)||is.infinite(delta)||is.na(delta)){
        message(paste("Warning in delta.downscale: Calculated delta with deltaop", deltaop, 
                      "is either NaN or Inf and cannot be processed normally.",
                      "Returning delta of 1 for the calculation."))
        #Add a value flagging operation at this point
        delta <- 1
        delta.status <- 1
      }
    }
  }
  return(list(delta=delta, status=delta.status))
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
