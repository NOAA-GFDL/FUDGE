
count.all.present.xy <- function(dataset){
  #Returns the number of non-missng values at each x,y,ens point
  return(as.vector(apply(dataset, c(1,2,4), 
                         function(x){
                           sum(!is.na(x))
                         })
  ))
}

which.missing.xy <- function(dataset){
  #Returns the number of non-missng values at each x,y,ens point
  return(as.vector(apply(dataset, c(1,2,4), 
                         function(x){
                           sum(!is.na(x))
                         })
  ))
}

missval.xyens <- function(x){
  #   #x is a 6-dimensional dataset
  #   tmp.dat <- apply(x, c(1,2,3), 
  #                function(z){is.na(z)})
  #   #Time is now first dimension
  #   time.dat <- apply(tmp.dat, c(1), sum)
  #...y'know, it might be faster to just sum the above vector
  #and THEN do the check for is.na
  tmp.dat <- apply(x, c(6), 'sum')
  return(is.na(tmp.dat))
}


remove.FUDGE.missvals <- function(OH, MH, MF, 
                                  missval.thold, missval.args, 
                                  missval.method='distributional', 
                                  return_qcmask = F){
  #Okay, so this should not technically remove missing values - 
  #you still need the extra missvals in place in order to index
  #your output vectors.
  missval.status <- 0
  #Evaluate back in the current environment (delta can affect status)
  switch(missval.method, 
         'distributional' = place.dist.missvals(OH, MH, MF), 
         'synchronous' = place.synch.missvals(OH, MH, MF), 
         'cdft' = place.dist.missvals(OH, MH, MF), #does NOT need its own method
         'delta' = place.delta.missvals(OH, MH, MF)
  )
  #Technically, this needs to loop by ensemble member - 
  #pooling steps can't be used at this point b/c we haven't implemented
  #those yet.
  npts.MF <- unlist(lapply(MF, count.all.present.xy), use.names = F)
  npts.OH <- unlist(lapply(OH, count.all.present.xy), use.names = T)
  npts.MH <- unlist(lapply(MH, count.all.present.xy), use.names = T)
  ##Actual missing value synchronization steps need to take place in here
  ## and return exactly the same shape of data
  if(missval.thold < 1){
    #Not used often, but should be kept in
    if (tmasks$apply_tmask){
      pct.MF <- npts.MF/sum(!is.na(tmasks$MF$masks[[w]]))
      pct.OH <- npts.OH/sum(!is.na(tmasks$OH$masks[[w]]))
      pct.MH <- npts.MH/sum(!is.na(tmasks$MH$masks[[w]]))
    }else{
      pct.MF <- npts.MF/dim(MF[[1]])[6]
      pct.OH <- npts.OH/dim(OH.slice[[1]])[6]
      pct.MH <- npts.MH/dim(MH.slice[[1]])[6]
    }
    insufficient.data <- any (c(pct.MF, pct.OH, pct.MH) < missval.thold)
  }else{
    #This is the most common case
    insufficient.data <- any(c(npts.MF, npts.OH, npts.MH) < missval.thold)
  }
  if (insufficient.data){
    #Retry with pr missings set to 0
    mv.retry <- T
    if (length(missval.args$pr) > 0){
      #Segment the pr missing value masks
      #(might use rle encoding at some point, but not yet)
      
      pr.OH.vars <- length(missval.args$pr$OH)
      OH <- segment.missval.vars(pr.OH.vars, OH, missval.args$pr$OH, 
                                 oh.coords, ival, jval, 
                                 tmasks$apply_tmask, tmasks$OH, w)
      pr.MH.vars <- length(missval.args$pr$MH)
      MH <- segment.missval.vars(pr.MH.vars, MH, missval.args$pr$MH, 
                                 mh.coords, ival, jval, 
                                 tmasks$apply_tmask, tmasks$MH, w)
      pr.MF.vars <- length(missval.args$pr$MF)
      MF <- segment.missval.vars(pr.MF.vars, MF, missval.args$pr$MF, 
                                 oh.coords, ival, jval, 
                                 tmasks$apply_tmask, tmasks$MF, w)
      npts.MF <- lapply(MF, count.all.present.xy)
      npts.OH <- lapply(OH, count.all.present.xy)
      npts.MH <- lapply(MH, count.all.present.xy)
      if(missval.thold < 1){
        #Not used often, but should be kept in
        if (tmasks$apply_tmask){
          pct.MF <- npts.MF/sum(!is.na(tmasks$MF$masks[[w]]))
          pct.OH <- npts.OH/sum(!is.na(tmasks$OH$masks[[w]]))
          pct.MH <- npts.MH/sum(!is.na(tmasks$MH$masks[[w]]))
        }else{
          pct.MF <- npts.MF/dim(MF[[1]])[6]
          pct.OH <- npts.OH/dim(OH.slice[[1]])[6]
          pct.MH <- npts.MH/dim(MH.slice[[1]])[6]
        }
        sufficient.data <- any (c(pct.MF, pct.OH, pct.MH) < missval.thold)
      }else{
        #This is the most common case
        sufficient.data <- any(c(npts.MF, npts.OH, npts.MH) < missval.thold)
      }
      if (sufficient.data){
        ds.possible <- F
      }
      ds.possible <- T
    }else{
      #If there are no pr missing values, don't downscale the point
      ds.possible <- F
    }
  }else{
    #In the event that everything is above the threshold, all is good.
    mv.retry <- F
    ds.possible <- T
  }
  if (return_qcmask){
    if (!ds.possible || missval.status != 0){
      qc.val <- NA
    }else{
      qc.val <- 1
    }
    qcmask <- array(qc.val, dim=dim(MF[[1]]))
  }else{
    qcmask <- NULL
  }
  #Because this treats a failure in any ensemble as cause to retry absolutely 
  #everything, it is going to be prone to false positives. OTOH, you only need 
  #a single status for the entire thing.
  return(list(OH=OH, MH=MH, MF=MF, 
              qc=qcmask, 
              mv.retry=mv.retry, ds.possible=ds.possible, 
              missval.status=missval.status))
}

segment.missval.vars <- function(varnames, data, masks, data.coords, ival, jval, 
                                 apply_tmask, tmasks, w){
  #Drills down from the full x,y components of a mask to the bits that apply to
  # the region that we are downscaling
  
  #get xy region
  data.masks <- get.xy.slice(masks[varnames], data.coords, ival, jval)
  #Apply tmask
  if (apply_tmask){
    data.masks <- lapply(data.masks, "apply.fudge.tmask", tmasks$masks[[w]])
  }
  #apply mask to data, setting from missing to other as required
  for (var in varnames){
    data[var][data.masks[var]] <- 0 #okay, so this is pr-specific now. Not going to cause issues yet.
  }
  return(data)
}

place.dist.missvals <- function(OH, MH, MF, env=parent.env()){
  #Does absolutely nothing; here to make code easier to read
}

place.synch.missvals <- function(OH, MH, MF, env=parent.env()){
  #synchronizes missing values between OH and MH, placing vals back
  #in the calling environment.
  #Assumes that OH vars and MH vars have the same time dimension
  OH.missing <- rowSums(sapply(OH, missval.xyens)) > 0 #T is NA 
  MH.missing <- rowSums(sapply(MH, missval.xyens)) > 0 #T is NA 
  all.missing <- OH.missing + MH.missing
  all.missing[all.missing  > 0] <- NA
  all.missing[all.missing == 0] <- 1
  OH <- lapply(OH, apply.fudge.tmask, all.missing)
  MH <- lapply(MH, apply.fudge.tmask, all.missing)
  env$OH <- OH
  env$MH <- MH
}

place.delta.missvals <- function(OH, MH, MF, args, threshold, 
                                 att.table,
                                 return.qc.mask=FALSE){
  #'Very similar to distributive, but with a few 
  #'extra checks for missing thresholds
  
  #Do nothing for OH
  #Do nothing for MF
  #For MH, zeroes get treated as missing for some calcs
  MH.tmp <- lapply(MH, function(x){replace(x, x==0, NA)})
  status <- 0
  #If there are no non-missing vals in MF, replace pr missvals
  #in MF AND MF ONLY
  if (any(count.all.present.xy(MF) == 0 )){
    pr.MF.vars <- length(missval.args$pr$MF)
    MF <- segment.missval.vars(pr.MF.vars, MF, missval.args$pr$MF, 
                               oh.coords, ival, jval, 
                               tmasks$apply_tmask, tmasks$MF, w)
    message(paste("Warning in remove.delta.missvals: one or more MF  datasets", 
                  "were all 0 or all missing! Zeroes will be added in place of missing pr."))
    status=130
  }
  #If all vals in MH are 0 or missing, you have two options:
  #(though both involve passing statuses to the Delta code)
  if (any(count.all.preseny.xy(MH.tmp) == 0)){
    if (args$deltaop=='ratio'){
      #If 'ratio', return status=120 (Delta returns OH data)
      message(paste("Warning in remove.delta.missvals: one or more MH datasets", 
                    "were all 0 or all missing when the ratio option was used!", 
                    "OH data will be returned from the delta method instead."))
      status=120 
    }else{
      #otherwise, replace pr missvals and return other status
      message(paste("Warning in remove.delta.missvals: one or more MH datasets", 
                    "were all 0 or all missing! Zeroes will be added in place of missing pr."))
      status=110
    }
  }
  #return MH and MF datasets plus delta status
  env$MH <- MH
  env$MF <- MF
  env$missval_status <- status
}
