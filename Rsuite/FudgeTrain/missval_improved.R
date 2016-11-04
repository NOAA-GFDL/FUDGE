 
count.all.present.xyn <- function(dataset){
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
  print(missval.thold)
  print(summary(missval.args))
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
  npts.MF <- unlist(lapply(MF, count.all.present.xyn), use.names = F)
  npts.OH <- unlist(lapply(OH, count.all.present.xyn), use.names = F)
  npts.MH <- unlist(lapply(MH, count.all.present.xyn), use.names = F)
  ##Actual missing value synchronization steps need to take place in here
  ## and return exactly the same shape of data
  print("*npts counts before:")
  print(paste("*OH:", npts.OH))
  print(paste("*MH:", npts.MH))
  print(paste("*MF:", paste(npts.MF)))
  #...you need an indexing step in here - only the predictor vars have a 
  #sample size requirment
  #First, create an index vector of the datasets that each var belongs to
  train.index <- unlist(mapply(rep, c('OH', 'MH', 'MF'), 
                               c(length(npts.OH), 
                                 length(npts.MH), 
                                 length(npts.MF))), 
                        use.names=F)
  #now check to see which datastes get counted in predictor datasets
#  train.index <- rowSums(sapply(ds.ref$predictors, grepl, train.index)) > 0 #Check on vs off
  train.index <- rowSums(sapply(ds.ref$predictors, grepl, train.index))
  train.index[train.index==1] <- 2
  print('train.index:')
  print(train.index)
  #stop('examine output')
  if(missval.thold < 1){
    #The percentage case
    #Not used often, but should be kept in
    if (tmasks$apply_tmask){
      pct.MF <- npts.MF/sum(!is.na(tmasks$MF$masks[[w]]))
      pct.OH <- npts.OH/sum(!is.na(tmasks$OH$masks[[w]]))
      pct.MH <- npts.MH/sum(!is.na(tmasks$MH$masks[[w]]))
    }else{
      pct.MF <- npts.MF/dim(MF[[1]])[6]
      pct.OH <- npts.OH/dim(OH[[1]])[6]
      pct.MH <- npts.MH/dim(MH[[1]])[6]
    }
    insufficient.data <- any (c(pct.OH, pct.MH, pct.MF)[train.index] < missval.thold)
  }else{
    #This is the most common case
    print(c(npts.OH, npts.MH, npts.MF)[train.index])
    print(missval.thold)
    print(c(npts.OH, npts.MH, npts.MF)[train.index] <= missval.thold)
    insufficient.data <- any(c(npts.OH, npts.MH, npts.MF)[train.index] < missval.thold)
  }
  if (insufficient.data){
    #Retry with pr missings set to 0
    mv.retry <- T
    print(summary(missval.args))
    print(length(missval.args$pr))
    if (length(missval.args$pr) > 0){
      #Segment the pr missing value masks
      #(might use rle encoding at some point, but not yet)
      message('retrying sample size wiht precipiation added in')
      print("*npts counts before:")
      print(paste("*OH:", npts.OH))
      print(paste("*MH:", npts.MH))
      print(paste("*MF:", paste(npts.MF)))
      MF <- retry.with.pr.present(MF, missval.args$pr$MF, missval.args$pr$pr_mf, 
                                  tmasks$apply_tmasks, tmasks$MF$masks[[w]])
      OH <- retry.with.pr.present(OH, missval.args$pr$OH, missval.args$pr$pr_oh, 
                          tmasks$apply_tmasks, tmasks$OH$masks[[w]])
      MH <- retry.with.pr.present(MH, missval.args$pr$MH, missval.args$pr$pr_mh, 
                          tmasks$apply_tmasks, tmasks$MH$masks[[w]])
      npts.MF <- lapply(MF, count.all.present.xyn)
      npts.OH <- lapply(OH, count.all.present.xyn)
      npts.MH <- lapply(MH, count.all.present.xyn)
      print("npts counts:")
      print(npts.OH)
      print(npts.MH)
      print(npts.MF)
      print(missval.thold)
      if(missval.thold < 1){
        #Not used often, but should be kept in
        if (tmasks$apply_tmask){
          pct.MF <- npts.MF/sum(!is.na(tmasks$MF$masks[[w]]))
          pct.OH <- npts.OH/sum(!is.na(tmasks$OH$masks[[w]]))
          pct.MH <- npts.MH/sum(!is.na(tmasks$MH$masks[[w]]))
        }else{
          pct.MF <- npts.MF/dim(MF[[1]])[6]
          pct.OH <- npts.OH/dim(OH[[1]])[6]
          pct.MH <- npts.MH/dim(MH[[1]])[6]
        }
        sufficient.data <- all (c(pct.OH, pct.MH, pct.MF)[train.index] >= missval.thold)
      }else{
        #This is the most common case
        print('common case')
        sufficient.data <- all(unlist(c(npts.OH, npts.MH, npts.MF), use.names = F)[train.index] >= missval.thold )
        print('sufficient data')
        print(sufficient.data)
#        stop('examine output')
      }
      #print(sufficient.data)
      #stop('retry happened successfully')
      if (sufficient.data){
        ds.possible <- T
      }
#       ds.possible <- F
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
  print('before return')
print('*#summary of OH:')
print(summary(OH[[1]]))
print('*#summary of MH:')
print(summary(MH[[1]]))
print('*#summary of MF:')
print(summary(MF[[1]]))
  return(list(OH=OH, MH=MH, MF=MF, 
              qc=qcmask, 
              mv.retry=mv.retry, ds.possible=ds.possible, 
              missval.status=missval.status))
}

retry.with.pr.present <- function(data.list, pr.mask, var.list, 
                                  apply_tmask, data.tmask){
  #'For each var in var.list, finds a corresponding el in 
  #'#data.list and makes all places where pr.mask was F
  #'(i.e all places where pr was set to NA, indicating dry day)
  #'to 0 instead of NA. This undoes some of the trace pr 
  #'correction for small sample sizes, resulting in enough data to 
  #'downscale.
  #'If apply_tmasks, a time windowing mask is first applied
  #'to the pr.mask in order to avoid setting values to 0 
  #'that would not have been included in the sample.
  #...this is going to be so nasty when we get vaugely pr-like vars
  #as fixed sptaial grids it is going to be the worst
  if (apply_tmask){
    pr.mask <- apply.fudge.tmask(pr.mask, data.tmask)
    pr.mask[is.na(pr.mask)] <- T
    #...okay, I can see why thishould be T.
    #You don't want to apply this when the 
    #data is not present.
#    stop('examine output')
  }
  for (el in var.list){
    data.list[[el]][!pr.mask] <- 0
    ##Retrying with the old, wrong behavior.
  }
  return(data.list)
}

segment.missval.vars <- function(varnames, data, masks, data.coords, ival, jval, 
                                 apply_tmask, tmasks, w){
  #Drills down from the full x,y components of a mask to the bits that apply to
  # the region that we are downscaling
  
  #get xy region
  #data.masks <- get.xy.slice(masks[varnames], data.coords, ival, jval)
  #No longer needed; looping on i and j before call to pr missvals
  data.masks <- masks
  print(summary(data.masks))
  print(dim(data.masks))
  print(mode(data.masks))
  print(summary(tmasks))
  #Apply tmask
  if (apply_tmask){
    data.masks <- apply.fudge.tmask(data.masks, tmasks$masks[[w]]) #lapply(data=data.masks, "apply.fudge.tmask", tmasks$masks[[w]])
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
  if (any(count.all.present.xyn(MF) == 0 )){
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
  if (any(count.all.preseny.xyn(MH.tmp) == 0)){
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
