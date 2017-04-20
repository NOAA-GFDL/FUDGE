 
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
  missval.status <- 0
  ds.thold <- 1 #At least this many points needed to downscale
  orig <- list()
  orig$OH <- OH
  orig$MH <- MH
  orig$MF <- MF
  #place modified OH, MH and MF (plus status, as appropriate) back into current env
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
  #First, create an index vector of the datasets that each var belongs to
  train.index <- unlist(mapply(rep, c('OH', 'MH', 'MF'), 
                               c(length(npts.OH), 
                                 length(npts.MH), 
                                 length(npts.MF))), 
                        use.names=F)
  #now check to see which datastes get counted in predictor datasets
  #since only teh predictor datasets have a minsample requirment
  #Note: this really needs to be > 0; otherwise, 
  #you index by an array of 1s and 0s rather than by a vector of T/F
  #and that means only one datset gets checked for minsamplesize
  train.index <- rowSums(sapply(ds.ref$training, grepl, train.index)) > 0 #Check on vs off
  ds.index <- rowSums(sapply(ds.ref$downscaling, grepl, train.index)) > 0
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
    insufficient.data <- any(c(npts.OH, npts.MH, npts.MF)[train.index] < missval.thold)
  }
  if (insufficient.data){
    #Retry with pr missings set to 0
    mv.retry <- T
    ds.possible <- F
#     print("missval.args$pr")
#     print(str(missval.args$pr))
    if (length(missval.args$pr) > 0){
      #Segment the pr missing value masks
      #(might use rle encoding at some point, but not yet)
      message('retrying sample size with precipiation added in')
      pr_vars <- list(OH='pr_oh', MH='pr_mh', MF='pr_mf')
      #env=parent.frame(n=0)
#       print('is there something wrong with teh indexing?')
#       print(train.index)
#       print(c('OH', 'MH', "MF")[train.index])
      #Datset.index only has 3 elements - important for multivar pr
      dataset.index <- rowSums(sapply(ds.ref$training, 
                                      grepl, c("OH", "MH", "MF"))) > 0
      for(el in c('OH', 'MH', 'MF')[dataset.index]){
        #Note: this needs to take place with elements of orig; 
        #if you do not use it, the synchronous missing values step 
        #removes samples in OH and MH that never get added back in
        var.idx <- pr_vars[[el]]
        #print(summary(orig[[el]]))
        assign(el, retry.with.pr.present(orig[[el]], 
                                     missval.args$pr[[el]],  
                                     missval.args$pr[[var.idx]],
                                     tmasks$apply_tmasks, 
                                     tmasks[[el]]$masks[[w]]))
      }
      npts.MF <- lapply(MF, count.all.present.xyn)
      npts.OH <- lapply(OH, count.all.present.xyn)
      npts.MH <- lapply(MH, count.all.present.xyn)
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
        sufficient.data <- all(unlist(c(npts.OH, npts.MH, npts.MF), use.names = F)[train.index] >= missval.thold )
      }
      if (sufficient.data){
        ds.possible <- T
      }
    }else{
      #If there are no pr missing values, don't downscale the point
      ds.possible <- F
    }
#stop('examine the output from the retry; there are too many missing values being written here')
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
  complete.hist <- complete.cases(c(OH,MH))
  complete.fut <- complete.cases(MF)
  return(list(OH=OH, MH=MH, MF=MF, 
              qc=qcmask, 
              mv.retry=mv.retry, ds.possible=ds.possible, 
              missval.status=missval.status, 
              complete.hist=complete.hist, complete.fut=complete.fut))
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
  #switching from appy_fudge_tmask to apply.any.mask; maybe fixes problem?
    pr.mask <- apply.any.mask(data=pr.mask, mask=data.tmask, dim='temporal')
    #pr.mask[is.na(pr.mask)] <- T
  }
  #Thanks to var collapsing, extra step needed to check for matches
  match.vars <- c()
  data.names <- names(data.list)
  for (v in var.list){
    match.index <- c(grep(paste0(v, "$"), data.names), 
                     grep(paste0(v, "\\."), data.names))
    match.vars <- c(match.vars, data.names[match.index])
  }
  for (el in match.vars){
    print(el)
    #print(summary(pr.mask==0))
    tmp.pr <- data.list[[el]]
    #dim(pr.mask) <- dim(tmp.pr)
    data.list[[el]] <- as.numeric(pr.mask)
    print(length(data.list[[el]]))
    print(dim(tmp.pr))
    data.list[[el]][!is.na(tmp.pr)] <- tmp.pr[!is.na(tmp.pr)]
#     print(dim(pr.mask))
#     print(dim(tmp.pr))
#     print(length(data.list[[el]]))
    #...ooookay, as of 03-07-2017, this is causing issues. Is there a way to fix it?
#dim issues seem to have been caused by unexamined assumption that target could be 3x3.
    dim(data.list[[el]]) <- dim(tmp.pr)
#    print(length(data.tmask))
    data.list[[el]] <- apply.any.mask(data=data.list[[el]], mask=data.tmask, dim='temporal')    
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

place.dist.missvals <- function(OH, MH, MF, env=parent.frame()){
  #Does absolutely nothing; here to make code easier to read
}

place.synch.missvals <- function(OH, MH, MF, env=parent.frame()){
  #synchronizes missing values between OH and MH, placing vals back
  #in the calling environment.
  #Assumes that OH vars and MH vars have the same time dimension
  OH.missing <- rowSums(sapply(OH, missval.xyens)) > 0 #T is NA 
  MH.missing <- rowSums(sapply(MH, missval.xyens)) > 0 #T is NA 
  all.missing <- OH.missing + MH.missing
  all.missing[all.missing  > 0] <- NA
  all.missing[all.missing == 0] <- 1
  print("working on the hist vals")
  OH <- lapply(OH, apply.fudge.tmask, all.missing)
  MH <- lapply(MH, apply.fudge.tmask, all.missing)
  #Added 11-10: Apparently, I was doing this for the synch lm masks, WTH.
#  MF.missvals <- lapply(MF, function(x){apply(x, c(1,2,3,5,6), 'sum')})
#   for (el in names(MF)){
#     missval.mask <- as.numeric(!is.na(MF.missvals[[el]]))
#     missval.mask[missval.mask==0] <- NA
#     print(summary(missval.mask))
#     MF[[el]] <- MF[[el]] *  missval.mask 
#   }
##Trying to fix this as of 4-19-2017
for (n in 1:dim(MF[[1]])[4]){
  #print(paste("starting ens", n))
  #print(dim(MF[[1]]))
  #print(summary(sapply(lapply(MF, '[', ,,,n,,,drop=F), missval.xyens)))
  MF.ens.missvals <- rowSums(sapply(lapply(MF, '[', ,,,n,,,drop=F), missval.xyens)) > 0 #T is NA
  MF.ens.missvals[MF.ens.missvals  > 0] <- NA
  MF.ens.missvals[MF.ens.missvals == 0] <- 1
  #print("MF.ens.missvals:")
  #print(summary(MF.ens.missvals))
  for (el in names(MF)){
    MF[[el]][,,,n,,] <- apply.fudge.tmask(MF[[el]][,,,n,,,drop=F], MF.ens.missvals)
  }
}
# MF.missing <- rowSums(sapply(MF, missval.xyens)) > 0 #T is NA
# MF.missing[MF.missing  > 0] <- NA
# MF.missing[MF.missing == 0] <- 1
# MF <- lapply(MF, apply.fudge.tmask, MF.missing)

#print(summary(MF.missing))
#print(summary(MF))
#print(lapply(MF, 'summary'))

#print("Summary of rags returend from missval functions:")
#print(lapply(MF, 'summary'))
#print(lapply(MF, 'length'))
  env$MF <- MF
  #Definetly keep these
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
