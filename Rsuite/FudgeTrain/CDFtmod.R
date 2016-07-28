callCDFt <- function (pred, targ, new, args, ds.method="CDFt", ds.var='null', att_table=NULL){
  #' Calls the GFDL-specific modified CDFt (Vrac et. al 2009) on a single variable
  #' predictor and target, returning the downscaled (DS) output.
  #' Takes as an argument
  #' npas:    The number of quantiles to split the input dataset into. 
  #'  If dev is 0 or 'default', uses the number of observations in either the
  #'  future or historical datasets, depending upon which has fewer observations.
  #' dev :   Parameter controlling the maximum range of the downscaled future data. 
  #'  Maximum range will be  min(OH, MH, MF) - dev * (abs( mean(OH) - mean(MH) ) 
  #'  to max(OH, MH, MF) + dev * (abs( mean(OH) - mean(MH) )
  #' low_tail: the method for handling the low tail in the distribution. 
  #'  Can be one of the following (defaluting to 'original' if not specified): 
  #'  'original':   Use the method always used by the CDft method. The method used
  #'    if the downscaling method is 'CDFt'
  #'  'modified':   Use the modified tail adjustment - the original CDFt high tail
  #'    adjustment reversed and applied to the low tail. 
  #'  'none':   No low tail adjustment will be performed
  #' maxdevretry:    If in a low-precipitation situation with uneven input lengths, 
  #' 
  #' retry:   Behavior upon reaching maxdev. Can be one of the following: 
  #'  'missing':   return all missing values if maxdev is exceeded while retrying
  #'  'predictor': return the future predictor (MF) values if maxdev is exceeded
  #'  'stop':      stop with a large and helpful error message.
  #' @citation P.-A. Michelangeli, M. Vrac, H. Loukos. "Probabilistic downscaling approaches: 
     #' @citation Application to wind cumulative distribution functions", 
     #' @citation Geophys. Res. Lett., doi:10.1029/2009GL038401, 2009)
     
     ###Obtain required arguments (and throw errors if not specified)
     if(!is.null(args$dev)){
       dev <- args$dev
     }else{
       stop(paste("CallCDFt Method Error: parameter dev was missing from the args list"))
     }
     if(is.null(args$retry)){
       args$retry <- "stop"
     }
     if(is.null(args$maxdevretry)){
       maxdev <- 20
     }else{
       maxdev <- args$maxdevretry
     }
     
     out.list <- list()
     pred <- unlist(pred)  #No need to reference the historic predictor names
     rip.list <- lapply(att_table$fut.pred, '[[', 'rip')
     for(rip in rip.list){
       #Select the future predictor vars that you are interested in
       out.var.name <- paste0(ds.var, ".", rip)
       #Unlist the ensemble members
       rip.index <- (rip.list==rip)
       new.data <- new[rip.index]
       new.data <- unlist(new.data)
       
       if(!is.null(args$npas)){
         npas <- args$npas
         if(npas=='default' || npas==0){
           npas=ifelse(length(targ) > length(new.data), length(new.data), length(targ))
         }else if(npas=='training_target'){
           #Note: this option is needed to duplicate 'default'
           #for results prior to 10-20-14
           npas=length(targ)
         }else if(npas=='future_predictor'){
           npas=length(new.data)
         }
         if(npas <= dev){
           stop(paste("Error in callCDFt: npas shouuld be greater than dev, but npas was", 
                      npas, "and dev was", dev))
         }
       }else{
         stop(paste("CDFt Method Error: parameter npas was missing from the args list"))
       }
       ##Note: if any of the input data parameters are named, CDFt will 
       ## fail to run with an 'unused arguments' error, without any decent
       ## explanation as to why. This way works.
       #args.list <- c(list(targ, pred, new.data), list(npas=npas, dev=dev))
       is.retry <- FALSE
       temp <-tryCatch({
         do.call(ds.method, c(list(targ, pred, new.data), 
                              list(npas=npas, dev=dev)
                              ))[c('DS', 'high_cutoff', 'low_cutoff')]}, 
                       error=function(e){
                         is.retry <<- TRUE;
                         loop.try <- FALSE
                         i=1;
                         while(dev <= maxdev && loop.try==FALSE){
                           message(paste0("Error caught from ", ds.method, " with dev=",
                                          dev," and npas=",npas,
                                          "; retrying with dev=dev+",1))
                           loop.try <- TRUE
                           dev=dev+1
                           i=i+1
                           loop.try <- tryCatch(do.call(ds.method, 
                                                        c(list(targ, pred, new.data), 
                                                          list(npas=npas, dev=(dev)
                                                               )))[c('DS', 'high_cutoff', 'low_cutoff')], 
                                                error=function(e){return(FALSE)}
                           )
                         }
                         if(dev > maxdev){
                           #This has not come up yet in testing, but it might.
                           if(args$retry=='stop'){
                             stop(paste("Error in callCDFt: In the process of retrying", 
                                        ds.method, "downscaling at this point",
                                        "dev grew to >", maxdev,
                                        "without the", ds.method, "function",
                                        "being able to complete", 
                                        'while retry was set to "stop".', 
                                        "Please retry with different settings", 
                                        "or contact Carolyn Whitlock."))
                           }else if(args$retry=='missing'){
                             return(list(DS=rep(NA, length(new.data)), 
                                         high_cutoff=NA, low_cutoff=NA))
                           }else if(args$retry=='predictor'){
                             return(list(DS=new.data, 
                                         high_cutoff=NA, low_cutoff=NA))
                           }
                         }else{
                           return(do.call(ds.method, 
                                          c(list(targ, pred, new.data), 
                                            list(npas=npas, dev=(dev))
                                            ))[c('DS', 'high_cutoff', 'low_cutoff')])
                         }
                       }, 
                       silent=FALSE)
     out.list[[out.var.name]] <- as.numeric(temp$DS)
     attributes(out.list[[out.var.name]]) <- c(attributes(out.list[[out.var.name]]), 
                                               list(high_cutoff=temp$high_cutoff, 
                                                    low_cutoff=temp$low_cutoff))
     if(is.retry){
       attr(out.list[[out.var.name]], 'status') <- 1
     }
  }
  return(out.list)
}

CDFtmod <- function (ObsRp, DataGp, DataGf, npas = 100, dev = 2){
  #'GFDL-specific modifications to the CDFt method
  #'mostly related to the low and high tail adjustments. 
  #'For comparison, please consult the original CDFt code located in the 
  #'CRAN repository.
  #'
  #save(file='/home/cew/Code/testing/test_out.R', list=c('ObsRp', 'DataGp', 'DataGf', 'npas', 'dev'))
  mO = mean(ObsRp)
  mGp = mean(DataGp)
  DataGp2 = DataGp + (mO - mGp)
  DataGf2 = DataGf + (mO - mGp)
  FRp = ecdf(ObsRp)
  FGp = ecdf(DataGp2)
  FGf = ecdf(DataGf2)
  a = abs(mean(DataGf) - mean(DataGp))
  m = min(ObsRp, DataGp, DataGf) - dev * a
  M = max(ObsRp, DataGp, DataGf) + dev * a
  x = seq(m, M, , npas)
  FGF = FGf(x)
  FGP = FGp(x)
  FRP = FRp(x)
  FGPm1.FGF = quantile(DataGp2, probs = FGF)
  FRF = FRp(FGPm1.FGF)
  hitail=NA
  lotail=NA
  #Low tail correction
  if (FRF[1] > 0) {
    message("Modified low tail adjust performed")
    i=1
    QQ = quantile(ObsRp, probs=FRF[1])
    while(x[i] <= QQ){
      i=i+1
    }
    i=i-1
    j=2
    while(j <= length(x) && FRF[j]==FRF[1]){
      j=j+1
    }
    j=j-1 #Added JL 3-29
    lotail=x[j]
    if( j==(length(x)+1) ){
      stop("In CDFt, dev must be higher for low tail\n")
    }
    dif_lo = min(j, i)
    FRF[(j-dif_lo+1):j] = FRP[(i-dif_lo+1):i]
    k = j-dif_lo
    if(k > 0){
      FRF[1:k] = 0
    }
  }
  #High tail correction:
  if (FRF[length(x)] < 1) {
    message("Modified High tail adjust performed")
    i = length(x)
    QQ = quantile(ObsRp, probs = FRF[length(x)])
    while (x[i] >= QQ) {
      i = i - 1
    }
    i = i + 1
    j = length(x) - 1
    while (j > 0 && FRF[j] == FRF[length(x)]) {
      j = j - 1
    }
    j=j+1
    hitail=x[j]
    if (j == 0) {
      stop("In CDFt, dev must be higher\n")
    }
    dif = min((length(x) - j), (length(x) - i))
    FRF[j:(j + dif)] = FRP[i:(i + dif)]
    k = j + dif + 1
    if (k < (length(x) + 1) ) {
      FRF[k:(length(x))] = 1
    }
  }
  qntl = FGf(DataGf2)
  xx = approx(FRF, x, qntl, yleft = x[1], yright = x[length(x)], 
              ties = "mean")
  FGp = ecdf(DataGp)
  FGf = ecdf(DataGf)
  FGP = FGp(x)
  FGF = FGf(x)
  #print(attributes(xx$y))
  #attributes(xx$y) <- c(attributes(xx$y), list('high_tail'=hitail, 'low_tail'=lotail))
  return(list(x = x, FRp = FRP, FGp = FGP, FGf = FGF, FRf = FRF, 
              DS = xx$y, high_cutoff=hitail, low_cutoff=lotail))
}