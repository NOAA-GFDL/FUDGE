callCDFt <- function(OH, MH, MF, args, ds.method, train_qcmask=F){
  #At least this one doesn't have to deal with multiple points - 
  # everything is going to be vectors of length 1.
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
     
     ### Remove missing values and be prepared to return a ds QC mask
     nrips <- dim(MF[[1]])[4]
     out.data <- array(NA, dim=dim(MF[[1]]))
     if (train_qcmask){
       qc <- array(1, dim=dim(out.data))
     }else{
       qc <- NULL
     }
     #unlist input data for easier reference
     print(lapply(MF, 'dim'))
     OH <- OH[[1]] #don't unlist; that removes dimension information
     MH <- MH[[1]]
     MF <- MF[[1]]
     notna.idx = apply(MF , c(4), function(x){!is.na(x)}) #one index per rip present
     OH <- OH[!is.na(OH)] #both of these should only
     MH <- MH[!is.na(MH)] #ever have one point in them
 
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
     
     #be ready to append high and low cutoffs for later use
     high_cutoffs = c()
     low_cutoffs = c()
     
     for(n in 1:nrips){
       MF.slice <- MF[,,,n,,]
       MF.slice <- MF.slice[!is.na(MF.slice)]
       #Yes the following bit is long, but all of it's needed for the retry function
       if(!is.null(args$npas)){
         npas <- args$npas
         if(npas=='default' || npas==0){
           npas=ifelse(length(OH) > length(MF.slice), length(MF.slice), length(OH))
         }else if(npas=='training_target'){
           #Note: this option is needed to duplicate 'default'
           #for results prior to 10-20-14
           npas=length(OH)
         }else if(npas=='future_predictor'){
           npas=length(MF.slice)
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
         do.call(ds.method, c(list(OH, MH, MF.slice), 
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
                                          c(list(OH, MH, MF.slice), 
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
               return(list(DS=rep(NA, length(MF.slice)), 
                           high_cutoff=NA, low_cutoff=NA))
             }else if(args$retry=='predictor'){
               return(list(DS=MF.slice, 
                           high_cutoff=NA, low_cutoff=NA))
             }
           }else{
             return(do.call(ds.method, 
                            c(list(OH, MH, MF), 
                              list(npas=npas, dev=(dev))
                            ))[c('DS', 'high_cutoff', 'low_cutoff')])
           }
         }, 
         silent=FALSE)
       out.data[,,,n,,][notna.idx[,n]] <- as.numeric(temp$DS)
       high_cutoffs = c(high_cutoffs, temp$high_cutoff)
       low_cutoffs = c(low_cutoffs, temp$low_cutoffs)
       if(is.retry && train_qcmask){
         #This used to be a status setting; replacing with a qcmask creation
         #attr(out.list[[out.var.name]], 'status') <- 1
         qc[,,,n,,][notna.idx[,n]] <- NA
       }
     }
     attributes(out.data) <- c(attributes(out.data), 
                               list(high_cutoff=high_cutoffs, 
                                    low_cutoff=low_cutoffs))
     return(list(ds=out.data, qc = qc))
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


gfdl_CDFt <- function (ObsRp, DataGp, DataGf, npas = 100, dev = 2) 
{
  #'Local GFDL modifications to the CDFt method to support 
  #'the modified GFDL tail adjustment
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
  if (min(ObsRp) < min(DataGf2)) {
    i = 1
    while (x[i] <= quantile(ObsRp, probs = FRF[1])) {
      i = i + 1
    }
    j = 1
    while (x[j] < min(DataGf2)) {
      j = j + 1
    }
    #The location is trickier for the low tail, but this looks about right
    lotail=x[j]
    k = i
    while (j > 0 && k > 0) {
      FRF[j] = FRP[k]
      j = j - 1
      k = k - 1
    }
    if (j > 0) {
      for (k in j:1) {
        FRF[k] = 0
      }
    }
  }
  if (FRF[length(x)] < 1) {
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
    hitail=x[j]
    if (j == 0) {
      stop("In CDFt, dev must be higher\n")
    }
    dif = min((length(x) - j), (length(x) - i))
    FRF[j:(j + dif)] = FRP[i:(i + dif)]
    k = j + dif
    if (k < length(x)) {
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
  return(list(x = x, FRp = FRP, FGp = FGP, FGf = FGF, FRf = FRF, 
              DS = xx$y, high_cutoff=hitail, low_cutoff=lotail))
}