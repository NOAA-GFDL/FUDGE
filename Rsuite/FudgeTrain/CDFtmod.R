callCDFtmod <- function (pred, targ, new, args, ds.var='null', att_table=NULL){
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
       stop(paste("CDFtmod Method Error: parameter dev was missing from the args list"))
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
           stop(paste("Error in callCDFtmod: npas shouuld be greater than dev, but npas was", 
                      npas, "and dev was", dev))
         }
       }else{
         stop(paste("CDFtmod Method Error: parameter npas was missing from the args list"))
       }
       low_tail='original'
       if('low_tail'%in%names(args)){
         if(args$low_tail=="modified" | args$low_tail=="none" 
            #| args$low_tail == "newcheck" | args$low_tail == "newcheck2"
            ){
           low_tail = args$low_tail
         }else if(args$low_tail!='original'){
           stop(paste("Error in CDFtmod: low_tail should be one of original, modified or none, but was", args$low_tail, "!"))
         }
       }
       if(is.null(args)){
         temp <- CDFt(targ, pred, new.data, npas=length(targ))$DS
       }else{
         ##Note: if any of the input data parameters are named, CDFt will 
         ## fail to run with an 'unused arguments' error, without any decent
         ## explanation as to why. This way works.
         #args.list <- c(list(targ, pred, new.data), list(npas=npas, dev=dev))
         is.retry <- FALSE
         temp <-tryCatch({#dev.1 <- dev 
           do.call("CDFtmod", c(list(targ, pred, new.data), 
                             list(npas=npas, dev=dev,
                                  low_tail=low_tail)))$DS}, 
                         error=function(e){
                           is.retry <<- TRUE;
                           loop.try <- FALSE
                           i=1;
                           while(dev <= maxdev && loop.try==FALSE){
                             message(paste0("Error caught from CDFtmod with dev=",
                                            dev," and npas=",npas,
                                            "; retrying with dev=dev+",1))
                             loop.try <- TRUE
                             dev=dev+1
                             i=i+1
                             loop.try <- tryCatch(do.call("CDFtmod", 
                                                          c(list(targ, pred, new.data), 
                                                            list(npas=npas, dev=(dev), 
                                                                 low_tail=low_tail)))$DS, 
                                                  error=function(e){return(FALSE)}
                             )
                           }
                           if(dev > maxdev){
                             #This has not come up yet in testing, but it might.
                             if(args$retry=='stop'){
                               stop(paste("Error in callCDFt: In the process of retrying", 
                                          "CDFt downscaling at this point",
                                          "dev grew to >", maxdev,
                                          "without the CDFt function",
                                          "being able to complete", 
                                          'while retry was set to "stop".', 
                                          "Please retry with different settings", 
                                          "or contact Carolyn Whitlock."))
                             }else if(args$retry=='missing'){
                               return(rep(NA, length(new.data)))
                             }else if(args$retry=='predictor'){
                               return(new.data)
                             }
                           }else{
                             return(do.call("CDFtmod", 
                                            c(list(targ, pred, new.data), 
                                              list(npas=npas, dev=(dev), 
                                                   low_tail=low_tail)))$DS)
                           }
                         }, 
                         silent=FALSE)
       }
       out.list[[out.var.name]] <- as.numeric(temp)
       if(is.retry){
         attr(out.list[[out.var.name]], 'status') <- 1
       }
     }
     return(out.list)
}

CDFtmod <- function (ObsRp, DataGp, DataGf, npas = 100, dev = 2, low_tail="original"){
  #'GFDL-specific modifications to the CDFt method
  #'mostly related to the low tail of the distribution. 
  #'
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
  #Low tail correction
  if(low_tail=='original'){
    if (min(ObsRp) < min(DataGf2)) {
      message("Original low tail adjust peformed")
      i = 1
      while (x[i] <= quantile(ObsRp, probs = FRF[1])) {
        i = i + 1
      }
      j = 1
      while (x[j] < min(DataGf2)) {
        j = j + 1
      }
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
#   }else if(low_tail=='newcheck'){
#     if (FRF[1] > 0) {
#       message("Modified if-check low tail adjust peformed")
#       i = 1
#       while (x[i] <= quantile(ObsRp, probs = FRF[1])) {
#         i = i + 1
#       }
#       j = 1
#       while (x[j] < min(DataGf2)) {
#         j = j + 1
#       }
#       k = i
#       while (j > 0 && k > 0) {
#         FRF[j] = FRP[k]
#         j = j - 1
#         k = k - 1
#       }
#       if (j > 0) {
#         for (k in j:1) {
#           FRF[k] = 0
#         }
#       }
#     }
#   }else if(low_tail=='newcheck2'){
#     if (FRF[1] > 0) {
#       message("Other modified if-check low tail adjust peformed")
#       i = 1
#       while (x[i] <= quantile(ObsRp, probs = FRF[1])) {
#         i = i + 1
#       }
#       #Modifed from original to match the low tail adjustment (fencepost error?)
#       i = i - 1
#       j = 2
#       while (x[j] < min(DataGf2)) {
#         j = j + 1
#       }
#       k = i
#       while (j > 0 && k > 0) {
#         FRF[j] = FRP[k]
#         j = j - 1
#         k = k - 1
#       }
#       if (j > 0) {
#         for (k in j:1) {
#           FRF[k] = 0
#         }
#       }
#     }
  }else if(low_tail=='modified'){
    if (FRF[1] > 0) {
      message("Modified low tail adjust performed")
      i=1
      QQ = quantile(ObsRp, probs=FRF[1])
      while(x[i] <= QQ){
        i=i+1
      }
      #added 2-29 to better match lower adjust
      i=i-1
      j=2 #2?
      while(j <= length(x) && FRF[j]==FRF[1]){
        j=j+1
      }
      if( j==(length(x)+1) ){
        stop("In CDFt, dev must be higher for low tail\n")
      }
      dif_lo = min(j, i)
      FRF[(j-dif_lo+1):j] = FRP[(i-dif_lo+1):i]
      k = j-dif_lo+1
      if(k > 1){
        FRF[1:k] = 0
      }
    }
  }
  #otherwise, include no tail correction.
  #High tail correction:
  if (FRF[length(x)] < 1) {
    message("High tail adjust performed")
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
              DS = xx$y))
}