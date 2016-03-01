callGFDL_CDFt <- function (pred, targ, new, args, ds.var='null', att_table=NULL){
  #' Calls the GFDL-specific modified CDFt (Vrac et. al 2009) on a single variable
  #' predictor and target, returning the downscaled (DS) output.
  #' Takes as an argument
  #' npas: The number of quantiles to split the input dataset into. 
  #' If dev is 0 or 'default', uses the number of observations in either the
  #' future or historical datasets, depending upon which has fewer observations.
  #' dev : The number of deviations used when calculating. 
  #' low_tail: the method for handling the low tail in the distribution. 
  #' Can be one of the following: 
  #'  '':
  #'  '':
  #' @citation P.-A. Michelangeli, M. Vrac, H. Loukos. "Probabilistic downscaling approaches: 
     #' @citation Application to wind cumulative distribution functions", 
     #' @citation Geophys. Res. Lett., doi:10.1029/2009GL038401, 2009)
     
     ###Obtain required arguments (and throw errors if not specified)
     if(!is.null(args$dev)){
       dev <- args$dev
     }else{
       stop(paste("GFDL_CDFt Method Error: parameter dev was missing from the args list"))
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
           stop(paste("Error in callGFDL_CDFt: npas shouuld be greater than dev, but npas was", 
                      npas, "and dev was", dev))
         }
       }else{
         stop(paste("GFDL_CDFt Method Error: parameter npas was missing from the args list"))
       }
       low_tail='none'
       if('low_tail'%in%names(args)){
         if(args$low_tail=="original" | args$low_tail=="modified"){
           low_tail = args$low_tail
         }else if(args$low_tail!='none'){
           stop(paste("Error in GFDL_CDFt: low_tail should be one of original, modified or none, but was", args$low_tail, "!"))
         }
       }
          if(is.null(args)){
            temp <- GFDL_CDFt(targ, pred, new.data, npas=length(targ), low_tai=low_tail)$DS
          }else{
            ##Note: if any of the input data parameters are named, CDFt will 
            ## fail to run with an 'unused arguments' error, without any decent
            ## explanation as to why. This way works.)
            retry <- FALSE
            temp <-tryCatch({#dev.1 <- dev 
              do.call("GFDL_CDFt", c(list(targ, pred, new.data), 
                                     list(npas=npas, dev=dev, low_tail=low_tail)))$DS}, 
                            #error=function(e){return(rep(NA, length(new.data)))},
                            error=function(e){
                              retry <<- TRUE;
                              loop.try <- FALSE
                              i=1;
                              while(dev <= 20 && loop.try==FALSE){
                                message(paste0("Error caught from GFDL_CDFt with dev=",
                                               dev," and npas=",npas,
                                               "; retrying with dev=dev+",1))
                                loop.try <- TRUE
                                dev=dev+1
                                i=i+1
                                loop.try <- tryCatch(do.call("GFDL_CDFt", 
                                                             c(list(targ, pred, new.data), 
                                                               list(npas=npas, dev=dev, 
                                                                    low_tail=low_tail)))$DS, 
                                                     error=function(e){return(FALSE)}
                                )
                              }
                              return(do.call("GFDL_CDFt", 
                                             c(list(targ, pred, new.data), 
                                               list(npas=npas, dev=dev, low_tail=low_tail)))$DS)
                            }, 
                            silent=FALSE)
          }
          out.list[[out.var.name]] <- as.numeric(temp)
          if(retry){
            #save(file='/home/cew/Code/testing/test_out.R', list=c('targ', 'pred', 'new.data'))
            attr(out.list[[out.var.name]], 'status') <- 1
          }
     }
     return(out.list)
}


GFDL_CDFt <- function (ObsRp, DataGp, DataGf, npas = 100, dev = 2, low_tail="original"){
  #'GFDL-specific modifications to the CDFt method
  #'mostly related to the low tail of the distribution
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