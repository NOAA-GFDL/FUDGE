#CallDSMethod.R
#' CallDSMethod
#' Calls a downscaling method specified as ds.method and returns the result of training it 
#' upon train.predict and train.target, and then running the relvant equation upon
#' esd.gen. No cross-validation is used for these methods. 
#' 
#' @param ds.method: a string representation of the downscaling data to be used. 
#' @param train.predict: A list of vectors of train.predictor data. For any downscaling run, 
#' there can be multiple predictor datasets - both in a multivariate sense and a multiple point
#' sense.
#' @param train.target: a vector of train.target data. For any downscaling run, there should 
#' only ever be one target dataset of one variable length.
#' @param esd.gen: A list of lists of vectors of esd generation data. For any downscaling run, 
#' if we attempt to train the data and apply it as separate steps, it should be possible to 
#' apply that training to several possible realizations - each of which is effectively a 
#' train.predict data structure. 
#' @param args = NULL: a list of arguments to be passed to the downscaling function.
#' Defaults to NULL (no arguments)
#' @param ds.var: The target variable. Can be used to key off of some methods. 
#' @examples 
#' @references \url{link to the FUDGE API documentation} 

CallDSMethod <- function(ds.method, train.predict, train.target, esd.gen, args=NULL, ds.var='irrelevant'){
  out <- switch(ds.method, 
                "simple.lm" = callSimple.lm(train.predict, train.target, esd.gen),
                'CDFt' = callCDFt(train.predict, train.target, esd.gen, args),
                'simple.bias.correct' = callSimple.bias.correct(train.predict, train.target, esd.gen, args),
                'general.bias.correct' = callGeneral.Bias.Corrector(train.predict, train.target, esd.gen, args),
                "BCQM" = callBCQMv2(train.target, train.predict, esd.gen, args), 
                "EDQM" = callEDQMv2(train.target, train.predict, esd.gen, args), 
                "CFQM" = callCFQMv2(train.target, train.predict, esd.gen, args), 
                "DeltaSD" = callDeltaSD(train.target, train.predict, esd.gen, args), #, ds.var)
                "QMAP" = QMAP(train.target, train.predict, esd.gen, args),
                ReturnDownscaleError(ds.method))
  return(out)  
}

ReturnDownscaleError <- function(ds.method){
  #Returns an error and stops the entire function if the DS method used is not supported.
  stop(paste("Downscale Method Error: the method", ds.method, "is not supported for FUDGE at this time."))
}

callSimple.lm <- function(pred, targ, new, args){
  #Creates a simple linear model without a cross-validation step. 
  #Mostly used to check that the procedure is working
  lm.results <- lm(targ ~ pred)
  lm.intercept <- lm.results$coefficients[1]
  lm.slope <- lm.results$coefficients[2]
  if(is.na(lm.intercept) || is.na(lm.slope) ){
    warning(paste("simple.lm warning: intercept was", lm.intercept, 
                  "and intercept was", lm.slope, ": therefore no ESD values will be generated."))
  }
  trained.function<-function(x){
    return( lm.intercept + unlist(x)*lm.slope)
  }
  #insert save command for saving 
  #May be rendered obsolete if going to a model that uekeeps everything in memory and trains each time.
  return(trained.function(new))
}

callCDFt <- function (pred, targ, new, args){
  #Calls the CDFt function
  #If no argument is provided for npas, defaults to 
  #npas=length(targ)
  ###Obtain required arguments (and throw errors if not specified)
  if(!is.null(args$dev)){
    dev <- args$dev
  }else{
    stop(paste("CDFt Method Error: parameter dev was missing from the args list"))
  }
  if(!is.null(args$npas)){
    npas <- args$npas
    if(npas=='default' || npas==0){
      npas=ifelse(length(targ) > length(new), length(new), length(targ))
    }else if(npas=='training_target'){
      #Note: this option is needed to duplicate 'default'
      #for results prior to 10-20-14
      npas=length(targ)
    }else if(npas=='future_predictor'){
      npas=length(new)
    }
    if(npas <= dev){
      stop(paste("Error in callCDFt: npas shouuld be greater than dev, but npas was", 
                 npas, "and dev was", dev))
    }
  }else{
    stop(paste("CDFt Method Error: parameter npas was missing from the args list"))
  }
  if(is.null(args)){
    temp <- CDFt(targ, pred, new, npas=length(targ))$DS
  }else{
    ##Note: if any of the input data parameters are named, CDFt will 
    ## fail to run with an 'unused arguments' error, without any decent
    ## explanation as to why. This way works.
    args.list <- c(list(targ, pred, new), list(npas=npas, dev=dev))
    temp <- do.call("CDFt", args.list)$DS
  }
  return(as.numeric(temp))
}

callSimple.bias.correct <- function(pred, targ, new, args){
  #Performs a simple bias correction adjustment,
  #applying the mean difference between the
  #predictor and target over the time series
  #to the esd.gen dataset to give downscaled data.
  bias <- mean(pred-targ)
  new.targ <- new-bias
  return(new.targ)
}

callGeneral.Bias.Corrector <- function(pred, targ, new, args){
  #Calls two downscaling methods: one used as a source of downscaling
  #values, the other used as a check against those values. Those values
  #are then compared; if the values are sufficiently similar to each other, 
  #the downscaled values are used; otherwise, the qc values are used. Ideally, 
  #the method used for QC should be less computationally-expensive than the 
  #method used for downscaling. 
  qc.method <- args$qc.method
  args$qc.method <- NULL
  ds.method <- args$ds.method
  args$ds.method <- NULL
  if(!is.null(args$compare.factor)){
    correct.factor <- args$compare.factor
    args$compare.factor <- NULL
  }else{
    correct.factor = 0.5
  }
  if(length(args)!=0) sample.args=args else sample.args=NULL
  ds.vals <- CallDSMethod(ds.method=ds.method, pred, targ, new, sample.args)
  qc.vals <- CallDSMethod(ds.method=qc.method, pred, targ, new, sample.args)
  out.vals <- ifelse( (abs(ds.vals-qc.vals) < correct.factor), yes=ds.vals, no=qc.vals )
  return(out.vals)
}

callDeltaSD <- function(LH,CH,CF,args){
  #'@author carlos.gaitan@noaa.gov
    #'@description The script uses the Delta Method to downscale coarse res. climate variables  
    #'@param LH: Local Historical (a.k.a. observations)
    #'@param CH: Coarse Historical (a.k.a. GCM historical)
    #'@param CF: Coarse Future (a.k.a GCM future)
    #'@param args: A list containing two arguments: deltatype, one of 'mean' or 'median', 
    #' which will be used to determine what single value will be used for the difference
    #' between the CH and CF, and deltaop, one of 'ratio' or 'add', which will be used 
    #' to determine the mthod for calculating the delta
    #'Uses the difference (ratio or subtraction) between CF and CH means or medians to calculate
    #' a delta that is applied to the LH (observational) data
    #'@return SDF: Downscaled Future (Local)
    ########################################
    #Note: preferred behavior is to truncate vectors
    #rather than randomly sampling iff too short.
    
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
    if(!is.null(args$keep.zeroes)){
      keep.zeroes <- ifelse(args$keep.zeroes=='on', TRUE, FALSE)
    }else{
      stop(paste("DeltaSD Downscaling Error: keep.zeroes not found in args"))
    }
    #Decide how many iterations of the delta method to perform
    #based on the relative lengths of the historical
    #and future data (future should be >= historical) #Switch 1-28 from <  
    if(length(LH) >= length(CF)){
      #If the future and historical periods are unequal, truncate the vectors
      #That...raises an interesting question: should the CH vector be truncated as well?
      #Technically, it doesn't need to be for the code to work...
      if(keep.zeroes){
        SDF <- LH[1:length(CF)]
          out.temp <- delta.downscale(LH[1:length(CF)], CH, CF, deltatype, deltaop, keep.zeroes)
          SDF[SDF!=0] <- out.temp
      }else{
        SDF <- delta.downscale(LH[1:length(CF)], CH, CF, deltatype, deltaop)
      }
    }else{
      #Otherwise, if the vectors are uneven then calculate n+1 deltas, 
      #where n=length(CF)/length(LH)
      write.len <- 1
      out.len <- length(CF)
      in.len <- length(LH)
      SDF <- rep(NA, out.len)
      #vector comparisons take a long time relative to other things
      if(keep.zeroes){
        comp.indices <- which(LH!=0)
      }
      while(write.len < out.len){
        #delta.downscale removes NAs in the output vector
        tempvec <- delta.downscale(LH, CH, CF[write.len:(write.len + in.len)], 
                                                             deltatype, deltaop, keep.zeroes)
        if(keep.zeroes){
          sd.write.indices <- (write.len-1) + comp.indices
          sd.write.indices <- sd.write.indices[sd.write.indices <= out.len] #Remove any that might lead to a longer write index
          SDF[sd.write.indices] <- tempvec
        }else{
           SDF[write.len:length(tempvec)] <- tempvec
        }
        write.len <- write.len + in.len
      }
    }
#     print("Number of NA values in DeltaSD")
    num.na <- (sum(is.na(SDF)))
    if(num.na > 0){
      print(num.na)
      stop("Error in DeltaSD: NAs not in out being introduced from somewhere")
    }
    return(SDF)
}

delta.downscale <- function(delta.targ, delta.hist, delta.fut, deltatype, deltaop, keep.zeroes=FALSE){
  #Calculates a delta after removing NAs and applies it to a target vector.
  #Helper method for callDeltaSD, but might be used elsewhere.
  
  #Make sure that there are no NA values in the current vector
  if(keep.zeroes){
    delta.fut <- delta.fut[!is.na(delta.fut) & delta.fut!=0]
    delta.targ <- delta.targ[!is.na(delta.targ) & delta.targ!=0]
    delta.hist <- delta.hist[!is.na(delta.hist) & delta.hist!=0]
  }
  if(deltaop=='add'){
    #Downscale by difference delta
    delta<-do.call(deltatype, list(delta.fut))-do.call(deltatype, list(delta.hist))
    out <- delta.targ + delta #CHANGED FROM delta.fut
  }else if(deltaop=='ratio'){
    #Downscale by percentage delta (never negative, but occasionally NaN)
    delta<-do.call(deltatype, list(delta.fut))/do.call(deltatype, list(delta.hist))
    #Loud warning message for divide-by-0 case
    if(is.nan(delta)||is.infinite(delta)|| is.na(delta)){
      message(paste("Warning in delta.downscale: Calculated delta is either NaN or Inf and will produce",
                    "non-numeric results. Returning values without delta."))
      return(delta.targ)
    }
    out <- delta.targ*delta #CHANGED FROM delta.fut
  }else{
    stop(paste("delta.downscale Downscaling Error: deltaop", deltaop, "is not one of 'ratio' or 'add'"))
  }
  return(out)
}


callEDQMv2<-function(LH,CH,CF,args){ 
  #'Performs an equidistant correction adjustment
  # LH: Local Historical (a.k.a. observations)
  # CH: Coarse Historical (a.k.a. GCM historical)
  # CF: Coarse Future (a.k.a GCM future)
  #'Cites Li et. al. 2010
  #' Calls latest version of the EDQM function
  #' as of 12-29
  lengthCF<-length(CF)
  lengthCH<-length(CH)
  lengthLH<-length(LH)
  
  if (lengthCF>lengthCH) maxdim=lengthCF else maxdim=lengthCH
  
  # first define vector with probabilities [0,1]
  prob<-seq(0.001,0.999,length.out=lengthCF)
  
  # initialize data.frame
  temp<-data.frame(index=seq(1,maxdim),CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),
                   qLHecdfCFqCF=rep(NA,maxdim),qCHecdfCFqCF=rep(NA,maxdim),
                   EquiDistant=rep(NA,maxdim))
  
  SDF<-data.frame(index=seq(1,maxdim),CF=rep(NA,maxdim),CH=rep(NA,maxdim),
                  LH=rep(NA,maxdim),CFQM=rep(NA,maxdim),BCQM=rep(NA,maxdim),
                  EDQM=rep(NA,maxdim),ERQM=rep(NA,maxdim))
  temp$CF[1:lengthCF]<-CF
  temp$CH[1:lengthCH]<-CH
  temp$LH[1:lengthLH]<-LH
  
  temp.CFsorted<-temp[order(temp$CF),]
  #Combine needed to deal with cases where CH longer than CF
  if (lengthCH-lengthCF > 0){
    temp.CFsorted$ecdfCFqCF<- c(ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE)), rep(NA, (lengthCH-lengthCF)))
  }else{
    temp.CFsorted$ecdfCFqCF<-ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE))
  }
  
  temp.CFsorted$qLHecdfCFqCF[1:lengthCF]<-quantile(temp$LH,ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE)),na.rm =TRUE)
  temp.CFsorted$qCHecdfCFqCF[1:lengthCF]<-quantile(temp$CH,ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE)),na.rm =TRUE)
  # EQUIDISTANT CDF (Li et al. 2010)
  temp.CFsorted$EquiDistant<-temp.CFsorted$CF+ temp.CFsorted$qLHecdfCFqCF-temp.CFsorted$qCHecdfCFqCF
  temp<-temp.CFsorted[order(temp.CFsorted$index),]
  
  return(temp$EquiDistant[!is.na(temp$EquiDistant)])
}

###CG DS method
callCFQMv2<-function(LH,CH,CF,args){
  #'Calls the latest version of the CFQM function
  #'as of 12-29
  #'2-27-2015 edit: added an argument for determining
  #'whether data is sorted by the CH or CF vectors
  # Obtain options
  if(!is.null(args$sort)){
    #can be one of 'future' or 'historical'
    sort.opt <- args$sort
    if(sort.opt=='future'){
      sort.opt <- 'CF'
    }else if(sort.opt=='historical'){
      sort.opt <- 'CH'
    }else if(sort.opt=='target'){
      sort.opt <- 'LH'
    }else{
      stop(paste("CFQM_DF Downscaling Error: arg sort was", sort.opt, "not 'future', 'historical', or 'target'"))
    }
  }else{
    stop(paste("CFQM_DF Downscaling Error: sort not found in args"))
  }
  
  lengthCF<-length(CF)
  lengthCH<-length(CH)
  lengthLH<-length(LH) 
  
  if (lengthCF>lengthCH){
    maxdim=lengthCF
    longest.dim <- 'F'
    }else{
     maxdim=lengthCH
     longest.dim <- 'H'
    }
  
  # first define vector with probabilities [0,1]
  prob<-seq(0.001,0.999,length.out=lengthCF)
    
  # initialize data.frame
  temp<-data.frame(index=seq(1:maxdim),
    #index=c(seq(1,lengthCF), rep(NA, maxdim-lengthCF)), #making changes to index to make sure that they make sense
                   CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),
                   qLH=rep(NA,maxdim),ecdfCHqLH=rep(NA,maxdim),qCFecdfCHqLH=rep(NA,maxdim))
  temp$CF[1:lengthCF]<-CF
  temp$CH[1:lengthCH]<-CH
  temp$LH[1:lengthLH]<-LH
  
  if (regexpr(longest.dim, sort.opt) < 0){ #If maxdim is not of the same time period as the sort vector
    print(paste(longest.dim, sort.opt, sep=" : "))
    #sort.vec <- rep(as.vector(temp[[sort.opt]]), length.out=maxdim)
    temp$sortVec <- rep(as.vector(temp[[sort.opt]]), length.out=maxdim)
    sort.opt <- 'sortVec'
  }
  #If the longest dim is used, sorting is straightforward
  temp.opt.sorted<-temp[order(temp[[sort.opt]]),] #sorts by sort.opt
  
  temp.opt.sorted$qLH<-quantile(temp.opt.sorted$LH,prob,na.rm =TRUE) #removed all 1:lengthCF
  temp.opt.sorted$ecdfCHqLH<-ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE))
  temp.opt.sorted$qCFecdfCHqLH<-quantile(temp$CF,ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)),na.rm =TRUE) #Added parenthesis befpre ecdf
  temp<-temp.opt.sorted[order(temp.opt.sorted$index, na.last=TRUE),] #, na.last=FALSE #removed order #temp.opt.sorted$index
  
  SDF<-temp$qCFecdfCHqLH
  print(summary(SDF))
  print(length(SDF))
  print(length(SDF[!is.na(SDF)]))
  cor.vector <- c("temp$CH", "temp$LH", "temp$CF")
  for (j in 1:length(cor.vector)){
    cor.var <- cor.vector[j]
    cor.out <- eval(parse(text=cor.var))
    if(length(cor.out) > length(SDF)){
      out.cor <- cor(as.vector(SDF), as.vector(cor.out)[1:length(SDF)], use='pairwise.complete.obs')
    }else{
      out.cor <- cor(as.vector(SDF)[1:length(cor.out)], as.vector(cor.out), use='pairwise.complete.obs')
    }
    print(paste("temp.out", ",", cor.var, "):", out.cor, sep=""))
  }
  return(SDF[!is.na(SDF)])
}

callBCQMv2<-function(LH,CH,CF,args){
  #' Calls latest version of BCQM function
  #' as of 12-29
  lengthCF<-length(CF)
  lengthCH<-length(CH)
  lengthLH<-length(LH)
    
  if (lengthCF>lengthCH) maxdim=lengthCF else maxdim=lengthCH
  
  # first define vector with probabilities [0,1]
  prob<-seq(0.001,0.999,length.out=lengthCF)
  
  # initialize data.frame
  temp<-data.frame(index=c(seq(1,lengthCF), rep(NA,maxdim-lengthCF)),CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),
                   qCF=rep(NA,maxdim),ecdfCHqCF=rep(NA,maxdim),qLHecdfCHqCF=rep(NA,maxdim))
  temp$CF[1:lengthCF]<-CF
  temp$CH[1:lengthCH]<-CH
  temp$LH[1:lengthLH]<-LH
  
  temp.CFsorted<-temp[order(temp$CF),]
  temp.CFsorted$qCF[1:lengthCF]<-quantile(temp.CFsorted$CF,prob,na.rm =TRUE)
  temp.CFsorted$ecdfCHqCF[1:lengthCF]<-ecdf(temp$CH)(quantile(temp$CF,prob,na.rm =TRUE))
  temp.CFsorted$qLHecdfCHqCF[1:lengthCF]<-quantile(temp$LH,ecdf(temp$CH)(quantile(temp$CF,prob,na.rm =TRUE)),na.rm =TRUE)
  temp<-temp.CFsorted[order(temp.CFsorted$index),]
  
  #SDF<-temp.final$qCFecdfCHqLH
  return(temp$qLHecdfCHqCF[!is.na(temp$qLHecdfCHqCF)])
}

################### Interpolation functions
interpolate.points <- function(invec, len.outvec, interp.mode='linear'){
  #Adds or subtracts points in a vector invec, maintaining its
  #distribution, in order to match an input length, len.outvec.
  #Is not meant to be called if len.outvec == len(invec)
  outvec <- rep(NA, len.outvec)
  
  if(length(invec) > len.outvec){
    #If fewer points are needed
    set.seed(seed=8675309, kind="Mersenne-Twister", normal.kind="Inversion")
    indices <- sort(sample.int(n=length(invec), len.outvec, replace=FALSE))
    outvec <- invec[indices]
  }else{
    #okay, trying a new technique here: 
    set.seed(seed=8675309, kind="Mersenne-Twister", normal.kind="Inversion")
    #Need one set of vectors for every point in the out vector, 
    #And one set of randomly selected points for the remainder
    indices <- sort(c( rep(seq(1:length(invec)), floor(len.outvec/length(invec)) ), 
                       sample.int(n=(length(invec)), size=(len.outvec%%length(invec)), replace=FALSE)))
    outvec <- invec[indices]
  }
  return(outvec)
}

interp.points <- function(startpoint, endpoint, len.out, mode){
  #Linera interpolation of len.outvalues between two points, including the starting point
  if(mode=='linear'){
    return(startpoint + ((endpoint-startpoint)/(len.out-1))*seq(from=0, by=1, to=(len.out-1)))
  }else{
    return(c(startpoint, rep(endpoint, len.out-1)))
  }
  #return(approx(x=c(startpoint, endpoint), y=NULL, n=len.out, method='linear')$x)
}

QMAP<-function(LH,CH,CF,args){
  lengthCF<-length(CF)
  lengthCH<-length(CH)
  lengthLH<-length(LH)
  
  
  if (lengthCF>lengthCH) maxdim=lengthCF else maxdim=lengthCH
  
  # first define vector with probabilities [0,1]
  prob<-seq(0.001,0.999,length.out=lengthCF)
  
  # initialize data.frame
  temp<-data.frame(index=seq(1,maxdim),CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),qLH=rep(NA,maxdim),ecdfCHqLH=rep(NA,maxdim),qCFecdfCHqLH=rep(NA,maxdim))
  SDF<-data.frame(index=seq(1,maxdim),CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),CFQM=rep(NA,maxdim),BCQM=rep(NA,maxdim),EDQM=rep(NA,maxdim),ERQM=rep(NA,maxdim))
  
  SDF$CF[1:lengthCF]<-temp$CF[1:lengthCF]<-CF
  SDF$CH[1:lengthCH]<-temp$CH[1:lengthCH]<-CH
  SDF$LH[1:lengthLH]<-temp$LH[1:lengthLH]<-LH
  
  # CHANGE FACTOR QMAP
  temp.CFsorted2<-temp[order(temp$CF),]
  temp.CFsorted2$qLH<-quantile(temp.CFsorted2$LH,prob,na.rm =TRUE)
  temp.CFsorted2$ecdfCHqLH<-ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE))
  temp.CFsorted2$qCFecdfCHqLH<-quantile(temp$CF,ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)),na.rm =TRUE)
  temp.CFQM2<-temp.CFsorted2[order(temp.CFsorted2$index),]
  #DEC 29 2014 Coarse Future and the SD output are now highly correlated
  
  
  ###### BIAS CORRECTION QMAP (~CDFt)
  temp.CFsorted<-temp[order(temp$CF),]
  temp.CFsorted$qCF<-quantile(temp.CFsorted$CF,prob,na.rm =TRUE)
  temp.CFsorted$ecdfCHqCF<-ecdf(temp$CH)(quantile(temp$CF,prob,na.rm =TRUE))
  temp.CFsorted$qLHecdfCHqCF<-quantile(temp$LH,ecdf(temp$CH)(quantile(temp$CF,prob,na.rm =TRUE)),na.rm =TRUE)
  temp.BCQM<-temp.CFsorted[order(temp.CFsorted$index),]
  
  ##### EQUIDISTANT QMAP
  
  temp.CFsorted$ecdfCFqCF<-ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE))
  
  temp.CFsorted$qLHecdfCFqCF<-quantile(temp$LH,ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE)),na.rm =TRUE)
  temp.CFsorted$qCHecdfCFqCF<-quantile(temp$CH,ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE)),na.rm =TRUE)
  temp.CFsorted$EquiDistant<-temp.CFsorted$CF+ temp.CFsorted$qLHecdfCFqCF-temp.CFsorted$qCHecdfCFqCF
  temp.EDQM<-temp.CFsorted[order(temp.CFsorted$index),]
  
  ##### EQUIRATIO QMAP
  temp.CFsorted$EquiRatio<-temp.CFsorted$CF* (temp.CFsorted$qLHecdfCFqCF/temp.CFsorted$qCHecdfCFqCF)
  temp.ERQM<-temp.CFsorted[order(temp.CFsorted$index),]
  
  ### Assign downscaled output to the SDF (Statistically Downscaled Future) list
  
  SDF$CFQM<-temp.CFQM2$qCFecdfCHqLH
  SDF$BCQM<-temp.BCQM$qLHecdfCHqCF
  SDF$EDQM<-temp.EDQM$EquiDistant
  SDF$ERQM<-temp.ERQM$EquiRatio
  return(SDF$CFQM)
}