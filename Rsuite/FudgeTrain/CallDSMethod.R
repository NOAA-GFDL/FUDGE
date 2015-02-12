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
#' TODO: Find a better name for general.bias.corrector
#' TODO: Modify methods other than simple.lm for this kind of dataset manipulation. 
#' TODO: add explicit rather than implicit multivariate support
#' TODO: Find better way to initialize the output storage vectors
#' TODO: Seriously, THERE HAS GOT TO BE A LESS COMPLEX WAY

CallDSMethod <- function(ds.method, train.predict, train.target, esd.gen, args=NULL, ds.var='irrelevant'){
  #  library(CDFt)
  #print(paste("start time:", date()))
  out <- switch(ds.method, 
                "simple.lm" = callSimple.lm(train.predict, train.target, esd.gen),
                'CDFt' = callCDFt(train.predict, train.target, esd.gen, args),
                'simple.bias.correct' = callSimple.bias.correct(train.predict, train.target, esd.gen, args),
                'general.bias.correct' = callGeneral.Bias.Corrector(train.predict, train.target, esd.gen, args),
                "BCQM" = callBiasCorrection(train.target, train.predict, esd.gen, args), 
                "EDQM" = callEquiDistant(train.target, train.predict, esd.gen, args), 
                "CFQM" = callChangeFactor(train.target, train.predict, esd.gen, args), 
                "CFQMv2" = callCFQMv2(train.target, train.predict, esd.gen, args),
                "BCQMv2" = callBCQMv2(train.target, train.predict, esd.gen, args),
                "EDQMv2" = callEDQMv2(train.target, train.predict, esd.gen, args),
                "DeltaSD" = callDeltaSD(train.target, train.predict, esd.gen, args), #, ds.var)
                'Nothing' = callNothing(train.target, train.predict, esd.gen, args),
                ReturnDownscaleError(ds.method))
  #print(paste("stop time:", date()))
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
    #print(lm.intercept)
    #print(lm.slope)
    return( lm.intercept + unlist(x)*lm.slope)  # + unlist(x)*lm.slope2) #Messily edited multivariate fxn? I don't remember this.
  }
  #insert save command for saving 
  #May be rendered obsolete if going to a model that uekeeps everything in memory and trains each time.
  return(trained.function(new))
}

callMulti.lm <- function(pred, targ, new, args, ds.lengths){
  #Creates a simple linear model without a cross-validation step. 
  #Mostly used to check that the procedure is working
  #esdgen.lengths <- length(new[[seq(1:length(new))]][[1]])
  
  #Initialize output structure
  print("inside ds fxn")
  #ds.lengths <- lapply(new, attr, "ds.length")
  #output <- rep(list("empty"), length(names(new)))
  #print(length(output))
  #names(output) <- names(new)
 # print(summary(output))
  output <- list()
  print("looping on new")
  print(ds.lengths)
  print(mode(ds.lengths))
  for(el in 1:length(new)){
    print(el)
    print(ds.lengths[[el]])
    new.list <- rep(NA, ds.lengths[[el]])
    output[[names(new)[el]]] <- new.list
    print("assignment")
    print(length(output[[el]]))
    print(paste("lenght of output:", length(output)))
  }
  
  #First, get matrix of predictor values to use for prediction
  pred.mat <- matrix(unlist(pred, use.names=FALSE), ncol=length(pred), byrow=FALSE)
  print("dimensions of predictor matrix")
  print(dim(pred.mat))
  print(length(unlist(targ)))
  lm.coef <- coef(lm(unlist(targ) ~ pred.mat))
  
  for (i in 1:length(new)){ #new is organized first by RIP, then component
    new.name <- names(new)[i]
    #initialize ouput vector
    #out.vec <- rep(NA, length(new$rip[1]))
    new.mat <- matrix(unlist(new[i], use.names=FALSE), ncol=length(pred), byrow=FALSE)
    #Note that this assumes that the order of the points/vars
    #is the same for the predictor as the esd.gen datasets
    #Multiply over all the columns
    no.intercept <- sweep(x=new.mat, MARGIN=2, lm.coef[2:length(lm.coef)], "*")
    #Replace any NA values with 0 for the addition step
    no.intercept[is.na(no.intercept)] <- 0
    no.intercept <- apply(X=no.intercept, MARGIN=1, "sum")
    outvec <- no.intercept + lm.coef[1]
    print(mode(outvec))
    print(summary(outvec))
    output[new.name] <- list(outvec)
    print(summary(output))
  }
  print(summary(output))
  return(output)
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
      npas <- 10
      stop(paste("Error in callCDFt: npas shouuld be greater than dev, but npas was", 
                 npas, "and dev was", dev))
    }
  }else{
    stop(paste("CDFt Method Error: parameter npas was missing from the args list"))
  }
  if(is.null(args)){
    #return(CDFt(targ, pred, new, npas=length(targ))$DS)
    temp <- tryCatch({CDFt(targ, pred, new, npas=length(targ))$DS}, 
                     error=function(err){
                         err$message <- paste(err$message,"This error often displays when the samples",
                                              "input to CDFt are uneven and/or too small.\n",
                                              "Please check your input vectors and pre-ds adjustment.\n"
                         )
                       stop(err)
                     }
    )
    return(as.numeric(temp))
  }else{
    ##Note: if any of the input data parameters are named, CDFt will 
    ## fail to run with an 'unused arguments' error, without any decent
    ## explanation as to why. This way works.
    args.list <- c(list(targ, pred, new), list(npas=npas, dev=dev))
    temp <- tryCatch({do.call("CDFt", args.list)$DS}, 
                     error=function(err){
                         err$message <- paste(err$message,"This error often displays when the samples",
                                              "input to CDFt are uneven and/or too small.\n",
                                              "Please check your input vectors and pre-ds adjustment.\n"
                         )
                       stop(err)
                     }
    )
    return(as.numeric(temp))
  }
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

callBiasCorrection <- function(LH, CH, CF, args){
  #'Performs a bias correction adjustment with parameters
  #'that I will ask CG about tomorrow
  # first define vector with probabilities [0,1]
  # LH: Local Historical (a.k.a. observations)
  # CH: Coarse Historical (a.k.a. GCM historical)
  # CF: Coarse Future (a.k.a GCM future)
  # args: A vector of arguments to the function. 
  #currently takes one: preserve.order="true"
  #or preserve.order="false"
  #   if(!is.null(args$size)){
  #     size <- args$size
  #     args$size <- NULL
  #   }else{
#   if(!is.null(args$flip)){
#     if(args$flip=="true"){
#       size <- length(CF)
#       prob<-c(0.001:1:size)/size
#       #check the order preservation status
#       in.sort <- order(CF)
#       CF.out <- CF[in.sort]
#       # QM Change Factor
#       #
#       #SDF<-quantile(LH,ecdf(CH)(quantile(CF.out,prob)),names=FALSE)
#       SDF<-quantile(LH,ecdf(CF.out)(quantile(CH,prob)),names=FALSE)
#       #CEW: creation of historical values commented out for the moment
#       #SDH<-quantile(LH,ecdf(CH)(quantile(CH,prob)),names=FALSE)
#       #SDoutput<-list("SDF"=SDF,"SDH"=SDH)
#       SDF <- SDF[order(in.sort)]
#     }
#   }else{
  size <- length(CF)
  prob<-seq(from=1/size, by=1, to=size)/size
  #check the order preservation status
    in.sort <- order(CF)
    CF.out <- CF[in.sort]
  # QM Change Factor
  #
  SDF<-quantile(LH,ecdf(CH)(quantile(CF.out,prob)),names=FALSE)
  #SDF<-quantile(LH,ecdf(CF.out)(quantile(CH,prob)),names=FALSE)
  #CEW: creation of historical values commented out for the moment
  #SDH<-quantile(LH,ecdf(CH)(quantile(CH,prob)),names=FALSE)
  #SDoutput<-list("SDF"=SDF,"SDH"=SDH)
    SDF <- SDF[order(in.sort)]
  return (SDF)
}

callEquiDistant <- function(LH, CH, CF, args){
  #'Performs an equidistant correction adjustment with parameters
  #'that I will ask CG about tomorrow
  # first define vector with probabilities [0,1]
  # LH: Local Historical (a.k.a. observations)
  # CH: Coarse Historical (a.k.a. GCM historical)
  # CF: Coarse Future (a.k.a GCM future)
  #'Cites Li et. al. 2010
  size <- length(CF)
  prob<-seq(from=1/size, by=1, to=size)/size
  
  #check order preservation status
    in.sort <- order(CF)
#    CF.out <- CF[in.sort]

  #Create numerator and denominator of equation
  #First scale with local historical and reorder
  temporal<-quantile(LH,(ecdf(CF)(quantile(CF,prob))),names=FALSE)
  temporal <- temporal[in.sort]
  
  #And then scale with climate historical
  temporal2<-quantile(CH,(ecdf(CF)(quantile(CF,prob))),names=FALSE)
  temporal2 <- temporal2[in.sort]
  
  # EQUIDISTANT CDF (Li et al. 2010)
  SDF<-CF + temporal-temporal2
  #CEW creation of downscaled historical values turned off for the moment
  #SDH<-CH + temporal-temporal2
  #SDoutput<-list("SDF"=SDF,"SDH"=SDH)

    #SDF <- SDF[order(in.sort)] 
  return (SDF)
}

callChangeFactor <- function(LH, CH, CF, args){
  #'The script uses the Quantile Mapping Change Factor
  #'(Ho, 2012) CDF to downscale coarse res. climate variables
  #'@param LH: Local Historical (a.k.a. observations)
    #'@param CH: Coarse Historical (a.k.a. GCM historical)
    #'@param CF: Coarse Future (a.k.a GCM future)
    #'@param args: named list of arguments for the function

    #Edited 1-9-15 to organize vectors by the CF vector
    size <- length(CF)
    # first define vector with probabilities [0,1]
    prob<-seq(from=1/size, by=1, to=size)/size
    
    #Check for arg for specifying calendar order preservation
#     LH.sorted <- order(LH)
#     LH.interp <- interpolate.points(LH, size, 'linear')
#     LH.sortorder <- interpolate.points(LH.sorted, size, 'repeat')
    #LH.order <- order(LH.interp)

    # QM Change Factor
    SDF<-quantile(CF,(ecdf(CH)(quantile(LH,prob))),names=FALSE)
    ##CEW: creation of historical quantiles turned off for the moment
    #SDH<-quantile(CH,(ecdf(CH)(quantile(LH,prob))),names=FALSE)
    #SDoutput<-list("SDF"=SDF,"SDH"=SDH)
    
    SDF <- SDF[order(CF)]
    return (SDF)
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
      keep.zeroes <- args$keep.zeroes
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
#        print('keeping zeroes')
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
#     print('activating keep zeroes case')
#     print(summary(delta.fut))
    delta.fut <- delta.fut[!is.na(delta.fut) & delta.fut!=0]
    delta.targ <- delta.targ[!is.na(delta.targ) & delta.targ!=0]
    delta.hist <- delta.hist[!is.na(delta.hist) & delta.hist!=0]
#     print(summary(delta.fut))
#     print(summary(delta.hist))
#     print(summary(delta.targ))
  }
  if(deltaop=='add'){
    #Downscale by difference delta
    delta<-do.call(deltatype, list(delta.fut))-do.call(deltatype, list(delta.hist))
    out <- delta.targ + delta #CHANGED FROM delta.fut
  }else if(deltaop=='ratio'){
    #Downscale by percentage delta (never negative, but aoocasionally NaN)
    delta<-do.call(deltatype, list(delta.fut))/do.call(deltatype, list(delta.hist))
#     print(paste('delta:', delta))
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

# callNothing <- function(pred=NA, targ=NA, new=NA, args=NA){
#   #Does absolutely nothing to the downscaling values of the current 
#   #function. 
# #   print('inside method')
# #   print(summary(new))
#   return(new)
# }

interpolate.points <- function(invec, len.outvec, interp.mode='linear'){
  #Adds or subtracts points in a vector invec, maintaining its
  #distribution, in order to match an input length, len.outvec.
  #Is not meant to be called if len.outvec == len(invec)
  #Note: successfully resisted urge to name it Procrustes.
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
#     #If more points are needed
#     #You will add either one or two points each time
#     #Add the first and last indices to the output vector
#     outvec[1] <- invec[1]
#     outvec[length(outvec)] <- invec[length(invec)] 
#     #Remove for next step
#     #Note: this will fail for n < 3 points, but that seems unlikely here
#     invec <- invec[seq(2,(length(invec)-1))] #-1
#     
#     changevec <- round((length(invec)/(len.outvec-2))*seq(1:(len.outvec-2)))
#     #changevec <- changevec +1
#     #changevec[changevec==len.outvec] <- len.outvec-1
#     #unique.indices <- unique(changevec)
#     
#     for (i in 1:(length(invec))){ #unique.indices
#       #index <- unique.indices[i]
#       index <- i
#       index.map <- which(changevec==index)[1]
#       outvec[index.map+1] <- invec[i]
#     }
#     interp.indices <- which(!is.na(outvec))
#     for(j in 2:length(interp.indices)){
#       startval <- outvec[interp.indices[j-1]]
#       endval <- outvec[interp.indices[j]]
#       vec.len <- (interp.indices[j]-interp.indices[j-1] + 1)
#       if(vec.len > 2){ #If the two are not right next to each other
#         outvec[interp.indices[j-1]:interp.indices[j]] <- interp.points(startval, endval, vec.len, 
#                                                                        mode=interp.mode)
#       }
#     }
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

####CG DS method
callCFQMv2<-function(LH,CH,CF,args)
{ 
  lengthCF<-length(CF)
  lengthCH<-length(CH)
  lengthLH<-length(LH)
  
  
  if (lengthCF>lengthCH) maxdim=lengthCF else maxdim=lengthCH
  
  # first define vector with probabilities [0,1]
  prob<-seq(0.001,0.999,length.out=lengthCF)
  
  # initialize data.frame
  temp<-data.frame(index=seq(1,maxdim),CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),
                   qLH=rep(NA,maxdim),ecdfCHqLH=rep(NA,maxdim),qCFecdfCHqLH=rep(NA,maxdim))
  temp$CF[1:lengthCF]<-CF
  temp$CH[1:lengthCH]<-CH
  temp$LH[1:lengthLH]<-LH
  
  temp.LHsorted<-temp[order(temp$LH),]
  temp.LHsorted$qLH<-quantile(temp.LHsorted$LH,prob,na.rm =TRUE)
  temp.LHsorted$ecdfCHqLH<-ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE))
  temp.LHsorted$qCFecdfCHqLH<-quantile(temp$CF,ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)),na.rm =TRUE)
  temp.final<-temp.LHsorted[order(temp.LHsorted$index),]
  
  SDF<-temp.final$qCFecdfCHqLH
  return(SDF)
}

callCFQMv2<-function(LH,CH,CF,args){
  #'Calls the latest version of the CFQM function
  #'as of 12-29
  lengthCF<-length(CF)
  lengthCH<-length(CH)
  lengthLH<-length(LH)  
  
  if (lengthCF>lengthCH) maxdim=lengthCF else maxdim=lengthCH
  
  # first define vector with probabilities [0,1]
  prob<-seq(0.001,0.999,length.out=lengthCF)
  
  # initialize data.frame
  temp<-data.frame(index=seq(1,maxdim),CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),
                   qLH=rep(NA,maxdim),ecdfCHqLH=rep(NA,maxdim),qCFecdfCHqLH=rep(NA,maxdim))
  temp$CF[1:lengthCF]<-CF
  temp$CH[1:lengthCH]<-CH
  temp$LH[1:lengthLH]<-LH
  
  temp.LHsorted<-temp[order(temp$LH),]
  temp.LHsorted$qLH<-quantile(temp.LHsorted$LH,prob,na.rm =TRUE)
  temp.LHsorted$ecdfCHqLH<-ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE))
  temp.LHsorted$qCFecdfCHqLH<-quantile(temp$CF,ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)),na.rm =TRUE)
  temp<-temp.LHsorted[order(temp.LHsorted$index, na.last=FALSE),]
  
  SDF<-temp$qCFecdfCHqLH
  return(SDF)
}

callBCQMv2<-function(LH,CH,CF,args){
  #' Calls latest version od BCQM function
  #' as of 12-29
  lengthCF<-length(CF)
  lengthCH<-length(CH)
  lengthLH<-length(LH)
    
  if (lengthCF>lengthCH) maxdim=lengthCF else maxdim=lengthCH
  
  # first define vector with probabilities [0,1]
  prob<-seq(0.001,0.999,length.out=lengthCF)
  
  # initialize data.frame
  temp<-data.frame(index=seq(1,maxdim),CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),
                   qCF=rep(NA,maxdim),ecdfCHqCF=rep(NA,maxdim),qLHecdfCHqCF=rep(NA,maxdim))
  temp$CF[1:lengthCF]<-CF
  temp$CH[1:lengthCH]<-CH
  temp$LH[1:lengthLH]<-LH
  
  temp.CFsorted<-temp[order(temp$CF),]
  temp.CFsorted$qCF<-quantile(temp.CFsorted$CF,prob,na.rm =TRUE)
  temp.CFsorted$ecdfCHqCF<-ecdf(temp$CH)(quantile(temp$CF,prob,na.rm =TRUE))
  temp.CFsorted$qLHecdfCHqCF<-quantile(temp$LH,ecdf(temp$CH)(quantile(temp$CF,prob,na.rm =TRUE)),na.rm =TRUE)
  temp<-temp.CFsorted[order(temp.CFsorted$index),]
  
  #SDF<-temp.final$qCFecdfCHqLH
  return(temp$qLHecdfCHqCF)
}

callEDQMv2<-function(LH,CH,CF,args){ 
  #' Calls latest version odf the EDQM function
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
  temp$CF[1:lengthCF]<-CF
  temp$CH[1:lengthCH]<-CH
  temp$LH[1:lengthLH]<-LH
  
  temp.CFsorted<-temp[order(temp$CF),]
  temp.CFsorted$ecdfCFqCF<-ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE))
  
  temp.CFsorted$qLHecdfCFqCF<-quantile(temp$LH,ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE)),na.rm =TRUE)
  temp.CFsorted$qCHecdfCFqCF<-quantile(temp$CH,ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE)),na.rm =TRUE)
  temp.CFsorted$EquiDistant<-temp.CFsorted$CF+ temp.CFsorted$qLHecdfCFqCF-temp.CFsorted$qCHecdfCFqCF
  temp<-temp.CFsorted[order(temp.CFsorted$index),]
  print(summary(temp))
  print(names(temp))
  
  #SDF<-temp$qCFecdfCHqLH
  return(temp$EquiDistant)
}
