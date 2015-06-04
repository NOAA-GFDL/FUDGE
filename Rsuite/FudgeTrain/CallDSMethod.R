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

CallDSMethod <- function(ds.method, train.predict, train.target, esd.gen, args=NULL, ds.var='irrelevant', 
                         att.table=NA, remove.missval.datasets = FALSE){
  #  library(CDFt)
  
#   if(remove.missval.datasets){
#     #In a multivariate workflow, removes datasets with excess missing values from the predictors
#     hist.missval.count <- lapply(train.predict, count.missvals)
#     print(hist.missval.count)
#     min.hist.missval <- min(unlist(hist.missval.count))
#     missval.index <- hist.missval.count <= min.hist.missval
#     if(sum(missval.index)!=length(train.predict)){
#       message('indexing by appropriate missing values')
#       hist.keep <- names(train.predict)[missval.index]
#       #Keep only datasets for which there are no excess missing values
#       train.predict <- train.predict[hist.keep]
#       esd.gen <- esd.gen[hist.keep]
#     }
#     #Now, do the same thing for the 
#   }
  #Finally, remove all missing values before going into downscaling
#   train.predict = lapply(train.predict, remove.missvals)
#   train.target = train.target[!is.na(train.target)]    
#   out.reference <- esd.gen[[1]]     #Save the locations of missing values in the output vector
#   esd.gen = lapply(esd.gen, remove.missvals)
  
  print(ds.method)
  
  out <- switch(ds.method, 
                #"simple.lm" = callSimple.lm(train.predict, train.target, esd.gen),
                'CDFt' = callCDFt(train.predict, train.target, esd.gen, args, ds.var, att.table),
                'simple.bias.correct' = callSimple.bias.correct(train.predict, train.target, esd.gen, args),
                "BCQM" = callBCQMv2(train.target, train.predict, esd.gen, args, ds.var, att.table), 
                "EDQM" = callEDQMv2(train.target, train.predict, esd.gen, args, ds.var, att.table), 
                "CFQM" = callCFQMv2(train.target, train.predict, esd.gen, args, ds.var, att.table), 
                "DeltaSD" = callDeltaSD(train.target, train.predict, esd.gen, args, ds.var, att.table),
                "multi.lm"=callMulti.lm(train.predict, train.target, esd.gen, args, ds.var, att.table),
                ReturnDownscaleError(ds.method))
#  if(remove.ds.missvals){
    #Same alternate missval removal structure as earlier
#     out.reference[!is.na(out.reference)] <- out
#     out <- out.reference
#  }
  return(out)  
}

ReturnDownscaleError <- function(ds.method){
  #Returns an error and stops the entire function if the DS method used is not supported.
  stop(paste("Downscale Method Error: the method", ds.method, "is not supported for FUDGE at this time."))
}

callMulti.lm <- function(pred, targ, new, args=NA, ds.var='tasmax', att_table = NULL){
  #'Calls R's lm() function using targ as a function
  #'of linear variability of one or more variables in 
  #'pred, and then uses the data in new to predict
  #'the future results. 
  #'Does not currently take any arguments
  
  #First, rename the predictor vars - if the ReadmultiVars
  #code truly is general, there is a good chance that this
  #is named by rip and var, not just var
  names(pred) <- lapply(att_table$hist.pred, '[[', 'var')
  #Now, create your data frame and make the linear model
    test.list <- c('target'=list(targ), pred)
    test.prod <- data.frame(test.list)
    lm.model <- lm(data=test.prod)
#     save(file="~/Code/test_multivar.R", list=c('lm.model', 'new'))
#     print(paste("save.file:~/Code/test_out.R"))
#     stop('examine future predictors and think accordingly')
    rip.list <- lapply(att_table$fut.pred, '[[', 'rip')
    rips <- unique(rip.list)
    out.list <- list()
    for(rip in rips){
      out.var <- paste(ds.var, rip, sep=".")
      #Get true/false vector to index out the data that you want
      rip.index <- (rip.list==rip)
      rip.data <- new[rip.index]
      #And to make sure that everything has the same names as
      #earlier, rename the list in the selected dataset
      names(rip.data) <- lapply(att_table$fut.pred[rip.index], '[[', 'var')
      out.list[[out.var]] <- predict.lm(lm.model, newdata=rip.data)
    }
  #return(predict.lm(lm.model, newdata=new))
    return(out.list)
}


callCDFt <- function (pred, targ, new, args, ds.var='null', att_table=NULL){
  #' Calls the CDFt function (Vrac et. al 2009) on a single variable
  #' predictor and target, returning the downscaled (DS) output.
  #' Takes as an argument
  #' npas: The number of quantiles to split the input dataset into. 
  #' If dev is 0 or 'default', uses the number of observations in either the
  #' future or historical datasets, depending upon which has fewer observations.
  #' dev : The number of deviations used when calculating. 
  #' @citation P.-A. Michelangeli, M. Vrac, H. Loukos. "Probabilistic downscaling approaches: 
     #' @citation Application to wind cumulative distribution functions", 
     #' @citation Geophys. Res. Lett., doi:10.1029/2009GL038401, 2009)
     
     ###Obtain required arguments (and throw errors if not specified)
     if(!is.null(args$dev)){
       dev <- args$dev
     }else{
       stop(paste("CDFt Method Error: parameter dev was missing from the args list"))
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
       if(is.null(args)){
         temp <- CDFt(targ, pred, new.data, npas=length(targ))$DS
       }else{
         ##Note: if any of the input data parameters are named, CDFt will 
         ## fail to run with an 'unused arguments' error, without any decent
         ## explanation as to why. This way works.
         args.list <- c(list(targ, pred, new.data), list(npas=npas, dev=dev))
         temp <- do.call("CDFt", args.list)$DS
       }
       #return(as.numeric(temp))
       out.list[[out.var.name]] <- as.numeric(temp)
     }
     return(out.list)
}

callSimple.bias.correct <- function(pred, targ, new, args){
  #'Performs a simple bias correction adjustment to a 
  #'single variable series, applying the mean difference 
  #'between the predictor and target datasets in the 
  #'historical period to the esd.gen dataset
  #'to generate downscaled data. 
  #'It takes no arguments. 
  out.var.name <- names(new)
  
  bias <- mean(unlist(pred)-targ)
  new.targ <- unlist(new)-bias
  out.list <- list()
  out.list[[out.var.name]] <- new.targ
  return(out.list)
}

callDeltaSD <- function(LH,CH,CF,args,ds.var='null', att_table=NULL){
  #'@author carlos.gaitan@noaa.gov
    #'@description The script uses the Delta Method to downscale coarse res. climate variables  
    #'over a single variable
    #'@param LH: Local Historical (a.k.a. observations)
    #'@param CH: Coarse Historical (a.k.a. GCM historical)
    #'@param CF: Coarse Future (a.k.a GCM future)
    #'@param args: A list containing three arguments: 
    #'  @param deltatype, one of 'mean' or 'median', which will be used to determine 
    #'  what single value will be used for the difference between the CH and CF
    #'  @param deltaop, one of 'ratio' or 'add', which will be used to determine 
    #'  the mthod for calculating the delta
    #'  @param keep.zeroes, a value of TRUE or FALSE that controls whether to use all days for
    #'  calculating the delta (TRUE), or use only those days for which the delta was greater
    #'  than zero (FALSE)
    #'Uses the difference (ratio or subtraction) between CF and CH means or medians to calculate
    #' a delta that is applied to the LH (observational) data
    #'@return SDF: Downscaled Future (Local)
    ########################################
    #Note: preferred behavior is to truncate vectors
    #rather than randomly sampling iff too short.
    
    out.var.name <- names(CF)
    
    #Unlist data from its input form
    CH <- unlist(CH)
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
    #Prepare to loop by RIP
    out.list <- list()
    rip.list <- lapply(att_table$fut.pred, '[[', 'rip')
    for(rip in rip.list){
      #Select the future predictor vars that you are interested in
      out.var.name <- paste0(ds.var, ".", rip)
      #Unlist the ensemble members
      rip.index <- (rip.list==rip)
      CF.data <- CF[rip.index]
      CF.data <- unlist(CF.data)
      #Decide how many iterations of the delta method to perform
      #based on the relative lengths of the historical
      #and future data (future should be >= historical) #Switch 1-28 from <  
      if(length(LH) >= length(CF.data)){
        #If the future and historical periods are unequal, truncate the vectors
        #That...raises an interesting question: should the CH vector be truncated as well?
        #Technically, it doesn't need to be for the code to work...
        if(keep.zeroes){
          SDF <- LH[1:length(CF.data)]
          out.temp <- delta.downscale(LH[1:length(CF.data)], CH, CF.data, deltatype, deltaop, keep.zeroes)
          SDF[SDF!=0] <- out.temp
        }else{
          SDF <- delta.downscale(LH[1:length(CF.data)], CH, CF.data, deltatype, deltaop)
        }
      }else{
        #Otherwise, if the vectors are uneven then calculate n+1 deltas, 
        #where n=length(CF.data)/length(LH)
        write.len <- 1
        out.len <- length(CF.data)
        in.len <- length(LH)
        SDF <- rep(NA, out.len)
        #vector comparisons take a long time relative to other things
        if(keep.zeroes){
          comp.indices <- which(LH!=0)
        }
        while(write.len < out.len){
          #delta.downscale removes NAs in the output vector
          tempvec <- delta.downscale(LH, CH, CF.data[write.len:(write.len + in.len)], 
                                     deltatype, deltaop, keep.zeroes)
          if(keep.zeroes){
            sd.write.indices <- (write.len-1) + comp.indices
            #Remove any that might lead to a longer write index
            sd.write.indices <- sd.write.indices[sd.write.indices <= out.len] 
            SDF[sd.write.indices] <- tempvec
          }else{
            SDF[write.len:length(tempvec)] <- tempvec
          }
          write.len <- write.len + in.len
        }
      }
      num.na <- (sum(is.na(SDF)))
      if(num.na > 0){
        print(num.na)
        stop("Error in DeltaSD: NAs not in out being introduced from somewhere")
      }
      out.list[[out.var.name]] <- SDF
    }
    return(out.list)
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


callEDQMv2<-function(LH,CH,CF,args,ds.var='null', att_table=NULL){ 
  #'Performs an equidistant correction adjustment on a single variable
  #'over the historical period, in order to generate downscaled data.
  #'@param LH: Local Historical (a.k.a. observations)
    #'@param CH: Coarse Historical (a.k.a. GCM historical)
    #'@param CF: Coarse Future (a.k.a GCM future)
    #'Contains no other arguments in the args parameter.
    #'@citation Li et. al. 2010
    
    #Unlist data from its input form
    CH <- unlist(CH)
    lengthCH<-length(CH)
    lengthLH<-length(LH)
    
    #prepare to loop by rip over the dataset
    out.list <- list()
    rip.list <- lapply(att_table$fut.pred, '[[', 'rip')
    for(rip in rip.list){
      #Select the future predictor vars that you are interested in
      out.var.name <- paste0(ds.var, ".", rip)
      #Unlist the ensemble members
      rip.index <- (rip.list==rip)
      CF.data <- CF[rip.index]
      CF.data <- unlist(CF.data)
      
      lengthCF<-length(CF.data)
      
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
      temp$CF[1:lengthCF]<-CF.data
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
      out.list[[out.var.name]] <- temp$EquiDistant[!is.na(temp$EquiDistant)]
    }
    return(out.list)
    #return(temp$EquiDistant[!is.na(temp$EquiDistant)])
}

###CG DS method
callCFQMv2<-function(LH,CH,CF,args){
  #'Performs an change-factor correction adjustment on a single variable
  #'over the historical period, in order to generate downscaled data.
  #' @param LH: Local Historical (a.k.a. observations)
  #' @param CH: Coarse Historical (a.k.a. GCM historical)
  #' @param CF: Coarse Future (a.k.a GCM future)
  #' Contains no other arguments in the args parameter.
  #'2-27-2015 edit: added an argument in args, sort, for determining
  #'whether data is sorted by the CH ('historical'), CF ('future'), 
  #'or LH ('target') vectors.

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
  #Save output var name for later
  out.var.name <- names(CF)
  
  #Unlist data from its input form
  CF <- unlist(CF)
  CH <- unlist(CH)
  
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
     print('sorting')
  temp.opt.sorted<-temp[order(temp[[sort.opt]]),] #sorts by sort.opt
#      save(file="~/Code/testing/test_out.R", list=c('temp.opt.sorted', 'temp', 'prob'))
#      stop("see where this is throwing an error")
  #i.e. all temp.LHsorted to temp.CFsorted
     print('quantile calculating')
  temp.opt.sorted$qLH<-quantile(temp.opt.sorted$LH,prob,na.rm =TRUE) #removed all 1:lengthCF #previously with TRUE on all na.rm
     print('prob distribution')
  temp.opt.sorted$ecdfCHqLH<-ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE))
  temp.opt.sorted$qCFecdfCHqLH<-quantile(temp$CF,ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)),na.rm =TRUE) #Added parenthesis befpre ecdf
     #temp<-temp.opt.sorted[order(temp.opt.sorted$index, na.last=TRUE),] # previous version, tested against cinnamon
  temp[1:lengthCF]<-temp.opt.sorted[order(temp.opt.sorted$index, na.last=TRUE),] #, na.last=FALSE #removed order #temp.opt.sorted$index
  
  #SDF<-temp$qCFecdfCHqLH #This is the version that got tested for regression against cinnamon, and that we passed out
     print(summary(temp.opt.sorted))
     print(lengthCF)
     SDF<-temp$qCFecdfCHqLH[1:lengthCF] #This is the version that is getting tested as of 5-18
  #save(file="~/Code/testing/test_out.R", list=c('SDF'))
  print(summary(SDF))
  print(length(SDF))
#  print(length(SDF[!is.na(SDF)]))
#  cor.vector <- c("temp$CH", "temp$LH", "temp$CF")
#   for (j in 1:length(cor.vector)){
#     cor.var <- cor.vector[j]
#     cor.out <- eval(parse(text=cor.var))
#     if(length(cor.out) > length(SDF)){
#       out.cor <- cor(as.vector(SDF), as.vector(cor.out)[1:length(SDF)], use='pairwise.complete.obs')
#     }else{
#       out.cor <- cor(as.vector(SDF)[1:length(cor.out)], as.vector(cor.out), use='pairwise.complete.obs')
#     }
#     print(paste("temp.out", ",", cor.var, "):", out.cor, sep=""))
#   }
  out.list <- list()
  #out.list[[out.var.name]] <- SDF[!is.na(SDF)]
     out.list[[out.var.name]] <- SDF[1:lengthCF]
     print(paste('lengthCF:', lengthCF))
  #return(SDF[!is.na(SDF)])
  return(out.list)
}

callCFQMv3<-function(LH,CH,CF, args)
{
  message('entering CFQMv3')
  # Obtain options
  if(!is.null(args$sort)){
    #can be one of 'future' or 'historical'
    sort.opt <- args$sort
    if(sort.opt=='future'){
      ORDERING <- 'CF'
    }else if(sort.opt=='historical'){
      ORDERING <- 'CH'
    }else if(sort.opt=='target'){
      ORDERING <- 'LH'
    }else{
      stop(paste("CFQMv3 Downscaling Error: arg sort was", sort.opt, "not 'future', 'historical', or 'target'"))
    }
  }else{
    stop(paste("CFQMv3 Downscaling Error: sort not found in args"))
  }
  #Save output var name for later
  out.var.name <- names(CF)
  
  #Unlist data from its input form
  CF <- unlist(CF)
  CH <- unlist(CH)
  
  lengthCF<-length(CF)
  lengthCH<-length(CH)
  lengthLH<-length(LH)
  
  if (lengthCF>lengthCH) maxdim=lengthCF else maxdim=lengthCH
  if (lengthCF<lengthCH) mindim=lengthCF else mindim=lengthCH
  
  lengthCFna<-length(which(!is.na(CF))) # count number of obs without NA
  lengthCHna<-length(which(!is.na(CH)))
  
  if (lengthCFna<lengthCHna) mindimNA=lengthCFna else mindimNA=lengthCHna
  
  # first define vector with probabilities [0,1]
  prob<-seq(0.001,0.999,length.out=lengthCF)
  
  # initialize data.frame
  message('initialize data frame')
  temp<-data.frame(index=seq(1,maxdim),CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),qLH=rep(NA,maxdim),ecdfCHqLH=rep(NA,maxdim),qCFecdfCHqLH=rep(NA,maxdim))
  SDF<-data.frame(index=seq(1,maxdim),CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),CFQM=rep(NA,maxdim))
  
  #initialize output list
  message('initialize output list')
  outputlist<-list(rep(NA,lengthCF)) #previously maxdim
  names(outputlist) <- out.var.name
  
  
  SDF$CF[1:lengthCF]<-temp$CF[1:lengthCF]<-CF
  SDF$CH[1:lengthCH]<-temp$CH[1:lengthCH]<-CH
  SDF$LH[1:lengthLH]<-temp$LH[1:lengthLH]<-LH
  
  message(ORDERING)
  
  if (ORDERING=="CF"){
    # CHANGE FACTOR ORDERED BY COARSE FUTURE
    message('sorting')
    temp.CFsorted2<-temp[order(temp$CF),]
    message('qantiles')
    #save(file="~/Code/testing/test_out.R", list = c('temp.CFsorted2', 'prob', 'temp', 'lengthCF'))
    #stop('examine the output')
    temp.CFsorted2$qLH[1:lengthCF]<-quantile(temp.CFsorted2$LH,prob,na.rm =TRUE) #Output of length lengthCF
    message('ecdf')
    temp.CFsorted2$ecdfCHqLH[1:lengthCF]<-ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)) #Output of length lengthCF
    message('quantiles of ECDF')
    temp.CFsorted2$qCFecdfCHqLH[1:lengthCF]<-quantile(temp$CF,ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)),na.rm =TRUE) #Output of length lengthCF
    save(file="~/Code/testing/test_out.R", list = c('temp.CFsorted2', 'prob', 'temp'))
    print(length(which(is.na(temp.CFsorted2))))
    temp.CFQM2<-temp.CFsorted2[order(temp.CFsorted2$index),] #By this point, it has successfully ordered the first lengthCF members of the output data
    print("Downscaled data ordered using the Coarse Future dataset")
    
  }
  
  if (ORDERING=="LH"){
    # CHANGE FACTOR ORDERED BY LOCAL HISTORICAL
    temp.LHsorted2<-temp[order(temp$LH),]
    
    temp.LHsorted2$qLH[1:lengthCF]<-quantile(temp.LHsorted2$LH,prob,na.rm =TRUE)
    temp.LHsorted2$ecdfCHqLH[1:lengthCF]<-ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE))
    temp.LHsorted2$qCFecdfCHqLH[1:lengthCF]<-quantile(temp$CF,ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)),na.rm =TRUE)
    
    temp.CFQM2<-temp.LHsorted2[order(temp.LHsorted2$index),]
    print("Downscaled data ordered using the Local Historical dataset")
    
    
  }
  
  if (ORDERING=="CH"){
    # CHANGE FACTOR ORDERED BY COARSE HISTORICAL
    temp.CHsorted2<-temp[order(temp$CH),]
    
    temp.CHsorted2$qLH[1:lengthCF]<-quantile(temp.CHsorted2$LH,prob,na.rm =TRUE)
    temp.CHsorted2$ecdfCHqLH[1:lengthCF]<-ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE))
    temp.CHsorted2$qCFecdfCHqLH[1:lengthCF]<-quantile(temp$CF,ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)),na.rm =TRUE)

    temp.CFQM2<-temp.CHsorted2[order(temp.CHsorted2$index),]
    print("Downscaled data ordered using the Coarse Historical dataset")
    
  }
  
  ### Assign downscaled output to the SDF (Statistically Downscaled Future) list
#   save(file="~/Code/testing/test_out.R", list=c('outputlist', 'SDF', 'lengthCF', 'lengthCH', 'lengthLH'))
#   stop('examine output')
  
  #SDF$CFQM<-temp.CFQM2$qCFecdfCHqLH
  outputlist[[out.var.name]][1:lengthCF]<-temp.CFQM2$qCFecdfCHqLH[!is.na(temp.CFQM2$qCFecdfCHqLH)] #SDF$CFQM[1:lengthCF]
#  outputlist[[out.var.name]][1:lengthCF]<-temp.CFQM2$qCFecdfCHqLH[1:lengthCF] #SDF$CFQM[1:lengthCF]
#   save(file="~/Code/testing/test_out.R", list=c('outputlist', 'lengthCF', 'SDF'))
#   stop('examine downscaled output')
  #outputlist[[out.var.name]]
  
#   if (ORDERING=="CF"){
#     print(sprintf("Correlation %s",cor(SDF$CFQM[1:mindim],SDF$CF[1:mindim])))}
#   if (ORDERING=="LH"){
#     print(sprintf("Correlation %s",cor(SDF$CFQM[1:mindim],SDF$LH[1:mindim])))}
#   if (ORDERING=="CH"){
#     print(sprintf("Correlation %s",cor(SDF$CFQM[1:mindim],SDF$CH[1:mindim])))}
  
  
  #return(SDF) or
  return(outputlist)
}


callBCQMv2<-function(LH,CH,CF,args,ds.var='null', att_table=NULL){
  #'Performs an bias-correction quantile mapping on a single variable
  #'over the historical period, in order to generate downscaled data.
  #' @param LH: Local Historical (a.k.a. observations)
  #' @param CH: Coarse Historical (a.k.a. GCM historical)
  #' @param CF: Coarse Future (a.k.a GCM future)
  #' Contains no other arguments in the args parameter.
  #' 
  #' Note: Even though there is no training step on this method, and no
  #' intention of implementing multivar downscaling for the QM methods, 
  #' they need to be named to fit into the new indexing scheme. 

     #Unlist data from its input form
     CH <- unlist(CH)
     
  lengthCH<-length(CH)
  lengthLH<-length(LH)
     #prepare to loop by rip over the dataset
     out.list <- list()
     rip.list <- lapply(att_table$fut.pred, '[[', 'rip')
     for(rip in rip.list){
       #Select the future predictor vars that you are interested in
       out.var.name <- paste0(ds.var, ".", rip)
       #Unlist the ensemble members
       rip.index <- (rip.list==rip)
       CF.data <- CF[rip.index]
       CF.data <- unlist(CF.data)
       lengthCF<-length(CF.data)

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
  
  out.list[[out.var.name]] <- temp$qLHecdfCHqCF[!is.na(temp$qLHecdfCHqCF)]
     }
  return(out.list)
}