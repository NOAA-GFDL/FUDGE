#qm_ds.R
#Contains a wrapper/setup method (callQM) 
#and several quantile mapping methods
#(BCQM, CFQM, EDQM)
#CFQMv2 is kept for reference purposes - it's been repalced by v3

callQM <- function(OH, MH, MF, args, type="", tmasks=list(), 
                   missval.args=list(minsamplesize=1)){
  #'Wrapper for the single-var, single-point 
  #'quantile mapping downscaling methods, 
  #'which share a lot of DNA with each other.
  #'For now, even methods that can do the training once
  #'all do training within looping by ensemble.
  
  #Prepare output and indexing
  out.data <- array(NA, dim=dim(MF[[1]]))
  nrips <- dim(out.data)[4]
  if(train_qcmask){
    qc <- array(1, dim(out.data))
  }else{
    qc <- NULL
  }
  #Prepare indexing mask
  if (tmasks$time_mask_overlap){
    index.mask <- tmasks$trim$masks[[w]]
    olap=T
  }else{
    index.mask <- tmasks$MF$masks[[w]]
    olap=F
  }
  MF_mask <- missval.args$pr$MF
  for (n in 1:nrips){
    MF.slice <- lapply(MF, '[', ,,,n,, , drop=F)
    #pr-specific check for now; only do if present
    if (!is.null(missval.args$pr)){
      missval.args$pr$MF <- MF_mask[,,,n,, ,drop=F]
    }
    missval.out <- remove.FUDGE.missvals(OH, MH, MF.slice, 
                                         missval.args$minsamplesize, 
                                         missval.args, 
                                         missval.method='distributional')
    if(missval.out$ds.possible){
      OH.slice <- missval.out$OH[[1]]
      MH.slice <- missval.out$MH[[1]]
      MF.slice <- missval.out$MF[[1]]
      train.stat <- missval.out$missval.status
      out <- switch(type, 
                    'BCQM' = callBCQMv2(OH.slice, MH.slice, MF.slice, args), 
                    'CFQM' = callCFQMv3(OH.slice, MH.slice, MF.slice, args), 
                    'EDQM' = callEDQMv2(OH.slice, MH.slice, MF.slice, args))
      out.data[,,,n,,] <- index.qm(index.mask, out, olap)
      if(missval.out$mv.retry && train_qcmask){
        #Double-check on pr settings for this - are there different settings
        #for retry and insufficient points?
        out.data[,,,n,,] <- NA 
      }
    }else{
      out.data[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      if(train_qcmask){
        qc[,,,n,,] <- NA 
      }
    }
  }
  return(list(ds=out.data, qc=qc))
}

callEDQMv2<-function(LH,CH,CF,args){ 
  #'Performs an equidistant correction adjustment on a single variable
  #'over the historical period, in order to generate downscaled data.
  #'@param LH: Local Historical (a.k.a. observations)
    #'@param CH: Coarse Historical (a.k.a. GCM historical)
    #'@param CF: Coarse Future (a.k.a GCM future)
    #'Contains no other arguments in the args parameter.
    #'@citation Li et. al. 2010
    
    #Unlist data from its input form
    #CH <- unlist(CH)
    CH <- CH[!is.na(CH)]
    LH <- LH[!is.na(LH)]
    CF.idx <- !is.na(CF)
    CF <- CF[!is.na(CF)]
    
    lengthCH<-length(CH)
    lengthLH<-length(LH)
    lengthCF<-length(CF)
    cf.present <- sum(!is.na(CF))
    
    #if (lengthCF>lengthCH) maxdim=lengthCF else maxdim=lengthCH
    maxdim=max(c(lengthCH, lengthCF, lengthLH))
    
    # first define vector with probabilities [0,1]
    prob<-seq(0.001,0.999,length.out=cf.present)
    
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
    if (maxdim-cf.present > 0){
      temp.CFsorted$ecdfCFqCF<- c(ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE)), rep(NA, (maxdim-cf.present)))
    }else{
      temp.CFsorted$ecdfCFqCF<-ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE))
    }
    
    temp.CFsorted$qLHecdfCFqCF[1:cf.present]<-quantile(temp$LH,ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE)),na.rm =TRUE)
    temp.CFsorted$qCHecdfCFqCF[1:cf.present]<-quantile(temp$CH,ecdf(temp$CF)(quantile(temp$CF,prob,na.rm =TRUE)),na.rm =TRUE)
    # EQUIDISTANT CDF (Li et al. 2010)
    temp.CFsorted$EquiDistant<-temp.CFsorted$CF+ temp.CFsorted$qLHecdfCFqCF-temp.CFsorted$qCHecdfCFqCF
    temp<-temp.CFsorted[order(temp.CFsorted$index),]
    #      out.list[[out.var.name]] <- temp$EquiDistant[!is.na(temp$EquiDistant)]
    #    }
    #    return(out.list)
    #    #return(temp$EquiDistant[!is.na(temp$EquiDistant)])
    ds.out <- rep(NA, length(CF.idx))
    ds.out[CF.idx] <- temp$EquiDistant[1:lengthCF]
    return(ds.out)
}

callCFQMv3<-function(LH,CH,CF, args){
  #'Performs an change-factor correction adjustment on a single variable
  #'over the historical period, in order to generate downscaled data.
  #' @param LH: Local Historical (a.k.a. observations)
     #' @param CH: Coarse Historical (a.k.a. GCM historical)
     #' @param CF: Coarse Future (a.k.a GCM future)
     #' Contains no other arguments in the args parameter.
     #'2-27-2015 edit: added an argument in args, sort, for determining
     #'whether data is sorted by the CH ('historical'), CF ('future'), 
     #'or LH ('target') vectors.
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
     #Unlist data from its input form
     LH <- LH[!is.na(LH)]
     CH <- CH[!is.na(CH)]
     MF.idx <- !is.na(CF)
     out <- rep(NA, length(MF.idx))
     CF <- CF[MF.idx]
     
     lengthCH<-length(CH)
     lengthLH<-length(LH)
     lengthCF<-length(CF)
     cf.present <- sum(!is.na(CF))
     if(is.null(args$len.out)){
       message(paste("Warning in callCFQMv3: len.out not specified; will be deduced from", 
                     'length of CF data'))
       len.out <- cf.present
     }else{
       len.out <- args$len.out
     }
     #if (lengthCF>lengthCH) maxdim=lengthCF else maxdim=lengthCH
     #if (lengthCF<lengthCH) mindim=lengthCF else mindim=lengthCH
     maxdim=max(c(lengthCH, lengthCF, lengthLH))
     mindim=min(c(lengthCH, lengthCF, lengthLH))
     
     #lengthCFna<-length(which(!is.na(CF))) # count number of obs without NA
     #lengthCHna<-length(which(!is.na(CH)))
     
     #if (lengthCFna<lengthCHna) mindimNA=lengthCFna else mindimNA=lengthCHna
     mindim=min(c(sum(!is.na(CF)), sum(!is.na(CH)), sum(!is.na(LH))))
     
     # first define vector with probabilities [0,1]
     prob<-seq(0.001,0.999,length.out=len.out) #lengthCF
     
     # initialize data.frame
     temp<-data.frame(index=seq(1,maxdim),CF=rep(NA,maxdim),CH=rep(NA,maxdim),
                      LH=rep(NA,maxdim),qLH=rep(NA,maxdim),ecdfCHqLH=rep(NA,maxdim),qCFecdfCHqLH=rep(NA,maxdim))
     SDF<-data.frame(index=seq(1,maxdim),CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),CFQM=rep(NA,maxdim))
     
     SDF$CF[1:lengthCF]<-temp$CF[1:lengthCF]<-CF
     SDF$CH[1:lengthCH]<-temp$CH[1:lengthCH]<-CH
     SDF$LH[1:lengthLH]<-temp$LH[1:lengthLH]<-LH
     if (ORDERING=="CF"){
       # CHANGE FACTOR ORDERED BY COARSE FUTURE
       temp.CFsorted2<-temp[order(temp$CF),]
       print(summary(quantile(temp.CFsorted2$LH,prob,na.rm =TRUE)))
       temp.CFsorted2$qLH[1:len.out]<-quantile(temp.CFsorted2$LH,prob,na.rm =TRUE) #Output of length lengthCF
       temp.CFsorted2$ecdfCHqLH[1:len.out]<-ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)) #Output of length lengthCF
       temp.CFsorted2$qCFecdfCHqLH[1:len.out]<-quantile(temp$CF,ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)),na.rm =TRUE) #Output of length lengthCF
       #print(summary(temp.CFsorted2$index))
       #print(summary(temp.CFsorted2[order(temp.CFsorted2$index),]))
       temp.CFQM2<-temp.CFsorted2[order(temp.CFsorted2$index),][1:lengthCF,] 
       #By this point, it has successfully ordered the first lengthCF members of the output data
     }
     if (ORDERING=="LH"){
       # CHANGE FACTOR ORDERED BY LOCAL HISTORICAL
       temp.LHsorted2<-temp[order(temp$LH),] 
       temp.LHsorted2$qLH[1:len.out]<-quantile(temp.LHsorted2$LH,prob,na.rm =TRUE)
       temp.LHsorted2$ecdfCHqLH[1:len.out]<-ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE))
       temp.LHsorted2$qCFecdfCHqLH[1:len.out]<-quantile(temp$CF,ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)),na.rm =TRUE)
       temp.CFQM2<-temp.LHsorted2[order(temp.LHsorted2$index),][1:lengthLH,]
     }
     if (ORDERING=="CH"){
       # CHANGE FACTOR ORDERED BY COARSE HISTORICAL
       temp.CHsorted2<-temp[order(temp$CH),] 
       temp.CHsorted2$qLH[1:len.out]<-quantile(temp.CHsorted2$LH,prob,na.rm =TRUE)
       temp.CHsorted2$ecdfCHqLH[1:len.out]<-ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE))
       temp.CHsorted2$qCFecdfCHqLH[1:len.out]<-quantile(temp$CF,ecdf(temp$CH)(quantile(temp$LH,prob,na.rm =TRUE)),na.rm =TRUE)
       temp.CFQM2<-temp.CHsorted2[order(temp.CHsorted2$index),][1:lengthCH,]
     }
     print(summary(temp.CFQM2$qCFecdfCHqLH))
     out[MF.idx] <- temp.CFQM2$qCFecdfCHqLH[1:len.out]
     return(out)
}

callBCQMv2<-function(LH,CH,CF,args){
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
     #     CH <- unlist(CH)
     lengthCH<-length(CH)
     lengthLH<-length(LH)
     lengthCF<-length(CF)
     
     # if (lengthCF>lengthCH) maxdim=lengthCF else maxdim=lengthCH
     maxdim=max(c(lengthCH, lengthCF, lengthLH))
     
     # first define vector with probabilities [0,1]
     #previously, prob was lengthCF
     cf.present <- sum(!is.na(CF))
     print(paste('num cf present:', cf.present))
     prob<-seq(0.001,0.999,length.out=cf.present)
     
     # initialize data.frame
     #index used ot be 1:lengthCF; doing 1:maxdim instead
     #c(seq(1,lengthCF), rep(NA,maxdim-lengthCF))
     temp<-data.frame(index=1:maxdim,CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),
                      qCF=rep(NA,maxdim),ecdfCHqCF=rep(NA,maxdim),qLHecdfCHqCF=rep(NA,maxdim))
     temp$CF[1:lengthCF]<-CF
     temp$CH[1:lengthCH]<-CH
     temp$LH[1:lengthLH]<-LH #THIS is the line that's throwing things off
     temp.CFsorted<-temp[order(temp$CF),]
     temp.CFsorted$qCF[1:cf.present]<-quantile(temp.CFsorted$CF,prob,na.rm =TRUE)
     temp.CFsorted$ecdfCHqCF[1:cf.present]<-ecdf(temp$CH)(quantile(temp$CF,prob,na.rm =TRUE))
     temp.CFsorted$qLHecdfCHqCF[1:cf.present]<-quantile(temp$LH,ecdf(temp$CH)(quantile(temp$CF,prob,na.rm =T)),
                                                        na.rm =T)
     temp<-temp.CFsorted[order(temp.CFsorted$index),]
     return(temp$qLHecdfCHqCF[1:lengthCF])
}