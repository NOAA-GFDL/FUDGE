#qm_ds.R
#Contains a wrapper/setup method (callQM) 
#and several quantile mapping methods
#(BCQM, CFQM, EDQM)
#CFQMv2 is kept for reference purposes - it's been repalced by v3

callQM <- function(OH, MH, MF, args, type="", tmasks=list()){
  out.data <- array(NA, dim=dim(MF[[1]]))
  nrips <- dim(out.data)[4]
  #Prepare indexing mask
  print(summary(tmasks))
  if (tmasks$time_mask_overlap){
    index.mask <- tmasks$trim$masks[[w]]
    olap=T
  }else{
    index.mask <- tmasks$MF$masks[[w]]
    olap=F
  }
  for (n in 1:nrips){
    MF.slice <- MF[[1]][,,,n,,]
    out <- switch(type, 
                  'BCQM' = callBCQMv2(OH[[1]], MH[[1]], MF.slice, args), 
                  'CFQM' = callCFQMv3(OH[[1]], MH[[1]], MF.slice, args), 
                  'EDQM' = callEDQMv2(OH[[1]], MH[[1]], MF.slice, args))
  out.data[,,,n,,] <- index.qm(index.mask, out, olap)
  }
  return(list(ds=out.data, qc=NULL))
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
  return(temp$EquiDistant[1:lengthCF])
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
     #print(summary(temp.opt.sorted))
     print(lengthCF)
     SDF<-temp$qCFecdfCHqLH[1:lengthCF] #This is the version that is getting tested as of 5-18
     #save(file="~/Code/testing/test_out.R", list=c('SDF'))
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