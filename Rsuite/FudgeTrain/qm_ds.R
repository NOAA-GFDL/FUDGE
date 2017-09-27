#qm_ds.R
#Contains a wrapper/setup method (callQM)
#and several quantile mapping methods
#(BCQM, CFQM, EDQM)
#CFQMv2 is kept for reference purposes - it's been repalced by v3

callQM <- function(OH, MH, MF, args, type="",
                   missval.args, tmasks, train_qcmask=F){
  #'Wrapper for the single-var, single-point
  #'quantile mapping downscaling methods,
  #'which share a lot of DNA with each other.
  #'For now, even methods that can do the training once
  #'all do training within looping by ensemble.
  #'

  #Note: pr adjustments now effectively require this to be called before
  #the retry options can work properly.

  #   OH.data <- compress.xy.data(OH)
  #   MH.data <- compress.xy.data(MH)
  #   MF.data <- compress.xy.data(MF)

  #Prepare output and indexing
  out.data <- array(NA, dim=dim(MF[[1]]))
  nrips <- dim(out.data)[4]
  #   #Prepare indexing mask
  #   if (tmasks$time_mask_overlap){
  #     index.mask <- tmasks$trim$masks[[w]]
  #     olap=T
  #   }else{
  #     index.mask <- tmasks$MF$masks[[w]]
  #     olap=F
  #   }
  #Trying a workaround for the quantile mapping workflow issue -
  #The new indexing works on non-overlap cases, but the s5 examples really
  #need the overlapped data in order to reproduce past results.
  #Deleting the more complicated indexing in here and moving that indexing step to just
  #before the addition step *should* solve that particular issue.
  index.mask <- tmasks$MF$masks[[w]]
  olap <- F
  if(train_qcmask){
    if(tmasks$apply_tmasks){
      qc <- mapply(rep, index.mask, nrips)
    }else{
      qc <- rep(1, dim(MF[[1]]))
    }
    dim(qc) <- dim(out.data)
  }else{
    qc <- NULL
  }
  #  MF_mask <- missval.args$pr$MF
  start_year <- 1
  for (n in 1:nrips){
    MF.slice <- lapply(MF, '[', ,,,n,, , drop=F)
    missval.new <- missval.args
    missval.new$pr_opts$MF <- missval.new$pr_opts$MF[,,,n,, , drop=F]
    missval.out <- remove.FUDGE.missvals(OH, MH, MF.slice,
                                         missval.args$minsamplesize,
                                         missval.new,
                                         missval.method='distributional')
    if(missval.out$ds.possible){
      OH.slice <- missval.out$OH[[1]]
      MH.slice <- missval.out$MH[[1]]
      MF.slice <- missval.out$MF[[1]]
      mf.missing <- sum(is.na(MF.slice))

      args$is.uniform.Oh <- length(unique(OH.slice[!is.na(OH.slice)])) <= 1
      args$is.uniform.Mh <- length(unique(MH.slice[!is.na(MH.slice)])) <= 1
      args$is.uniform.Mf <- length(unique(MF.slice[!is.na(MF.slice)])) <= 1

      train.stat <- missval.out$missval.status
      out <- switch(type,
                    'BCQM' = callBCQMv4(OH.slice, MH.slice, MF.slice, args),
                    #'CFQM_orig' = callCFQMv3(OH.slice, MH.slice, MF.slice, args),
                    'CFQM_hist' = CFQM_hist(OH.slice, MH.slice, MF.slice, args, tmasks, start_year),
                    'CFQM_fut' =  CFQM_fut(OH.slice, MH.slice, MF.slice, args),
                    'EDQM' = callEDQMv2(OH.slice, MH.slice, MF.slice, args),
                    'KDDM' = callKDDM(OH.slice, MH.slice, MF.slice, args)
      )
      # print(length(out))
      out.data[,,,n,,] <- index.qm(index.mask, out, olap)
      if(missval.out$mv.retry & train_qcmask){
        #Double-check on pr settings for this - are there different settings
        #for retry and insufficient points?
        qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }else{
      print('downscaling not possible at this point')
      out.data[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      if(train_qcmask){
        qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }
  }
  #stop('examine idx returned')
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
    cf.present <- sum(!is.na(CF))
    CF <- CF[!is.na(CF)]

    lengthCH<-length(CH)
    lengthLH<-length(LH)
    lengthCF<-length(CF)


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

CFQM_futa <- function(OH, MH, MF, args){
  prob <- seq(0.001,0.999,length.out=sum(!is.na(MF))) #lengthCF
  MF.rank <- rank(MF, ties='first')
  #Create OH mapped to MH
  OH.mapped.to.MH <- ecdf(MH)(quantile(OH, prob, na.rm=T))
  #Map MF
  ds <- quantile(MF, OH.mapped.to.MH, na.rm=T)
  return(ds[MF.rank])
}

CFQM_hista <- function(OH, MH, MF, args, tmasks, start_year){

  oh_longer <- ifelse(length(OH) > length(MF), T, F)

  #Prepare indexing mask
  if (tmasks$time_mask_overlap){
    index.mask <- tmasks$trim$masks[[w]]
    olap=T
  }else{
    index.mask <- tmasks$MF$masks[[w]]
    olap=F
  }

  #print(str(tmasks))
  #Do the sample size adjustment as your initial step
  if (oh_longer){
    #Repeat current MF to extend same length as OH,MH - taking leapdays into account
    print('indexing args with issues')
    print(tmasks$nyears_hist)
    print(tmasks$nyears_fut)
    print(tmasks$hist.year.div)
    print(tmasks$fut.year.div)
    print(start_year)
    #     stop('examine this out')
    idx <- get.OH.time.index(tmasks$nyears_hist, tmasks$nyears_fut,
                             tmasks$hist.year.div, tmasks$fut.year.div,
                             start_year=start_year)
    print(str(idx))
    OH.idx <- idx$index
    #start_year <<- idx$end
    MH <- MH[OH.idx]
    OH <- OH[OH.idx]
    #delta <- delta.train(MH, MF.data, deltatype, deltaop, ignore0.delta, status)
    #ds.out <- delta.downscale(delta, OH[OH.idx], deltaop, status)
  }else{
    #Otherwise, repeat MH,MF to extend to same length as current MF
    idx <- get.OH.time.index(tmasks$nyears_hist, tmasks$nyears_fut,
                             tmasks$hist.year.div, tmasks$fut.year.div,
                             start_year=1)
    OH.idx <- idx$index
    start_year <<- idx$end
    #OH.idx is the repeated number of entries that you need in order
    #
    MH <- MH[OH.idx]
    OH <- OH[OH.idx]
  }

  #Create OH mapped to MH
  prob <- seq(0.001,0.999,length.out=sum(!is.na(OH)))
  OH.mapped.to.MH <- ecdf(MH)(quantile(OH, prob, na.rm=T))
  #map MF
  ds <- quantile(MF, OH.mapped.to.MH, na.rm=T)

  #And finally, do the indexing according to the
  #delta method
  #print(str(tmasks))
  ds <- index.delta(tmasks$hist.leapmask[OH.idx],
                    tmasks$fut.leapmask,
                    tmasks$OH$masks[[w]][OH.idx],
                    index.mask,
                    ds)
  return(ds)
}

callBCQMv4 <- function(Oh, Mh, Mf, args) {
#   if (w == 1){
#     save(file='/home/cew/Code/testing/BCQMv4_pr.R',
#          list=c('Oh', 'Mh', 'Mf', 'args'))
#   }
  DSout <- Mf + NA
  Oh <- Oh[!is.na(Oh)]
  Mh <- Mh[!is.na(Mh)]
  Mf.orig <- Mf
  Mf <- Mf[!(Mf.nas <- is.na(Mf))]

  if (!args$is.uniform.Oh & !args$is.uniform.Mh) {
		n.max <- max(length(Oh), length(Mh))
		tau <- c(0, (1:n.max)/(n.max+1), 1)
		x <- quantile(Mh, tau, type=8, na.rm=TRUE)
		y <- quantile(Oh, tau, type=8, na.rm=TRUE)
		yout <- approx(x, y, xout=Mf, rule=1)$y
		extrap.lower <- is.na(yout) & (Mf <= min(Mh))
		extrap.upper <- is.na(yout) & (Mf >= max(Mh))
		yout[extrap.lower] <- min(Oh) + (Mf[extrap.lower]- min(Mh))
		yout[extrap.upper] <- max(Oh) + (Mf[extrap.upper]- max(Mh))
		DSout[!Mf.nas] <- yout
		return(DSout)
	} else {
		print(paste0(ifelse(args$is.uniform.Oh,"Oh",""), ifelse(args$is.uniform.Oh & args$is.uniform.Mh," and ",""),
                 ifelse(args$is.uniform.Mh,"Mh",""), ifelse(args$is.uniform.Oh & args$is.uniform.Mh," are"," is"),
                 " uniform. Returning Mf."))
		return(Mf.orig)
	}

}


callKDDM <- function(Oh, Mh, Mf, args){
  #'Bias correction of climate model output using KDDM on a single variable
  #'from the 'climod' package https://github.com/sethmcg/climod
  #'@param Oh: Observed Historical (a.k.a. observations, obs)
    #'@param Mh: Model Historical (a.k.a. GCM historical, cur)
    #'@param Mf: Model Future (a.k.a GCM future, fut)
    #'@param norm: Choice of normalization technique. Contained in args parameter.
    #'Contains no other arguments in the args parameter.
    #' Not valid for pr variables or julian calendar. When updated, remove check in line 351 of MasterDriver.R

    if (!(args$norm %in% c("boxcox", "zscore", "none"))) {
      stop(paste('KDDM norm must be one of {"zscore", "boxcox", "none"}'))
    }
    # Prepare Downscaled output array the same as Mf
    DSout <- Mf
    if (!args$is.uniform.Oh & !args$is.uniform.Mh) {
      # Prepare Downscaled output array of NAs the same dimension as Mf
      DSout <- DSout + NA
      if (tmasks$apply_tmasks) {
        oh.mask <- tmasks$OH$masks[[w]]
        mh.mask <- tmasks$MH$masks[[w]]
        mf.mask <- tmasks$MF$masks[[w]]
      } else {
        oh.mask <- rep(1,length(Oh))
        mh.mask <- rep(1,length(Mh))
        mf.mask <- rep(1,length(Mf))
      }
      # Subset to remove the masked NAs
      subset.OH <- as.vector(Oh)[!is.na(oh.mask)]
      subset.MH <- as.vector(Mh)[!is.na(mh.mask)]
      subset.MF <- as.vector(Mf)[!is.na(mf.mask)]
      # Find the time window width
      window.width <- sum(!is.na(mf.mask[1:365]))
      # Find the number of years in each data set
      num.years.Oh <- length(Oh)/365
      num.years.Mh <- length(Mh)/365
      num.years.Mf <- length(Mf)/365
      # Find the width of a half of the time window, to determine closeness to Dec31/Jan1 boundary.
      half.window <- ceiling(window.width/2)
      # Initialize a padding variable to shift December data across the boundary if necessary.
      pad <- 0
      # If the first or last NAs are the first or last array entry, then mask does not wrap.
      if ( (pad1 <- min(which(is.na(mf.mask))) - 1) == 0 | (pad2 <- length(mf.mask) - max(which(is.na(mf.mask)))) == 0 | window.width == 365) {
        # If either pad is zero, Dec31/Jan1 boundary is not an issue, fold vector into lists of winow.width length.
        folded.OH <- as.list(data.frame(matrix(subset.OH, byrow=FALSE, nrow=window.width)))
        folded.MH <- as.list(data.frame(matrix(subset.MH, byrow=FALSE, nrow=window.width)))
        folded.MF <- as.list(data.frame(matrix(subset.MF, byrow=FALSE, nrow=window.width)))
      } else {
        print("Adjusting for 31DEC/1JAN overlap.")
        # otherwise, the smaller side should be padded
        pad <- c(pad1, pad2)[padside <- which.min(c(pad1, pad2))]
        # If window is beginning of year...
        #       if ( w < half.window ) {
        if (padside == 2) {
          #         pad <- half.window - w
          subset.OH <- c(rep(Inf,pad), subset.OH[-c((length(subset.OH)-pad+1):length(subset.OH))])
          subset.MH <- c(rep(Inf,pad), subset.MH[-c((length(subset.MH)-pad+1):length(subset.MH))])
          subset.MF <- c(rep(Inf,pad), subset.MF[-c((length(subset.MF)-pad+1):length(subset.MF))])

          folded.OH <- as.list(data.frame(matrix(subset.OH, byrow=FALSE, nrow=window.width)))
          folded.MH <- as.list(data.frame(matrix(subset.MH, byrow=FALSE, nrow=window.width)))
          folded.MF <- as.list(data.frame(matrix(subset.MF, byrow=FALSE, nrow=window.width)))

          folded.OH[[1]] <- folded.OH[[1]][!is.infinite(folded.OH[[1]])]
          folded.MH[[1]] <- folded.MH[[1]][!is.infinite(folded.MH[[1]])]
          folded.MF[[1]] <- folded.MF[[1]][!is.infinite(folded.MF[[1]])]
        } else {
          # otherwise window is end of year
          #         pad <-  half.window - (366 - w)
          subset.OH <- c(subset.OH[-c(1:pad)], rep(Inf,pad))
          subset.MH <- c(subset.MH[-c(1:pad)], rep(Inf,pad))
          subset.MF <- c(subset.MF[-c(1:pad)], rep(Inf,pad))

          folded.OH <- as.list(data.frame(matrix(subset.OH, byrow=FALSE, nrow=window.width)))
          folded.MH <- as.list(data.frame(matrix(subset.MH, byrow=FALSE, nrow=window.width)))
          folded.MF <- as.list(data.frame(matrix(subset.MF, byrow=FALSE, nrow=window.width)))

          folded.OH[[num.years.Oh]] <- folded.OH[[num.years.Oh]][!is.infinite(folded.OH[[num.years.Oh]])]
          folded.MH[[num.years.Mh]] <- folded.MH[[num.years.Mh]][!is.infinite(folded.MH[[num.years.Mh]])]
          folded.MF[[num.years.Mf]] <- folded.MF[[num.years.Mf]][!is.infinite(folded.MF[[num.years.Mf]])]
        }
      }
      # Bias Correct using the climod KDDM routine, return the unlisted array of future values

      # NOTE: Using GFDL modfified KDDM biascorrect() function.  When NCAR updates, this can be changed back.
      kddm.bc.out <- unlist(gfdl.kddm.biascorrect(list(obs=folded.OH, cur=folded.MH, fut=folded.MF), norm=ifelse(args$norm=="none","identity",args$norm))$fut)
      # If pad was increased to shift data, shift it back
      if(pad > 0) {
        if ( w < half.window ) {
          kddm.bc.out <- c(kddm.bc.out, rep(NA,pad))
        } else {
          kddm.bc.out <- c(rep(NA,pad), kddm.bc.out)
        }
      }
      # Write the bias corrected results into the DSout array in the unmasked indices and return.
      DSout[!is.na(mf.mask)] <- kddm.bc.out
    } else {
      print(paste0(ifelse(args$is.uniform.Oh,"Oh",""), ifelse(args$is.uniform.Oh & args$is.uniform.Mh," and ",""),
                   ifelse(args$is.uniform.Mh,"Mh",""), ifelse(args$is.uniform.Oh & args$is.uniform.Mh," are"," is"),
                   " uniform. Returning Mf."))
    }
    return(DSout)
}

# NOTE: GFDL modfified KDDM biascorrect() function includes "na.rm=TRUE" fix for missing ESTCP December bug.
#       When NCAR updates, this can be deleted
gfdl.kddm.biascorrect <- function (bcdata, norm = "zscore", minobs = 10, dmap = FALSE, ...)
{
  if (sum(is.finite(unlist(bcdata$obs))) < minobs | sum(is.finite(unlist(bcdata$cur))) <
        minobs) {
    unfixable <- function(x) {
      x@uncorrectable <- is.finite(x)
      x + NA
    }
    return(rapply(bcdata, unfixable, how = "replace"))
  }
  if (norm == "boxcox") {
    gamma <- megamma(unlist(bcdata, use.names = FALSE))
    nbcd <- rapply(bcdata, normalize, how = "replace", norm = norm,
                   gamma = gamma)
  }
  else {
    nbcd <- rapply(bcdata, normalize, how = "replace", norm = norm)
  }
  mapping <- distmap(unlist(nbcd$cur), unlist(nbcd$obs), ...)
  fixed <- rapply(nbcd, function(x) {
    predict(mapping, x)
  }, how = "replace")
  adj <- rapply(fixed, attributes, how = "replace")
  adj <- lapply(adj, renest)
  adj <- lapply(adj, function(x) {
    lapply(x, function(y) {
      unlist(y)
    })
  })
  adj <- lapply(adj, function(x) {
    x$norm <- NULL
    x
  })
  ## FIX for NA/NaN here...
#   adj <- rapply(adj, mean, how = "replace")
  adj <- rapply(adj, mean, how = "replace", na.rm=TRUE)
  adj <- renest(adj)
  shift <- NA
  scale <- NA
  pscale <- NA
  if (norm == "zscore") {
    shift <- adj$mu$obs - adj$mu$cur
    scale <- adj$sigma$obs/adj$sigma$cur
  }
  if (norm == "boxcox") {
    shift <- 0
    pscale <- 1
  }
  result <- list()
  result$obs <- bcdata$obs
  result$cur <- rapply(fixed$cur, denormalize, how = "replace",
                       shift = shift, scale = scale, pscale = pscale)
  result$fut <- rapply(fixed$fut, denormalize, how = "replace",
                       shift = shift, scale = scale, pscale = pscale)
  if (dmap) {
    result$distmap <- mapping
  }
  return(result)
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
     prob<-seq(0.001,0.999,length.out=cf.present)

     # initialize data.frame
     #index used ot be 1:lengthCF; doing 1:maxdim instead
     #c(seq(1,lengthCF), rep(NA,maxdim-lengthCF))
     if(cf.present != 0){
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
     }else{
       print("No present values in CF; returning all NA")
       return(CF)
     }
}

