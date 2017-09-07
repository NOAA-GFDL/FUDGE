#CFQM_ds 
#Function of CFQM, downscaled data

#Okay, so we're going to have to do that sort on top of everythign else


callCFQM <- function(OH,MH,MF,args, train_qcmask, missval.args, 
                     tmasks=list(), s5_list=list()){
  #First shot at implementing CFQM in a delta-SD-like form
  #   OH.data <- compress.xy.data(OH)
  #   MH.data <- compress.xy.data(MH)
  #   MF.data <- compress.xy.data(MF)
  #   print(lapply(MF.data, 'summary'))
  #   print(lapply(MF.data, 'dim'))
  
  status=0
  out.data <- array(NA, dim=dim(MF[[1]]))
  nrips = dim(out.data)[4]
  #Are we returning a mask of those points where delta beahved as expected?
  if (train_qcmask){
    if(tmasks$apply_tmasks){
      #This is going to have to be revised for use in the workflow
      qc.data <- mapply(rep, tmasks$MF$masks[[w]], nrips)
    }else{
      qc.data <- rep(1, dim(MF[[1]]))
    }
    dim(qc.data) <- dim(out.data)
  }else{
    qc.data <- NULL
  }
  #Obtain sort options
  pr_start <- 1
  oh_longer <- length(MF[[1]][,,,1,,]) <= length(OH[[1]])
#   print(dim(OH[[1]]))
#   print(dim(MH[[1]]))
#   print(dim(MF[[1]]))
  for (n in 1:nrips){
#     print(paste("on ens", n))
#     print(pr_start)
    #Determine data to use when indexing against OH
    out.status <- 0
    MF.slice <- lapply(MF, '[', ,,,n,, , drop=F)
    #Determine if there is sufficient data to downscale at this point
    #print(str(tmasks))
    #stop('examine this out')
    if ('pr_opts' %in% names(missval.args)){
      tmp.missval.args <- missval.args
      tmp.missval.args$pr_opts$MF <- missval.args$pr_opts$MF[,,,n,, , drop=F]
    }else{
      tmp.missval.args <- missval.args
    }
    missval.out <- remove.FUDGE.missvals(OH, MH, MF.slice, 
                                         tmp.missval.args$minsamplesize, 
                                         tmp.missval.args, 
                                         missval.method='distributional')
    if(missval.out$ds.possible){
      OH.slice <- missval.out$OH[[1]]
      MH.slice <- missval.out$MH[[1]]
      MF.slice <- missval.out$MF[[1]]
      out.idx <- !is.na(OH.slice)
      if (oh_longer){
        #Speciciations for this were created in main driver script when delta ds is used
        idx <- get.OH.time.index(tmasks$nyears_hist, tmasks$nyears_fut, 
                                 tmasks$hist.year.div, tmasks$fut.year.div, 
                                 start_year=pr_start)
        OH.idx <- idx$index
        pr_start <- idx$end
        #ds is now indexed by a subset of the OH timeseries; if using non-noleap
        #calendars in any dataset, need to adjust the two for concurrent leapdays
        #note:leapday masks are already in global environment as part of setup
      }else{
        #Otherwise, if the vectors are uneven then calculate n+1 deltas, 
        #where n=length(MF.data)/length(OH)
        idx <- get.OH.time.index(tmasks$nyears_hist, tmasks$nyears_fut, 
                                 tmasks$hist.year.div, tmasks$fut.year.div, 
                                 start_year=1)
        OH.idx <- idx$index
        pr_start <- idx$end
      }
      ds.idx <- !is.na(OH.slice[OH.idx])
      ds.out <- rep(NA, length(ds.idx))
      tmp.out <- CFQM_hist(remove.missvals(MH.slice), remove.missvals(MF.slice), 
                           remove.missvals(OH.slice[OH.idx])) 
      ds.out[ds.idx] <- tmp.out
      indexed.out <- ds.out
      #out.data[,,,n,,] <- indexed.out
      out.data[,,,n,,] <- index.delta(tmasks$hist.leapmask[OH.idx], 
                                      tmasks$fut.leapmask, 
                                      tmasks$OH$masks[[w]][OH.idx],
                                      tmasks$MF$masks[[w]],
                                      indexed.out)
      if (train_qcmask){
        #the below used to store the status return; for now, to keep 
        #consistency with the other, more commonly-used training mask
        #methods, we're replacing that with NA
        qc.data[,,,n,,] <- 1 
      }
    }else{
      print("insufficient data to downscale at this point")
      print(dim(out.data))
      print(dim(qc.data))
      out.data[,,,n,,] <- NA
      if (train_qcmask){
        qc.data[,,,n,,] <- NA
      }
    }
  }
  return(list(ds=out.data, qc=qc.data))
}


CFQM_hist <- function(MH, MF, OH){
  #Not worrying about sort in this function - we need it OH-ordered to get the
  #leapdays in the right order anyway
  prob=seq(from=0.001, to=0.999, length.out=length(OH))
  OH.mapped.to.MH <- ecdf(MH)(quantile(OH, prob, na.rm=T))
  DS <- quantile(MF, OH.mapped.to.MH, na.rm=T)
  return(DS[rank(OH, ties='first')])
}

set.CFQM.defaults <- function(args, predictor.vars, ..., env=parent.frame()){
  return.args <- args[! (names(args) %in% c('sort'))]
  apply.s5 <- F
  if ('wxseq' %in% names(args)){
    if (args$wxseq=="MFseq"){
      apply.s5 <- T
      post.ds$mapMF <- list(type="mapMFOrder", 
                                 adjust.out="on",
                                 qc.mask="off", 
                                 loc="outloop", 
                                 qc_args=list())
    }else if (args$wxseq != "OHseq"){
      stop(paste("Error in set.CFQM.defaults: wxseq should be one of", 
                 "OHseq, MFseq"))
    }
  }else{
    if(args$sort=="future"){
      return.args <- c(args)
      post.ds$mapMF <-  list(type="mapMFOrder", 
                                 adjust.out="on",
                                 qc.mask="off", 
                                 loc="outloop", 
                                 qc_args=list())
    }else{
      return.args <- c(return.args, list(wxseq="OHseq"))
    }
  }
  env$s5.outloop <- apply.s5
  env$ds.args <- return.args
  env$post.ds <- post.ds
  return(env)
}

