#CFQM_ds 
#Function of CFQM, downscaled data

#Okay, so we're going to have to do that sort on top of everythign else


callCFQM <- function(OH,MH,MF,args, train_qcmask, missval.args, 
                     tmasks=list(), s5_list=list()){
  status=0
  MF.dim <- dim(MF[[1]])
  OH.dim <- dim(OH[[1]])
  out.data <- array(NA, dim=c(MF.dim[1:5], OH.dim[6]))
  nrips = dim(out.data)[4]
  #Are we returning a mask of those points where delta beahved as expected?
  if (train_qcmask){
    if(tmasks$apply_tmasks){
      #This is going to have to be revised for use in the workflow
      qc.data <- mapply(rep, tmasks$OH$masks[[w]], nrips)
    }else{
      if (!is.null(dim(OH[[1]]))){
       qc.data <- rep(1, length(OH[[1]])) 
      }else{
      qc.data <- rep(1, dim(OH[[1]]))
      }
    }
    dim(qc.data) <- dim(out.data)
  }else{
    qc.data <- NULL
  }
  #Obtain sort options
  pr_start <- 1
  oh_longer <- length(MF[[1]][,,,1,,]) <= length(OH[[1]])
  for (n in 1:nrips){
    #Determine data to use when indexing against OH
    out.status <- 0
    MF.slice <- list(MF[[1]][,,,n,, ,drop=F])
    names(MF.slice) <- names(MF)
    #Determine if there is sufficient data to downscale at this point
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
      ds.idx <- !is.na(OH.slice)
      ds.out <- rep(NA, length(ds.idx))
      
      oh <- OH.slice[!is.na(OH.slice)]
      mh <- MH.slice[!is.na(MH.slice)]
      mf <- MF.slice[!is.na(MF.slice)]
      
      args$is.uniform.Oh <- length(unique(oh)) <= 1
      args$is.uniform.Mh <- length(unique(mf)) <= 1
      args$is.uniform.Mf <- length(unique(mf)) <= 1
      
      tmp.out <- call_qm(oh, mh, mf, args, method='cfqm') 
      ds.out[ds.idx] <- tmp.out
      out.data[,,,n,,] <- ds.out
      if (train_qcmask){
        #train_qcmask tracks whether any of the Oh data we work with 
        #fell outside the bounds of the MH data. We expect this to be 
        #a thing in the mountains and on the coasts.
        qc.out <- rep(NA, length(ds.idx))
        qc.out[oh > max(mh)] <- 2 #previously oh.repeat
        qc.out[oh < min(mh)] <- 1
        qc.data[,,,n,,] <- qc.out
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
  env$ds.ref$args <- return.args
  env$post.ds <- post.ds
  return(env)
}

