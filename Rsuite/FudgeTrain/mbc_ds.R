

callMBC <- function(OH, MH, MF, args, ds.method="",
                    tmasks, train_qcmask=F){
  library(MBC)
  OH <- compress.xy.data(OH, remove.all.identical=F)
  MH <- compress.xy.data(MH, remove.all.missing=F)
  MF <- compress.xy.data(MF, remove.all.missing=F)

  #Create output data
  out.data <- array(NA, dim=dim(MF[[1]]))
  nrips <- dim(out.data)[4]
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
    if (all(is.na(missval.out$MH[[1]]))){
      stop(paste("Error in QDM: adding args back in didn't fix anytning"))
    }
    
    if (args$adjust_trace){
      print("adjusting pr trace")
      #print(str(missval.args$pr))
      adj.thold <- missval.args$pr$adjusted.thold
      #print(adj.thold)
      #print(args$trace)
      if (args$transform){
        if (is.null(args$tform_fxn[[2]])){
          adj.thold <- do.call(args$tform_fxn[[1]], list(adj.thold))
        }else{
          adj.thold <- do.call(args$tform_fxn[[1]], list(adj.thold, args$tform_fxn[[2]]))
        }
      }
      if (args$trace > adj.thold){
        print("QDM: using adjusted trace instead of trace")
        print(args$trace)
        print(adj.thold)
        args$trace <- adj.thold
      }
      #args$trace <- min(args$trace, adj.thold)
    }
    loop.ds <- ds.method
    if (missval.out$mv.retry){
      print('switching QDM and QDM_mod')
      loop.ds <- 'QDM_mod'
    }else{
      loop.ds <- ds.method
    } 
    
    if(missval.out$ds.possible){
      #There are different pre-conditioning steps that need
      #to take place for each method
      out <- switch(loop.ds, 
                    'QDM' = callQDM(missval.out$OH, missval.out$MH, missval.out$MF, args), 
                    'QDM_mod' = callQDM_mod(missval.out$OH, missval.out$MH, missval.out$MF, args),
                    'MBC' = callMBC_any(missval.out$OH, missval.out$MH, missval.out$MF, 
                                        args$variant, 
                                        missval.out$complete.hist, 
                                        missval.out$complete.fut, args))
      train.stat <- missval.out$missval.status
      #... the indexing on this function is going to be a pain.
      #I sort of think that we have to do synchronous missing values
      #and that the synch missing values also need some sort of master 
      #OH and MH vector for missing points.
      #We also CAN'T use the pr masking functions with this method, I think
      #out.data[,,,n,,][!is.na(index.mask)] <- out
      out.data[,,,n,,] <- index.qm(index.mask, out, olap)
      if (train_qcmask &  missval.out$mv.retry){
        qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }else{
      #       print('downscaling not possible at this point')
      #       stop('examine this out')
      out.data[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      if(train_qcmask){
        qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }
  }
  print("summary of all MBC/QDM outputs:")
  print(summary(out.data))
  return(list(ds=out.data, qc=qc))
}

callQDM_mod <- function(OH, MH, MF, args){
  MF.idx <- !is.na(unlist(as.vector(MF)))
  OH.slice <- remove.missvals(unlist(as.vector(OH)))
  MH.slice <- remove.missvals(unlist(as.vector(MH)))
  MF.slice <- remove.missvals(unlist(as.vector(MF)))
  if (max (OH.slice) > max(MF.slice)){
    print("callQDM: OH max greater than MF max")
  }
  tmp <- QDM_mod(OH.slice, MH.slice, MF.slice, 
                 ratio=args$ratio.seq, trace=args$trace, 
                 jitter.factor=args$jitter.factor, ties=args$ties)$mhat.p
  out <- rep(NA, length(MF.idx))
  out[MF.idx] <- tmp
  return(out)
}



callQDM <- function(OH, MH, MF, args){
  MF.idx <- !is.na(unlist(as.vector(MF)))
  OH.slice <- remove.missvals(unlist(as.vector(OH)))
  MH.slice <- remove.missvals(unlist(as.vector(MH)))
  MF.slice <- remove.missvals(unlist(as.vector(MF)))
  if (max (OH.slice) > max(MF.slice)){
    print("callQDM: OH max greater than MF max")
  }
  tmp <- QDM(OH.slice, MH.slice, MF.slice, 
             ratio=args$ratio.seq, trace=args$trace, 
             jitter.factor=args$jitter.factor, ties=args$ties)$mhat.p
  out <- rep(NA, length(MF.idx))
  out[MF.idx] <- tmp
  return(out)
}

callMBC_any <- function(OH, MH, MF, variant, complete.hist, complete.fut, args){
  #Okay...this is the step when any x.y.vars which are all missing get removed.
  #This is then going to have to be applied to the same list of vars that 
  OH.slice <- as.matrix(as.data.frame(lapply(OH, function(x,y){return(x[y])}, 
                                             complete.hist)))
  MH.slice <- as.matrix(as.data.frame(lapply(MH, function(x,y){return(x[y])}, 
                                             complete.hist)))
  MF.slice <- as.matrix(as.data.frame(lapply(MF, function(x,y){return(x[y])}, 
                                             complete.fut)))
  tmp <- switch(variant, 
                "pearson"  = MBCp(OH.slice, MH.slice, MF.slice, 
                                  iter=args$iter, jitter.factor=args$jitter, 
                                  ratio.seq=args$ratio.seq, trace=args$trace, 
                                  ties=args$ties), 
                "spearman" = MBCr(OH.slice, MH.slice, MF.slice, 
                                  iter=args$iter, jitter.factor=args$jitter, 
                                  ratio.seq=args$ratio.seq, trace=args$trace, 
                                  ties=args$ties), 
                "all"      = MBCn(OH.slice, MH.slice, MF.slice, 
                                  iter=args$iter, jitter.factor=args$jitter, 
                                  ratio.seq=args$ratio.seq, trace=args$trace, 
                                  ties=args$ties), 
                stop(paste("Error in callMBC_any: type was not one of", 
                           "pearson (MBCp), spearman (MBCr), or all (MBCn)!")))
  #Need to remove vars that aren't target var at this step
  #Oh[[1]] should still work, even for multi-point target
  #...also need to do more advanced checking of missing values. 
  #Need to add back in (from the MF) any values that we could not
  #downscale over/are NA
  return(tmp$mhat.p[,1])
}

set.MBC.defaults <- function(args, predictor.vars, ...){
  #'Sets common defaults for the inputs to MBC that we want
  #'to control. Most of these also apply to QDM.
  iter = 30
  jitter.fac = 0
  ratio.seq = rep(F, length(predictor.vars))
  #This is going to have implications for the multi-point downscaling. 
  #Need access to npts_by in order to expand this list...
  #or really just need 1; multivar-targets are going to have that effect.
  trace = 0.05
  ties = 'first'
  #Trace is going to be in the same notation as our pr units adjuster. 
  #It'll work, but we need to have an idea of which vars are getting a unit
  #conversion. 
  #...I'm pretty sure those are globals anyway; we can check.
  if ('trace' %in% names(args)){
    non.numeric.trace <- is.na(suppressWarnings(as.numeric(args$trace)))
    #if trace is non-numeric (i.e an arg we'd pass to the pr functions
    #get OH pr units from adjust.metadata (created back in main driver)
    if (non.numeric.trace){
      pr.units <- adjust.metadata$pr_oh_units
      if (length(pr.units)==0){
        stop(paste("Error in set.MBC.defaults: no pr vars found in OH!"))
      }
      pr.trace <- convert.pr.threshold(args$trace, pr.units)
      trace <- pr.trace
    }else{
      trace <- as.numeric(args$trace)
    }
    if ('transform' %in% names(args)){
      tform <- return.t.fxn(args$transform, args$transform_arg)
      if (is.null(tform[[2]])){
        trace <- do.call(tform[[1]], list(trace))
      }else{
        trace <- do.call(tform[[1]], list(trace, tform[[2]]))
      }
    }
  }
  rat.names <- c('ratio', 'ratio.seq', 'ratio_seq')
  if (any(rat.names %in% names(args))){
    rat.name <- rat.names[which(rat.names %in% names(args))]
    in.rat.seq <- unlist(strsplit(args[[rat.name]], ','))
    #...oh_by is going to blow this entire bit up hard at some point in the 
    #future. I'm not sure I can get a good check going without it, though.
    #...it's also going to blow up when we get to using time-varying amplitudes
    #in here - need to make sure that only one val gets used for those.
    ##Okay - as of 2-14-17, adding this in two steps: one for the 
    #     in.rat.seq <- sapply(in.rat.seq, function(x,r){rep(x,r)}, oh_by**2)
    #     ratio.seq <- in.rat.seq
    if (length(in.rat.seq) != length(ratio.seq)){
      stop(paste("Error in set.MBC.defaults: string", args$ratio.seq, 
                 "did not have as many elements as MH has vars!"))
    }
    ratio.seq <- in.rat.seq
  }
  if ('type' %in% names(args)){
    if (args$type=='add'){
      ratio.seq <- FALSE
    }else if (args$type=='mult'){
      ratio.seq <- TRUE
    }
  }
  jitter.names <- c("jitter", 'jitter.fac', 'jitter_fac')
  if (any(jitter.names %in% names(args))){
    jitter.name <- jitter.names[which(jitter.names %in% names(args))] 
    jitter.fac <- args[[jitter.name]]
  }
  return(list(iter=iter, jitter.factor=jitter.fac, ratio.seq=ratio.seq, trace=trace, ties=ties, 
              variant=args$variant, var.names=unlist(predictor.vars)))
}


set.QDM.defaults <- function(args, predictor.vars, ...){
  #'Sets common defaults for the inputs to MBC that we want
  #'to control. Most of these also apply to QDM.
  iter = 30
  jitter.fac = 0
  ratio.seq = F #should be 1 for this case
  trace = 0.05
  adjust_trace = F
  transform = F
  tform_fxn = 'none'
  ties = 'first'
  if ('trace' %in% names(args)){
    non.numeric.trace <- is.na(suppressWarnings(as.numeric(args$trace)))
    #if trace is non-numeric (i.e an arg we'd pass to the pr functions
    #get OH pr units from adjust.metadata (created back in main driver)
    if (non.numeric.trace){
      pr.units <- adjust.metadata$pr_oh_units
      if (length(pr.units)==0){
        stop(paste("Error in set.MBC.defaults: no pr vars found in OH!"))
      }
      pr.trace <- convert.pr.threshold(args$trace, pr.units)
      trace <- pr.trace
    }else{
      trace <- as.numeric(args$trace)
    }
    #Now check the section 5 args for the 
    if (exists('pre.ds.train')){
      pre.ds.types <- sapply(pre.ds.train, '[[', 'type')
      if ('transform' %in% pre.ds.types){
        transform <- T
        tform.idx <- which(pre.ds.types=='transform')
        tform_fxn <- return.t.fxn(pre.ds.train[[tform.idx]]$pp.args$transform, 
                              pre.ds.train[[tform.idx]]$pp.args$arg)
        if (is.null(tform_fxn[[2]])){
          trace <- do.call(tform_fxn[[1]], list(trace))
        }else{
          trace <- do.call(tform_fxn[[1]], list(trace, tform_fxn[[2]]))
        }
      }
    }
  }
  rat.names <- c('ratio', 'ratio.seq', 'ratio_seq')
  if ('type' %in% names(args)){
    if (args$type=='add'){
      ratio.seq <- FALSE
    }else if (args$type=='mult'){
      ratio.seq <- TRUE
    }
  }
  jitter.names <- c("jitter", 'jitter.fac', 'jitter_fac')
  if (any(jitter.names %in% names(args))){
    jitter.name <- jitter.names[which(jitter.names %in% names(args))] 
    jitter.fac <- args[[jitter.name]]
  }
  if ('adjust_trace' %in% names(args)){
    adjust_trace <- ifelse(args$adjust_trace=='on', T, F)
  }
  #Put together MBC-specific args  
  return.args <- list(iter=iter, jitter.factor=jitter.fac, ratio.seq=ratio.seq, trace=trace, ties=ties, 
                      var.names=unlist(predictor.vars), 
                      adjust_trace=adjust_trace, transform=transform, tform_fxn = tform_fxn)
  #Deal with sample size
  if ('minsamplesize' %in% names(args)){
    return.args$minsamplesize <- args$minsamplesize
  }  
  return(return.args)
}

