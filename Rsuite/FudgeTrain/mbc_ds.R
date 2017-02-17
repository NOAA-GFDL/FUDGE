

callMBC <- function(OH, MH, MF, args, ds.method="",
                   tmasks, train_qcmask=F){
  #'02-14-17 - new asusmption is that rat.seq is of same length as predictor vars
  OH <- compress.xy.data(OH)
  MH <- compress.xy.data(MH)
  MF <- compress.xy.data(MF)
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
  #Get/override default arguments for MBC
  #As of 02-14-71, this is done in the main instead
  #args <- set.MBC.defaults(MH, args)
  for (n in 1:nrips){
    MF.slice <- lapply(MF, '[', ,,,n,, , drop=F)
    #pr-specific check for now; only do if present
    if (!is.null(missval.args$pr)){
      missval.args$pr$MF <- MF_mask[,,,n,, ,drop=F]
    }
    missval.out <- remove.FUDGE.missvals(OH, MH, MF.slice, 
                                         missval.args$minsamplesize, 
                                         missval.args, 
                                         missval.method='synchronous')
    if(missval.out$ds.possible){
      #There are different pre-conditioning steps that need
      #to take place for each method
      #But one common conditioning step should be adjusting the length of ratio.seq
      loop.args <- args
      print(loop.args)
      message("Adjusting ratio.seq to use ")
      loop.args$ratio.seq <- map.var.to.xy.properties(names(missval.out$MF), 
                                                 args$var.names, args$ratio.seq)
      print(loop.args$ratio.seq)
      print(loop.args$var.names)
      out <- switch(ds.method, 
                    'QDM' = callQDM(missval.out$OH, missval.out$MH, missval.out$MF, args),  
                    'MBC' = callMBC_any(missval.out$OH, missval.out$MH, missval.out$MF, 
                                    args$variant, 
                                    missval.out$complete.hist, 
                                    missval.out$complete.fut, loop.args))
      train.stat <- missval.out$missval.status
      #... the indexing on this function is going to be a pain.
      #I sort of think that we have to do synchronous missing values
      #and that the synch missing values also need some sort of master 
      #OH and MH vector for missing points.
      #We also CAN'T use the pr masking functions with this method, I think
      out.data[,,,n,,][missval.out$complete.fut] <- out
      if(missval.out$mv.retry & train_qcmask){
        out.data[,,,n,,][missval.out$complete.fut] <- rep(NA, length(out$mhat.p))
      }
    }else{
      print('downscaling not possible at this point')
      out.data[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      if(train_qcmask){
        qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }
  }
  return(list(ds=out.data, qc=qc))
}

callQDM <- function(OH, MH, MF, args){
  OH.slice <- remove.missvals(unlist(as.vector(OH)))
  MH.slice <- remove.missvals(unlist(as.vector(MH)))
  MF.slice <- remove.missvals(unlist(as.vector(MF)))
  tmp <- QDM(OH.slice, MH.slice, MF.slice, 
             ratio=args$ratio.seq, trace=args$trace, 
             jitter.factor=args$jitter.factor, ties=args$ties)
  return(tmp$mhat.p)
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
  print(summary(tmp$mhat.p))
  return(tmp$mhat.p[,1])
}

set.MBC.defaults <- function(args, predictor.vars){
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
  jitter.names <- c("jitter", 'jitter.fac', 'jitter_fac')
  if (any(jitter.names %in% names(args))){
    jitter.name <- jitter.names[which(jitter.names %in% names(args))] 
    jitter.fac <- args[[jitter.name]]
  }
  return(list(iter=iter, jitter.factor=jitter.fac, ratio.seq=ratio.seq, trace=trace, ties=ties, 
              variant=args$variant, var.names=unlist(predictor.vars)))
}