#'Wrapper function for the FUDGE calls to MBC and QQDC,
#'since both use the same package and there is similar
#'argument checking between the two of them

callQQDC <- function(OH, MH, MF, args, ds.method="",
                    tmasks, train_qcmask=FALSE){
  if (args$force_reproducibility){
    #i_idx, j_idx, i_offset and j_offset are passed in from the command-line.
    #This is meant to enforce reprodcability when we have alternative
    #subsetting in place.
    seedval <- ( 31 * (ival + i_idx) ) + ( 29 * (jval + j_idx) )
    #print(paste("seedval:", seedval))
    set.seed(seedval)
  }
  #Create output data
  out.data <- array(NA, dim=dim(OH[[1]]))
  nrips <- dim(out.data)[4]
  index.mask <- tmasks$OH$masks[[w]]
  olap <- FALSE
  if(train_qcmask){
    if(tmasks$apply_tmasks){
      qc <- mapply(rep, index.mask, nrips)
    }else{
      qc <- rep(1, dim(OH[[1]]))
    }
    dim(qc) <- dim(out.data)
  }else{
    qc <- NULL
  }
  MF_mask <- missval.args$pr_opts$MF
  tmp.missvals <- missval.args
  for (n in 1:nrips){
    nval <<- n
    MF.slice <- lapply(MF, '[', ,,,n,, , drop=F)
    #pr-specific check for now; only do if present
    missval.args$pr_opts$MF <- MF_mask[,,,n,, ,drop=F]
    missval.out <- remove.FUDGE.missvals(OH, MH, MF.slice,
                                         missval.args$minsamplesize,
                                         missval.args,
                                         missval.method='distributional')

    if (args$adjust_trace){
      adj.thold <- missval.args$pr_opts$adjusted.thold
      if (args$transform){
        if (is.null(args$tform_fxn[[2]])){
          adj.thold <- do.call(args$tform_fxn[[1]], list(adj.thold))
        }else{
          adj.thold <- do.call(args$tform_fxn[[1]], list(adj.thold, args$tform_fxn[[2]]))
        }
      }
      if (args$trace > adj.thold){
        args$trace <- adj.thold
      }
    }
    if(missval.out$ds.possible){
      #There are different pre-conditioning steps that need
      #to take place for each method
      ds_arglist = list(missval.out$OH, missval.out$MH, missval.out$MF, args)
      out <- tryCatch({do.call("callQQDC_internal", args = ds_arglist)},
                      error = function(e){
                        is_known <- grepl("need at least two non-NA values to interpolate",
                                          e$message)
                        if (is_known){
                          message(paste("Warning in callQQDC: QQDC did not have",
                                        "enough unique values in input datasets",
                                        "to interpolate!"))
                          message(paste("Returning MF unchanged."))
                          return(MF.slice[[1]])
                        }else{
                          print(e)
                          #print(traceback())
                          stop('Unknown error in QQDC: Please examine outputs!')
                        }
                        stop('examine error output')
                      })

      train.stat <- missval.out$missval.status
      loop.mask <- index.mask
      loop.mask[is.na(MF.slice[[1]])] <- NA
			if (missval.out$mv.retry){
			  out <- apply.fudge.tmask(data=out, mask=loop.mask)
			}
      out.data[,,,n,,] <- index.qm(index.mask, out, olap)
      if (train_qcmask &  missval.out$mv.retry){
        print("adding the pr retry mask")
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
  return(list(ds=out.data, qc=qc))
}

callQQDC_internal <- function(OH, MH, MF, args){
  OH.idx <- !is.na(unlist(as.vector(OH)))
  OH.slice <- remove.missvals(unlist(as.vector(OH), use.names=F))
  MH.slice <- remove.missvals(unlist(as.vector(MH), use.names=F))
  MF.slice <- remove.missvals(unlist(as.vector(MF), use.names=F))
  args$ratio <- args$ratio.seq
  screen_NaNs <- args$screen_NaNs
  not.args <- c('adjust.trace', 'transform', 'tform_fxn',
                'force_reproducibility', 'var.names',
                'ratio.seq', 'iter', 'adjust_trace', 'screen_NaNs')
  args <- args[!(names(args) %in% not.args)]
  q.args <- c(list(o.c=MF.slice, m.c=MH.slice, m.p=OH.slice),
              args)
  #Feb 2023 - suppressing *all* warnings; once per tmask is too many
  tmp <- suppressWarnings(MBC::QDM(o.c=MF.slice, m.c=MH.slice, m.p=OH.slice)$mhat.p)
  if (screen_NaNs){ # Screen out NaNs from 0/0
    tmp[is.nan(tmp)] <- 0
  }
  out <- rep(NA, length(OH.idx))
  out[OH.idx] <- tmp
  if ('trace.calc' %in% names(args)){
    #print('QDM_mult_noise invoked; replacing DS[OH == 0] <- 0')
    out[unlist(as.vector(OH), use.names=FALSE)==0] <- 0
  }
  return(out)
}

set.QQDC.defaults <- function(args, predictor.vars, ..., env=parent.frame()){
  #'Sets common defaults for the inputs to MBC that we want
  #'to control. Most of these also apply to QQDC.
  iter = 30
  jitter.fac = 0
  ratio.seq = F #should be 1 for this case
  trace = 0.05
  adjust_trace = F
  transform = F
  tform_fxn = 'none'
  ties = 'first'
  force_reproducibility=FALSE
  screen_NaNs = FALSE
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
        tform_fxn <- return.t.fxn(pre.ds.train[[tform.idx]]$qc_args$transform,
                              pre.ds.train[[tform.idx]]$qc_args$arg)
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
  if ('force_reproducibility' %in% names(args)) {
    force_reproducibility <- ifelse(args$force_reproducibility == 'on', TRUE, FALSE)
  }else{
    if (args$type == 'mult'){
      stop(paste("Error in set.QQDC.defaults: as of fudgetag icing-testing-20180510",
                 "force_reproducibility argument ('on' or 'off')",
                 "is required to be set in the XML for MBC and QQDC when type == 'mult'!"))
    }else{
      force_reproducibility <- FALSE
    }
  }
  #Put together QQDC-specific args - but keep trace_calc default of 0.5 * trace
  return.args <- list(iter=iter, jitter.factor=jitter.fac, ratio.seq=ratio.seq, trace=trace, ties=ties,
                      var.names=unlist(predictor.vars),
                      adjust_trace=adjust_trace, transform=transform, tform_fxn = tform_fxn,
                      force_reproducibility = force_reproducibility)
  if ('QQDC_mult_noise' %in% names(args)){
    if (args$QQDC_mult_noise == 'off'){
      return.args$trace.calc <- -99 # setting to 0 can still hit eps. compares
      if (ratio.seq){
        screen_NaNs = TRUE
      }
    }
  }
  return.args$screen_NaNs = screen_NaNs
  #Deal with sample size
  if ('minsamplesize' %in% names(args)){
    return.args$minsamplesize <- args$minsamplesize
  }
  env$ds.ref$args <- return.args
  return(env)
}
