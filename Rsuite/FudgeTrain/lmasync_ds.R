#lm and lmstep calls and main downscaling functions.
#Both functions are designed to work on multivar datasets,
#and handle multiple points by decomposing into separate vars.

callLMAsync <- function(OH.data, MH.data, MF.data, args, ds.method='lm', 
                        missval.args, tmasks, train_qcmask=F){
  #'Setup operations for methods that behave a lot like lm
  #'and lmstep: output time-synchronous with the future, 
  #'traing steps are supposed to have same timelevels present/
  #'not present in OH and MH.
  #'PARM doesn't quite qualify due to its internal setup requirements; 
  #'it' smostly its own weird thing.
  #Pre-processing step to compress vectors into multivariate/multi-point form
#   OH.data <- compress.xy.data(OH)
#   MH.data <- compress.xy.data(MH)
#   MF.data <- compress.xy.data(MF)
  
  targ.var <- names(OH.data)
  nrips <- dim(MF.data[[1]])[4]
  out.data <- array(NA, dim=dim(MF.data[[1]]))
  #These methods are asynchronous - they can take an overlapping mask
  index.mask <- tmasks$MF$masks[[w]]
  olap <- tmasks$time_mask_overlap
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
  MF_mask <- missval.args$pr_opts$MF
  missval.out <- remove.FUDGE.missvals(OH=OH.data, MH=MH.data, MF=MF.data,
                                       missval.args$minsamplesize, 
                                       missval.args, 
                                       missval.method = 'synchronous')
  for (n in 1:nrips){
    args <- c(args, list(targ.var=targ.var))
    if (missval.out$ds.possible){
      #If there is enough data to downscale
      #Multi ensembles in OH and MH get pooled in the next step
      OH.data <- missval.out$OH
      MH.data <- missval.out$MH
      MF.data <- missval.out$MF #added 11-8-16; might be responsible for lm diffs
      lm.model <- switch(ds.method, 
                         'lmAsync' = lmAsync.train(OH.data, 
                                                     MH.data, args), 
                         'lmstepAsync' = lmstepAsync.train(OH.data, MH.data, args)
      )
      MF.slice <- lapply(MF.data, '[', ,,,n,,)
      MF.slice <- lapply(MF.slice, 'as.vector')
      ds.out <- switch(ds.method, 
                       'lmAsync' = lmAsync.downscale(lm.model, MF.slice,args), 
                       'lmstepAsync' = lmstepAsync.downscale(lm.model, MF.slice, args)
      )
      #out.data[,,,n,,] <- index.qm(index.mask, ds.out, olap)
      notna.index <- which(!is.na(unlist(MF.slice[grepl(targ.var, names(MF.slice))])))
      #print(str(missval.out))
      #print(summary(missval.out$complete.fut))
      out.data[,,,n,,][missval.out$complete.fut[,,,n,,]] <- ds.out
      if(missval.out$mv.retry & train_qcmask){
        #Double-check on pr settings for this - are there different settings
        #for retry and insufficient points?
        #qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
        qc[,,,n,,] <- NA #index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }else{
      print('downscaling not possible at this point')
      out.data[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      if(train_qcmask){
        qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }
  }
  #Final indexing step - just in case there is an overlapping mask
  # (which is possible, this is supposed to support the pm2weeks masks)
  if (olap){
    out.data[,,,n,,] <- apply.any.mask(mask=index.mask, 
                                       data=out.data[,,,n,,], dim='time')
  }
  return(list(ds=out.data, qc=qc))
}


lmAsync.train <- function(OH, MH, args){
  #OH and MH are named lists of vectors returned by compress.xy.data
  OH <- lapply(OH, remove.missvals)
  MH <- lapply(MH, remove.missvals)
  OH <- lapply(OH, as.vector)
  MH <- lapply(MH, as.vector)
  #Sort all by the target variable (first el OH)
  #order.all <- order(MH[[1]])
  #OH <- lapply(OH, '[', order.all)
  #MH <- lapply(MH, '[', order.all)
  ##That's not what we're doing - instead, we should sort OH and MH independent
  #of each other, for quantile mapping
  oh.order <- order(OH[[1]])
  OH <- lapply(OH, '[', oh.order)
  if ('prime_predictor' %in% names(args)){
    mh.order <- order(MH[[args$prime_predictor]])
  }else{
    mh.order <- order(MH[[1]])
  }
  MH <- lapply(MH, '[', mh.order)
  
  #Missing values between two datasets already synched from missing value removal function
  test.list <- c(list('target'=OH[[1]]), MH)
  test.prod <- data.frame(test.list)
  lm.model <- lm(data=test.prod, na.action='na.omit') #na.omit='don't include incompletes'
  return(lm.model)
}

lmAsync.downscale <- function(lm.model, MF, args){
  #MF is a named list of vectors returned from compress.xy.data
  #...so we can't sort by DS. We can at least sort MF by rank, though.
  MF <- lapply(MF, remove.missvals)
  MF <- MF[!is.null(names(MF))]

  if ('prime_predictor' %in% names(args)){
    mf.order <- order(MF[[args$prime_predictor]])
    mf.rank  <- rank(MF[[args$prime_predictor]], ties='first')
  }else{
    mf.order <- order(MF[[1]])
    mf.rank <- rank(MF[[1]], ties='first')
  }  
  MF <- lapply(MF, '[', mf.order)
  out <- suppressWarnings(predict.lm(lm.model, newdata=as.data.frame(MF)))
  return(out[mf.rank])
}

lmstepAsync.train <- function(OH,MH,args){
  #'Calls R's lm() function using targ as a function
  #'of linear variability of one or more variables in 
  #'pred, optimizes the model with step, and then uses 
  #'the data in new to predict the future results
  #'Does not currently take any arguments according to XMLGen; 
  #'k can be specified for training purposes.
  OH <- lapply(OH, remove.missvals)
  MH <- lapply(MH, remove.missvals)
  
  oh.order <- order(OH[[1]])
  OH <- lapply(OH, '[', oh.order)
  if ('prime_var' %in% names(args)){
    mh.order <- order(MH[[args$prime_var]])
  }else{
    mh.order <- order(MH[[1]])
  }
  MH <- lapply(MH, '[', mh.order)
  
  if(!is.null(args$k)){
    k.val <- args$k
  }else{
    k.val <- 2
  }
  direc='forward'
  targ.var <- args$targ.var
  test.list <- c(OH, MH)
  test.prod <- data.frame(matrix(unlist(test.list), ncol=length(test.list)))
  names(test.prod) <- c('target', names(MH))
  #Sort test.prod by the first element in the DF
  test.prod <- test.prod[order(test.prod[names(MH)[1]]), ]
  #Get model back
  lm.train.model <- step(lm(as.formula(paste0('target',"~1")),
                            data=test.prod, na.action='na.omit'), #.^2, 
                         scope=list(upper=paste0('target','~',paste0(names(MH), collapse="+"),sep=""),
                                    lower=paste0('target','~1')),
                         scale=0, direction=direc, 
                         trace=0,
                         keep=NULL, steps=(length(names(MH))-1), k=k.val)
  return(lm.train.model)
}

lmstepAsync.downscale <- function(lm.train.model, MF, args){
  #'Calls downscaling function on lmstep 
  #'#sort MF - we don't have DS, but we can sort by first el of MF instead
  
  MF <- lapply(MF, remove.missvals)
  if ('prime_var' %in% names(args)){
    mf.order <- order(MF[[args$prime_var]])
    mf.rank  <- rank(MF[[args$prime_var]])
  }else{
    mf.order <- order(MF[[1]])
    mf.rank <- rank(MF[[1]])
  }
  MF <- lapply(MF, '[', mf.order)
  tmp.out <- suppressWarnings(predict(lm.train.model, newdata=MF))
  if(length(tmp.out)==0){
    warning(paste("Warning in lmstep.downscale: step eliminated all variables,", 
                  "resulting in output of length 0! Returning a subset of MF data"))
    if ('prime_var' %in% names(args)){
      print("returning prime_var")
      return(MF[[args$prime_var]][mf.rank])
    }else{
      MF.idx <- which(grepl(args$targ.var, names(MF)))
      if (length(MF.idx) > 0){
        print("returning targ.var in predictors")
        return(MF[[MF.idx]][mf.rank])
      }else{
        #In some cases, target is not present in predictors.
        stop(paste("Error in lmstep.downscale: prime_var not specified", 
                   "and targ.var not found in predictors!"))
      }
    }
  }else{
    return(tmp.out[mf.rank])
  }
}

set.lmAsync.defaults <- function(args, ..., env){
  #Sets the defaults for the lmAsync function - including behavior if no prime
  #predictor (climvar + xy location) is specified.
  #For this method, it is assumed that lmAsync has 'prime_var' set in its 
  #arguments.
  
  #get stencil - should be same for MH and MF
  if (! ('prime_var' %in% names(args))){
    stop(paste("Error in set.lmAsync.defaults: prime_var is required!"))
  }
  prime_predictor <- get.prime.predictor(args$prime_var, aux.ref)
  return_args <- c(args, list(prime_predictor=prime_predictor))
  env$ds.args <- return_args
  env$prime_predictor <- prime_predictor
  return(env)
}

set.lmstepAsync.defaults <- function(args, ..., env){
  #Same.
  
  #get stencil - should be same for MH and MF
  if (! ('prime_var' %in% names(args))){
    stop(paste("Error in set.lmAsync.defaults: prime_var is required!"))
  }
  prime_predictor <- get.prime.predictor(args$prime_var, aux.ref)
  return_args <- c(args, list(prime_predictor=prime_predictor))
  env$ds.args <- return_args
  env$prime_predictor <- prime_predictor
  return(env)
}
