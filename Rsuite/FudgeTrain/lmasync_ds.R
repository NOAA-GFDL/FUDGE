#lm and lmstep calls and main downscaling functions.
#Both functions are designed to work on multivar datasets,
#and handle multiple points by decomposing into separate vars.

callLMAsync <- function(OH, MH, MF, args, ds.method='lm', 
                        missval.args, tmasks, train_qcmask=F){
  #'Setup operations for methods that behave a lot like lm
  #'and lmstep: output time-synchronous with the future, 
  #'traing steps are supposed to have same timelevels present/
  #'not present in OH and MH.
  #'PARM doesn't quite qualify due to its internal setup requirements; 
  #'it' smostly its own weird thing.
  #Pre-processing step to compress vectors into multivariate/multi-point form
  OH.data <- compress.xy.data(OH)
  MH.data <- compress.xy.data(MH)
  MF.data <- compress.xy.data(MF)
  targ.var <- names(OH.data)
  nrips <- dim(MF.data[[1]])[4]
  out.data <- array(NA, dim=dim(MF.data[[1]]))
  #These methods are asynchronous - they can take an overlapping mask
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
  missval.out <- remove.FUDGE.missvals(OH=OH.data, MH=MH.data, MF=MF.data,
                                       missval.args$minsamplesize, 
                                       missval.args, 
                                       missval.method = 'synchronous')
  # save(file="/work/cew/testing/h_pr_2.R", list=c('missval.out', 'missval.args'))
  #  print('ds.possible:')
  #  print(missval.out$ds.possible)
  if (missval.out$ds.possible){
    #If there is enough data to downscale
    #Multi ensembles in OH and MH get pooled in the next step
    OH.data <- missval.out$OH
    MH.data <- missval.out$MH
    MF.data <- missval.out$MF #added 11-8-16; might be responsible for lm diffs
    lm.model <- switch(ds.method, 
                       'lmAsync' = multiparm.train(OH.data, 
                                                   MH.data), 
                       'lmstepAsync' = multiparm_step.train(OH.data, MH.data, args)
    )
    for (n in 1:nrips){
      MF.slice <- lapply(MF.data, '[', ,,,n,,)
      MF.slice <- lapply(MF.slice, 'as.vector')
      ds.out <- switch(ds.method, 
                       'lmAsync' = multiparm.downscale(lm.model, MF.slice), 
                       'lmstepAsync' = multiparm_step.downscale(lm.model, MF.slice, targ.var)
      )
      out.data[,,,n,,] <- index.qm(index.mask, ds.out, olap)
      if(missval.out$mv.retry & train_qcmask){
        #Double-check on pr settings for this - are there different settings
        #for retry and insufficient points?
        qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }
  }else{
    print('downscaling not possible at this point')
    out.data[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
    if(train_qcmask){
      qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
    }
  }
  return(list(ds=out.data, qc=qc))
}


multiparm.train <- function(OH, MH){
  #OH and MH are named lists of vectors returned by compress.xy.data
  OH <- lapply(OH, as.vector)
  MH <- lapply(MH, as.vector)
  #Sort all by the target variable (first el OH)
  order.all <- order(MH[[1]])
  OH <- lapply(OH, '[', order.all)
  MH <- lapply(MH, '[', order.all)
  #Missing values between two datasets already synched from missing value removal function
  test.list <- c(list('target'=OH[[1]]), MH)
  test.prod <- data.frame(test.list)
  lm.model <- lm(data=test.prod, na.action='na.omit') #na.omit='don't include incompletes'
  return(lm.model)
}

multiparm.downscale <- function(lm.model, MF){
  #MF is a named list of vectors returned from compress.xy.data
  #...so we can't sort by DS. We can at least sort MF by rank, though.
  rank.orig <- rank(MF[[1]])
  MF <- lapply(MF, '[', order(MF[[1]]))
  out <- suppressWarnings(predict.lm(lm.model, newdata=as.data.frame(MF)))
  return(out[rank.orig])
}

multiparm_step.train <- function(OH,MH,args){
  #'Calls R's lm() function using targ as a function
  #'of linear variability of one or more variables in 
  #'pred, optimizes the model with step, and then uses 
  #'the data in new to predict the future results
  #'Does not currently take any arguments according to XMLGen; 
  #'k can be specified for training purposes.
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
                         scope=list(upper=paste0(targ.var,'~',paste0(names(MH), collapse="+"),sep=""),
                                    lower=paste0(targ.var,'~1')),
                         scale=0, direction=direc, 
                         trace=0,
                         keep=NULL, steps=(length(names(MH))-1), k=k.val)
  return(lm.train.model)
}

multiparm_step.downscale <- function(lm.train.model, MF, targ.var){
  #'Calls downscaling function on lmstep 
  #'#sort MF - we don't have DS, but we can sort by first el of MF instead
  rank.orig <- rank(MF[[1]])
  MF <- lapply(MF, '[', order(MF[[1]]))
  tmp.out <- suppressWarnings(predict(lm.train.model, newdata=MF))
  if(length(tmp.out)==0){
    warning(paste("Warning in callStep.lm: step eliminated all variables,", 
                  "resulting in output of length 0! Returning the MF data for now"))
    MF.idx <- which(grepl(targ.var, names(MF)))
    if (length(MF.idx) > 0){
      return(MF[[MF.idx]][rank.orig])
    }else{
      #In some cases, target is not present in predictors.
      return(MF[[1]][rank.orig])
    }
  }else{
    return(tmp.out[rank.orig])
  }
}