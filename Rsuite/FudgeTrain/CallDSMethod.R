#CallDSMethod.R
#' CallDSMethod
#' Calls a downscaling method specified as ds.method and returns the result of training it
#' upon train.predict and train.target, and then running the relvant equation upon
#' esd.gen. No cross-validation is used for these methods.
#'
#' @param ds.method: a string representation of the downscaling data to be used.
#' @param train.predict: A list of vectors of train.predictor data. For any downscaling run,
#' there can be multiple predictor datasets - both in a multivariate sense and a multiple point
#' sense.
#' @param train.target: a vector of train.target data. For any downscaling run, there should
#' only ever be one target dataset of one variable length.
#' @param esd.gen: A list of lists of vectors of esd generation data. For any downscaling run,
#' if we attempt to train the data and apply it as separate steps, it should be possible to
#' apply that training to several possible realizations - each of which is effectively a
#' train.predict data structure.
#' @param args = NULL: a list of arguments to be passed to the downscaling function.
#' Defaults to NULL (no arguments)
#' @param ds.var: The target variable. Can be used to key off of some methods.
#' @examples
#' @references \url{link to the FUDGE API documentation}
#' TODO: Find a better name for general.bias.corrector


callDSMethod <- function(method, OH, MH, MF, ds.args,
                         missvals="distributional",
                         missval.args=list(minsamplesize=1),
                         train_qcmask=F, tmasks=list(),
                         s5_list=list()){
  #Missing value removal now takes place within the ds method wrappers
  #called by CallDSMethod
  ds.out <- switch(method,
                   'lm'      = callLMAll(OH, MH, MF, ds.args, ds.method='lm',    missval.args, tmasks, train_qcmask),
                   'lmstep'  = callLMAll(OH, MH, MF, ds.args, ds.method='lmstep', missval.args, tmasks, train_qcmask),
                   'lmAsync'      = callLMAsync(OH, MH, MF, ds.args, ds.method='lmAsync',    missval.args, tmasks, train_qcmask),
                   'lmstepAsync'  = callLMAsync(OH, MH, MF, ds.args, ds.method='lmstepAsync', missval.args, tmasks, train_qcmask),
                   'CDFt'    = callCDFt(OH, MH, MF, ds.args,  ds.method="gfdl_CDFt",   missval.args, tmasks, train_qcmask),
                   #'CDFt'    = callCDFt(OH, MH, MF, ds.args,  ds.method="CDFt",   missval.args, tmasks, train_qcmask),
                   'CDFtmod' = callCDFt(OH, MH, MF, ds.args,  ds.method="CDFtmod", missval.args, tmasks, train_qcmask),
                   'PARM'    = callPARM(MH, OH, MF, ds.args, missval.args, tmasks, train_qcmask),
                   'BCQM'    = callQM(OH, MH, MF, ds.args, type='BCQM', missval.args, tmasks, train_qcmask),
                   'EDQM'    = callQM(OH, MH, MF, ds.args, type='EDQM', missval.args, tmasks, train_qcmask),
                   'KDDM'    = callQM(OH, MH, MF, ds.args, type='KDDM', missval.args, tmasks, train_qcmask),
                   #'CFQM'    = callQM(OH, MH, MF, ds.args, type='CFQM', missval.args, tmasks, train_qcmask),
                   'DeltaSD' = callDeltaSD(OH, MH, MF, ds.args, train_qcmask, tmasks),
                   #'MBC'     = callMBC(OH, MH, MF, ds.args, ds.method="MBC", tmasks, train_qcmask),
                   'QDM'     = callMBC(OH, MH, MF, ds.args, ds.method="QDM", tmasks, train_qcmask),
                   'CFQM'    = callCFQM(OH, MH, MF, ds.args, train_qcmask, missval.args, tmasks, s5_list),
                   'returnMF'   = ds.MF.function(OH, MH, MF, ds.args),
                   ReturnDownscaleError(method)
  )
  #Restore missing values to original place now takes place within method
  #No longer taking place within method as of 10-15; now adding as formal indexing
  #step based off of downscaling method classification
  return(list(ds=ds.out$ds, qc=ds.out$qc, ds.atts=ds.out$ds.atts))
}

ReturnDownscaleError <- function(ds.method){
  #Returns an error and stops the entire function if the DS method used is not supported.
  stop(paste("Downscale Method Error: the method", ds.method, "is not supported for FUDGE at this time."))
}

ds.sum.function <- function(OH, MH, MF, args){
  print("within sum function")
  sum.all <- sum(c(OH[[1]], MH[[1]], MF[[1]]), na.rm=T)
  print(sum.all)
  tmp.data <- array(sum.all, dim=c(1,1,1,1,dim(OH[[1]])[5:6]))
  print(dim(tmp.data))
  return(list(ds=tmp.data,
              qc=NULL))
}

ds.MF.function <- function(OH, MH, MF, args){
  #'returns the MF data, unchanged
  return(list(ds=MF[[1]],
              qc=NULL))
}

callSimple.bias.correct <- function(pred, targ, new, args){
  #'Performs a simple bias correction adjustment to a
  #'single variable series, applying the mean difference
  #'between the predictor and target datasets in the
  #'historical period to the esd.gen dataset
  #'to generate downscaled data.
  #'It takes no arguments.
  out.var.name <- names(new)

  bias <- mean(unlist(pred)-targ)
  new.targ <- unlist(new)-bias
  out.list <- list()
  out.list[[out.var.name]] <- new.targ
  return(out.list)
}

callNoDSMethod <- function(pred, targ, new, args, ds.var, att.table){
  out.list <- list()
  rip.list <- lapply(att_table$fut.pred, '[[', 'rip')
  rips <- unique(rip.list)
  for (rip in rips){
    out.var <- paste0(ds.var, rip)
    rip.index <- (rip.list==rips)
    out.list[[out.var]] <- new[[rip.index]][[1]]
  }
  return(out.list)
}
