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


callDSMethod <- function(method, OH, MH, MF, args, 
                         missvals="distributional", 
                         minsample=1, missval.args=list(), 
                         train_qcmask=F, tmasks=list()){
#   save(file="/home/cew/Code/h_testing//ds_debug/save/ds_call.R", 
#        list=c('OH', 'MH', 'MF'))
#   stop('examine output')
  #Remove missing values
  missval.out <- remove.FUDGE.missvals(OH, MH, MF, 
                                       minsample, missval.args, 
                                       missval.method='distributional', 
                                       return_qcmask = train_qcmask)
  OH <- missval.out$OH
  MH <- missval.out$MH
  MF <- missval.out$MF
  train.qc   <- missval.out$qcmask
  train.stat <- missval.out$missval.status
  ds.possible <- missval.out$ds.possible
  remove(missval.out) #The remove might not be neccessary here, but it doesn't hurt
  #don't bother assigning status for now
  #Call downscaling function
  if (ds.possible){
    ds.out <- switch(method, 
                     'lm'      = callMultiLM(OH, MH, MF, args), 
                     'lmstep'  = callStep.lm(OH, MH, MF, args), 
                     'CDFt'    = callCDFt(OH, MH, MF, args, ds.method="CDFt", train_qcmask, tmasks), 
                     'CDFtmod' = callCDFt(OH, MH, MF, args, ds.method="CDFtmod", train_qcmask, tmasks),
                     'PARM'    = callPARM(MH, OH, MF, args),
                     'BCQM'    = callQM(OH, MH, MF, args, type='BCQM', tmasks),
                     'EDQM'    = callQM(OH, MH, MF, args, type='EDQM', tmasks),
                     'CFQM'    = callQM(OH, MH, MF, args, type='CFQM', tmasks),
                     'DeltaSD' = callDeltaSD(OH, MH, MF, args, train_qcmask, train.stat, tmasks),
                     ReturnDownscaleError(method)
    )
    #Restore missing values to original place now takes place within method
    #No longer taking place within method as of 10-15; now adding as formal indexing
    #step based off of downscaling method classification
    #ds.out <- method$index_method(tmasks, time_mask_overlap, ds.out)
    ##back to indexing internal to ds methods again
    #as a h-release expidency shortcut, combine ds.out and missval qc masks
    #note: in planned I, these are going to count as two separate masks
    if (train_qcmask){
      ds.out$qc <- ds.out$qc * train.qc # 1 and NA; NAs propagate
    }
    #If there are not sufficient points to downscale, return all missing values
  }else{
    print('Insufficient values to attempt downscaling; returning NA')
    if (train_qcmask){
      ds.out=list(ds = array(NA, dim=dim(MF[[1]])), 
                  qc = array(NA, dim=dim(MF[[1]])))
    }else{
      ds.out=list(ds = array(NA, dim=dim(MF[[1]])), 
                  qc = NULL)
    }
  }
  return(list(ds=ds.out$ds, qc=ds.out$qc))
}

ReturnDownscaleError <- function(ds.method){
  #Returns an error and stops the entire function if the DS method used is not supported.
  stop(paste("Downscale Method Error: the method", ds.method, "is not supported for FUDGE at this time."))
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
