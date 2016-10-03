#QCDSArguments.R
#'Checks for agreement between the core and optional parameters for the
#'downscaling method and data.
#'Relies upon variables set in DSMethodInformation.R
#'Does not return anything at present.

QCDSArguments <- function(k, ds.method,...){
  #Check for cross-validation conflict
  if(k > 1 && !crossval.possible){
    stop(paste("Cross-Validation Conflict Error: Method", ds.method,
               "does not support cross-validaton. Check documentation for more details."))
  }
}

QCAdjustArguments <- function(pre_ds, post_ds,
                              is.multivar, is.multi.ens,
                              target.in.predictors,
                              predictor.vars, target.vars){
  multivar.possible  <- c('PR', 'flag.neg', 'transform')
  multi_ens.possible <- c('PR', 'SBiasCorr', 'flag.neg', 'BCQM.tail.adj', 'transform', 'simpletailadj', 'cdfttailadj', 'cdft_orig_tailadj')
  target.req.in.pred <- c('PR', 'SBiasCorr', 'bcsd.correct', 'BCQM.tail.adj','simpletailadj', 'cdfttailadj', 'cdft_orig_tailadj')
  #Obtain all adjustments that will be performed
  pre.ds.list <- lapply(pre_ds, '[[', 'type')
  post.ds.list <- lapply(post_ds, '[[', 'type')
  list.all <- c(pre.ds.list, post.ds.list)
  #Check each of them against the adjustments that will be performed
  for(adj in list.all){
    if(is.multivar){
      if(!adj%in%multivar.possible){
        stop(paste("Error in QCAdjustArguments: method",adj,"does not support",
                   "multiple variables!"))
      }
    }
    if(is.multi.ens){
      if(!adj%in%multi_ens.possible){
        stop(paste("Error in QCAdjustArguments: method",adj,"does not support",
                   "multiple ensemble members!"))
      }
    }
    if(!target.in.predictors){
      if(adj%in%target.req.in.pred){
        stop(paste("Error in QCAdjustArguments: method",adj,"does not support",
                   "target vars that are not present in the predictors!"))
      }
    }
    if(adj=='transform'){
      t.list <- compact(lapply(pre_ds, 'index.a.list', 'type', 'transform'))[[1]]
      print(t.list)
      print('pp.args bit:')
      print(t.list$pp.args)
      print('vars component:')
      print(t.list$pp.args$vars)
      if(sum(sapply(t.list$pp.args$vars, '%in%', c(predictor.vars, target.var)))!=length(t.list$pp.args$vars)){
        stop(paste("Error in QCAdjustArguments: one of the vars",
                   paste(t.list$vars, collapse=","), "was not present in the",
                   "predictor or target vars!"))
      }
    }
  }
}
