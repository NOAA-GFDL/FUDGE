#QCDSArguments.R
#'Checks for agreement between the core and optional parameters for the
#'downscaling method and data.
#'Relies upon variables set in DSMethodInformation.R
#'Does not return anything at present.

QCDSArguments <- function(k, ds.method, ...){
  #Check for cross-validation conflict
  #if(k > 1 && !crossval.possible){
  #  stop(paste("Cross-Validation Conflict Error: Method", ds.method,
  #             "does not support cross-validaton. Check documentation for more details."))
  #}
  #New check for cross-validation conflict:
  if (is.multi_ens){
    if (k > 1){
      stop(paste("Error in QCDSArguments: k of ", k, "is not", 
                 "supported for multi-ensemble data!"))
    }
  }else{
    if (k > 1){
      #Check construction of tmasks
      if (tmasks$apply_tmasks){
        tmask_types = c('OH', 'MH', 'MF', 'trim')
        k_tmask <- function(tmask_name){
          #All between 'kfold' and '.nc'
          kt <- gsub(".*kfold\\s*|\\.nc", "", tmask_name)
          if (is.numeric(as.integer(kt))){
            return(as.numeric(kt))
          }else{
            return(-1)
          }
        }
        #...check construction of 'olap' in the tmask names
        for (tname in tmask_types){
          k_tmp <- k_tmask(tmasks$filenames[[tname]])
          if (k_tmp != k){
            stop(paste("Error in QCDSArguments: the ", tname, 
                       "tmask", tmasks$filenames[[tname]], 
                       'did not match the kfold of the input', 
                       k_tmp, "!"))
          }
        }
      }
      #check construction of experiment names
      exp_pos = regexpr('K[0-9]{2}', ds.experiment)
      exp_name_kfold = as.numeric(substr(ds.experiment, exp_pos + 1, exp_pos + 2))
      print("checks on experiment name kfold")
      print(ds.experiment)
      print(exp_name_kfold)
      print(k)
      if (exp_name_kfold != k){
        dtop(paste("Error in QCDSArguments: the kfold in the experiment name", 
                   ds.experiment, ',namely,', exp_name_kfold, 
                   "did not match the kfold of the input", k, '!'))
      }
    }
  }
}

QCAdjustArguments <- function(pre_ds, post_ds,
                              is.multivar, is.multi.ens,
                              target.in.predictors,
                              predictor.vars, target.vars, ds.method){
  multivar.possible  <- c('PR', 'flag.neg', 'transform')
  multi_ens.possible <- c('PR', 'SBiasCorr', 'flag.neg', 'BCQM.tail.adj', 'transform', 'simpletailadj', 'cdft_simpletailadj', 'cdft_limtailadj', 'limtailadj')
  target.req.in.pred <- c('PR', 'SBiasCorr', 'bcsd.correct', 'BCQM.tail.adj','simpletailadj', 'cdft_simpletailadj', 'cdft_limtailadj', 'limtailadj')
  #Downscaling method checks
  simpletailadj.possible <- c("BCQM", 'PARM')
  #Obtain all adjustments that will be performed
  pre.ds.list <- lapply(pre_ds, '[[', 'type')
  post.ds.list <- lapply(post_ds, '[[', 'type')
  list.all <- c(pre.ds.list, post.ds.list)
  #Check each of them against the adjustments that will be performed
  for(adj in list.all){
  	if (length(adj) > 0){    if(is.multivar){
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
    if (adj == 'simpletailadj'){
      if (!(ds.method %in% simpletailadj.possible)){
        stop(paste("Error in QCAdjustArguments: simpletailadj is not certified for", 
                   "use with",ds.method,"!"))
      }
    }
    }
  }
}
