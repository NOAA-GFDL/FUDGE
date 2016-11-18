getMissvalArgs <- function(args, ds.ref.method){
  #'Pulls the missing value arguments parameter from 
  #'the input ds arguments, where it is currently stuck.
  #'I hope this doesn't stick aroudn longer than H.
  missval.args <- list(minsamplesize = 1, 
                       missval_method = ds.ref.method)
  if (!(is.null(args$minsamplesize))){
    missval.args$minsamplesize <- args$minsamplesize
  }
  if (!(is.null(args$missvals))){
    missval.args$missval_method <- args$missvals
  }
  return(missval.args)
}