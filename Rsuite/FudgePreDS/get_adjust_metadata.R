#get.adjust.metadata 
#Gets metadata needed for s3/s5 adjustments in R from input datasets
#and places in a list for easy access.

get.adjust.metadata <- function(OH, MH, MF){
  out.metadata <- list()
  #get pr vars (needed for pr)
  oh_idx <- sapply(OH$metadata, '[[', 'is_precip')
  out.metadata$pr_oh <- names(oh_idx)[oh_idx]
  mh_idx <- sapply(MH$metadata, '[[', 'is_precip')
  out.metadata$pr_mh <- names(mh_idx)[mh_idx]
  mf_idx <- sapply(MF$metadata, '[[', 'is_precip')
  out.metadata$pr_mf <- names(mf_idx)[mf_idx]
  #get units of pr vars (needed for pr)
  out.metadata$pr_oh_units <- lapply(OH$metadata, '[[', 'units')[oh_idx]
                                    # function(x, pr_name){x[[pr_name]][['units']]}, 
                                    # out.metadata$pr_oh)
  out.metadata$pr_mh_units <- lapply(MH$metadata, '[[', 'units')[mh_idx]
  out.metadata$pr_mf_units <- lapply(MF$metadata, '[[', 'units')[mf_idx]
  out.metadata$pr_vars <- NA
  if (!is.null(pr_opts)){
    if ('vars' %in% names(pr_opts)){
    out.metadata$pr_vars <- unlist(strsplit(pr_opts$vars, ","))
    }
  }
  return(out.metadata)
}