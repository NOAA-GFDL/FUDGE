#'QCSection5.R
#'@description Parses the list of options provided for adjustment, checks
#'for internal consistency (throwing errors if checks are failed) and 
#'returns information useful in developing file metadata. 
#'@param mask.list: A list of options for adjusting/qc-ing downscaling 
#'functions. 
#'@return A list with four components: qc.check, whether or not to perform 
#'a qc check upon the downscaled data, qc.method, the method used for 
#'any qc checks performed, adjust.pre.qc, the list of adjustments performed
#'before the qc check, and adjust.methods, the list of methods performed
#'before and after the QC check.
#'All are used in the history and info attributes of the metadata.
#'@author Carolyn Whitlock
#'

QCSection5 <- function(mask.list){
  ##Note:masklist is the same things as section.5.list provided earlier, 
  ##and NOT to be confused with tmask.list
  message("checking Section 5 options provided")
  qc.check <- 0
  qc.method <- ""
  adjust.pre.qc <- list()
  adjust.methods <- list()
  adjust.args <- ""
  adjust.pre.qc.args = ""
  qc.args = ""
  for (element in 1:length(mask.list)){
    #message(mask.list[[element]])
    qc.check <- qc.check + as.numeric(mask.list[[element]]$qc.mask=='on')
    if(qc.check==1){
      #Check for QC options
      qc.method <- mask.list[[element]]$type
      if(!is.null(names(mask.list[[element]]$qc_options))){
        arg.names <- names(mask.list[[element]]$qc_options)
        args <- mask.list[[element]]$qc_options
        arg.string <- paste(arg.names, args, sep="=", collapse=", ")
      }else{
        arg.string = "NA"
      }
      qc.args <- paste(qc.method, ":", arg.string, ";", sep="") 
      if(length(adjust.pre.qc)==0){
        adjust.pre.qc <- adjust.methods
        adjust.pre.qc.args <- adjust.args
      }
    }
    if(mask.list[[element]]$adjust.out=='on'){
      #Check for adjustment options (performed AFTER QC in the adjustment functions)
      adjust.methods <- c(adjust.methods, mask.list[[element]]$type)
      if(!is.null(names(mask.list[[element]]$qc_options))){
        arg.names <- names(mask.list[[element]]$qc_options)
        args <- mask.list[[element]]$qc_options
        arg.string <- paste(arg.names, args, sep="=", collapse=", ")
      }else{
        arg.string = "NA"
      }
      adjust.args <- paste(adjust.args, paste(mask.list[[element]]$type, " ", arg.string, sep=""), ";", sep="")
    }
  }
  if (qc.check > 1){
    stop(paste("QC Mask Option Error: FUDGE expected 1 or 0 qc creation options and there were", qc.check))
  }
  qc.check <- as.logical(qc.check)
  print(length(adjust.pre.qc))
  print(length(adjust.methods))
  #Convert argument lists to strings for easier storage
  s5.settings <- list('adjust.methods'=convert.list.to.string(adjust.methods), 
                      'adjust.args' = adjust.args,
                      'adjust.pre.qc'=convert.list.to.string(adjust.pre.qc),
                      'adjust.pre.qc.args' = adjust.pre.qc.args,
                      'qc.check' = qc.check, 'qc.method'=qc.method, 'qc.args'=qc.args)
  return(s5.settings)
}

convert.list.to.string <- function(this.vector){
  #Converts a list into a string representation
  #Does not assume that the list is named
  #(easy to convert though; just count off of the names)
  if(length(this.vector)!=0){
    if(length(this.vector) > 1){
    out <- paste(c(this.vector[1:length(this.vector)-1], paste("and", this.vector[length(this.vector)])), collapse=",")
    return(out)
    }else{
      #no 'and' needed
      return(paste(this.vector))
    }
  }else{
    #No string to convert
    return(NA)
  }
}

#Code for obtaining the filenames of all files from tmask.list
# commandstr <- paste("attr(tmask.list[['", names(tmask.list), "']],'filename')", sep="")
# time.mask.names <- ""
# for (i in 1:length(names(tmask.list))){
#   var <- names(tmask.list[i])
#   time.mask.names <- paste(time.mask.names, paste(var, ":", eval(parse(text=commandstr[i])), ",", sep=""), collapse="")
#   print(time.mask.names)
# }