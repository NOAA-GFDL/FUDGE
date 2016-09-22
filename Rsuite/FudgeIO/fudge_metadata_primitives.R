make.fudge.history <- function(inst,ver){
  paste('File processed at',inst,
        'using FUDGE (Framework For Unified Downscaling of GCMs Empirically) developed at GFDL, using version:', 
        ver,'on', date())
}
make.fudge.comment <- function(ds.method, ds.var, mask.region, grid.region, kfold, 
                               experiment.name, experiment.config, training.data, 
                               predictor.vars, fut.pred.rips, qc.method=NA){
  #'The comment attribute stores a human-readable description of the experiment
  #'and its arguments.
  is_qcmask <- !is.na(qc.method)
  if (is_qcmask){
    string.start <- paste("Output produced from a QC mask check", qc.method, "performed upon", ds.method, 'downscaled output')
    string.desc <- paste("This is a QC check related to a downscaled estimate of", ds.var) 
  }else{
    string.start <- paste("Output produced by downscaling by the", ds.method, "method")
    string.desc <- paste("This is a downscaled estimate of", ds.var)
  }
  string.main <- paste("on the subset", mask.region, "of the", grid.region, "grid", 
                       ",and based on", paste0(kfold, "-fold cross-validation,"), 
                       "with experiment configuration", paste0(experiment.name,"."), 
                       string.desc, "for the", experiment.config, "experiment", 
                       "having done training using the", training.data, "time series,", 
                       "predictor(s) used:", paste0(predictor.vars,","), 
                       "future predictor rip used:", paste(fut.pred.rips, collapse=","))
  return(paste(string.start, string.main))
}
make.fudge.info <- function(){
  #'The info attribute in the output file stores a summary of all args that would
  #'go into a downscaling experiment, with little comcern about being human-readable.
  #'This makes some bits trickier, since it has to do a lot more parsing to figure out
  #'what pieces are going to be present.
  return("Adding info attributes later; this is going to take a while to track")
}
get.fudge.climatology <- function(){
  return("Why isn't the climatology being picked up as one of the returned atts?")
}

get.qc.comment <- function(qc.method){
  #'When we get mor einto the mask revisions, use qc_maskvar instead
  basedir = Sys.getenv("BASEDIR")
  if(basedir==""){basedir='/home/cew/Code/fudge/'}
  qc.metatable <- file.path(basedir, '/Rsuite/aux/qcmask_metadata.csv')
  qc.metadata <- read.csv(qc.metatable)
  qc.methodlist <- qc.metadata[,2]
  qc.match <- which(qc.methodlist==qc.method)
  if (length(qc.match)==0){
    qc.comment <- "1 = unadjusted; NA = adjusted"
  }else{
    qc.comment <- qc.metadata[qc.match,3] 
  }
  return(qc.comment)
}