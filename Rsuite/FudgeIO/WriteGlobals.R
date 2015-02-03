#Aparna Radhakrishnan, 08/06/2014
#'Writes global variables to a netCDF file, given the filename
#'---Inputs related to the file
#'@param filename: The name of the file to which the global attributes are added.
#'---Inputs related to the variables supplied: 
#'@param kfold
#'@param predictor, predictand: The variables being used to produce downscaled output, 
#'and the variables describing the produced downscaled output. 
#'@param label.training
#'@param downscaling.method
WriteGlobals <- function(filename,kfold,predictand=NA,predictor=NA,
                         label.training=NA,downscaling.method=NA,reference=NA,label.validation=NA,
                         institution='NOAA/GFDL',version='undefined',title="undefined", 
                         ds.arguments='na', time.masks=NA, ds.experiment = 'unknown-experiment', 
                         time.trim.mask='na', 
                         grid_region='somewhere', mask_region='somewhere_subset',
                         tempdir="", include.git.branch=FALSE, FUDGEROOT="", BRANCH='undefined',
#                         is.adjusted=FALSE, adjust.method=NA, adjust.args=NA,
                         is.qcmask=FALSE, qc.method=NA, qc.args=NA,
#                          pr.process=FALSE, pr_opts=NA, 
#                          is.transform=FALSE, transform=NA
                         is.pre.ds.adjust=FALSE,
                         pre.ds.adjustments=list('na'),
                         is.post.ds.adjust=FALSE,
                         post.ds.adjustments=list('na')){
  #a1r: removing count.dep.samples=NA,count.indep.samples=NA from function params
  #'Adds global attributes to existing netCDF dataset 
  #'#TODO CEW: What is the purpose of the comments attribute? 
  #'TODO CEW: What would an apprioriate title attribute actually be?
  comment.info <- "Output produced from "
  if (is.qcmask){
    comment.info <- paste(comment.info, "a QC check ", qc.method, " performed upon ", sep="")
  }
  if(!is.na(downscaling.method)){ 
    comment.info <- paste(comment.info, 'downscaled output via ',downscaling.method,' downscaling',sep='')
  }
  #
  if(grid_region!='somewhere'){
    comment.info <- paste(comment.info, 'on the subset', mask_region, 'of the', grid_region, 'grid ')
  }
  #Note: is.adjusted and is.qcmask can both be true, but is.qcmask only reports on the adjustments that took place before the mask
#   if(is.transform){
#     comment.info <- paste(comment.info, 'transformed and back-transformed with', transform)
#   }
#   if(pr.process){
#     comment.info <- paste(comment.info, 'adjusted for precipitation,')
#   }
  if(is.pre.ds.adjust){
    pre.methods <- lapply(pre.ds.adjustments, '[[', "type")
    comment.info <- paste(comment.info, "adjusted with the ", convert.list.to.string(pre.methods),
                          " method(s) before downscaling", sep="")
  }
  if(is.post.ds.adjust){
    print(post.ds.adjustments)
    post.methods <- lapply(post.ds.adjustments, '[[', "type")
    print(post.methods)
    if(!is.pre.ds.adjust){
      comment.info <- paste(comment.info, "adjusted with the", convert.list.to.string(post.methods), "method(s) after downscaling")
    }else{
      comment.info <- paste(comment.info, "and the", convert.list.to.string(post.methods),
                            "method(s) after downscaling")
    }
  }
  if(!is.na(kfold)){
    comment.info <- paste(comment.info, ', and based on ',kfold,'-fold',' cross-validation, ',sep='') #parenthesis removed
  }
  if(!is.na(ds.experiment)){
    comment.info <- paste(comment.info, 'with experiment configuration', ds.experiment, ").")
  }
  if(!is.na(predictand)){
    if(!is.qcmask){
      comment.info <- paste(comment.info, 'This is a downscaled estimate of ',predictand,sep='')
    }else{
      comment.info <- paste(comment.info, 'This is a QC check for a downscaled estimate of ',predictand,sep='')
    }
  }
  if(!is.na(label.validation)){
    comment.info <- paste(comment.info,' for the ', label.validation,' experiment',sep='')
  }
  if(!is.na(label.training)){
    comment.info <- paste(comment.info,' having done training using the ',label.training, ' time series',sep='')
  }
  if(!is.na(predictor)){
    comment.info <- paste(comment.info, ', predictor(s) used: ', paste(predictor, collapse=", ") ,sep='')
  }
  ##info attribute
  info <- ""
  if(time.masks[[1]]!='na'){
    commandstr <- paste("attr(tmask.list[['", names(tmask.list), "']],'filename')", sep="")
    time.mask.string <- ""
    for (i in 1:length(names(tmask.list))){
      var <- names(tmask.list[i])
      #Removes temp directory from filename in order to keep in line with archive
      tmask.filename <- sub(pattern=tempdir, replacement="", x=eval(parse(text=commandstr[i])))
      time.mask.string <- paste(time.mask.string, paste(var, ":", tmask.filename, ",", sep=""), 
                                collapse="")
    }
    info <- paste("Path to time mask files:", time.mask.string, ";", sep=" ")
  }else{
    info <- paste("No time masks were used to process this data; ")
  }
  if(ds.arguments[[1]]!='na'){
    argnames <- ls.str(args)
    argstring <- paste(argnames, args, sep="=", collapse=", ")
    info <- paste(info, "Arguments used in downscaling function: ", argstring, "; ", sep="")
  }
#   if(pr.process){
#     pr.optstring <- paste(names(pr_opts), pr_opts, sep="=", collapse=", ")
#     info <- paste(info, "Precipitation pre-processing and post-processing options: ", pr.optstring, "; ", sep="")
#   }
  if(is.pre.ds.adjust || is.post.ds.adjust){
    #Section 5 and Section 3 stuff
    if(is.qcmask){
      qc.string = "before QC masks applied "
    }else{
      qc.string = ""
    }
    print(qc.string)
    info <- paste(info, "Arguments used in adjustment functions", qc.string)
    if(is.pre.ds.adjust){
      pre.args <- paste(lapply(pre.ds, '[[', "pp.args"), collapse=",")
      #These next two might need to be in reverse order
      pre.args <- gsub('\"', "", pre.args)
      pre.args <- sub('list', "", pre.args)
      pre.string <- convert.list.to.string(paste(pre.methods, pre.args, sep=":"))
      info <- paste(info, "before downscaling",  ": ", pre.string, sep="")
    }
    if(is.post.ds.adjust){
      if(is.pre.ds.adjust){
        info <- paste(info, "and")
      }
      post.args <- paste(lapply(post.ds, '[[', "qc_args"), collapse=",")
      post.args <- gsub('\"', "", post.args)
      post.args <- sub('list', "", post.args)
      print(post.args)
      post.string <- convert.list.to.string(paste(post.methods, post.args, sep=":"))
      print(post.string)
      info <- paste(info, "after downscaling:", post.string)
    }
    info <- paste(info, ";")
  }
  if(is.qcmask){
    #More section 5 stuff
    info <- paste(info, "Arguments used in calculation of the QC mask: ", qc.args, "; ", sep="")
  }
  if(time.trim.mask!='na'){
    info <- paste(info, "Time trimming mask used:", time.trim.mask, sep="")
  }
  if(include.git.branch){
    popdir <- getwd()
    setwd(FUDGEROOT)
    out <- pipe('git symbolic-ref HEAD')
    branch.string <- readLines(out)
    close(out)
    #out <- pipe('git log | head -1')
    out <- pipe('git describe --always --tag')
    commit.string <- readLines(out)
    close(out)
    message(paste( "Git branch:", branch.string, commit.string))
    info <- paste(info, "Git branch:", branch.string, commit.string)
    setwd(popdir)
  }
  history <- paste('File processed at ',institution,
                   '  using FUDGE (Framework For Unified Downscaling of GCMs Empirically) developed at GFDL, in branch: ', 
                   BRANCH ,' on ', date(), sep='')
  if(file.exists(filename)){ 
    print("File already exists. Open in WRITE MODE")
    nc.object = nc_open(filename,write=TRUE)
  }else{
    print(paste('!!ERROR:  File ',filename,' does not exist for writing global attributes. Quitting..',sep=''))
    stop("error code returned: Invalid file specified")
  }
  ncatt_put(nc.object, 0 , "title", title )
  ncatt_put(nc.object, 0 , "history", history )
  ncatt_put(nc.object, 0 , "institution", institution )
  if(comment.info != ""){
    ncatt_put(nc.object, 0 , "comment", comment.info )
  }
  if(info!=""){
    print("adding info attribute")
    ncatt_put(nc.object, 0, "info", info)
  }
  #' TODO info versus comment global attribute
  if(!is.na(reference)){
    ncatt_put(nc.object, 0 , "references", reference )
  }
  if(is.qcmask){
    ncatt_put(nc.object, paste(predictand, "_qcmask",sep=""), "comments", "Flagged cases = missing; Non-flagged cases = 1.0" )
  }
  nc_close(nc.object) 
  return(filename)
}
