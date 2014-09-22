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
                         post.process=""){
#a1r: removing count.dep.samples=NA,count.indep.samples=NA from function params
  #'Adds global attributes to existing netCDF dataset 
  comment.info <- ""
  if(!is.na(downscaling.method)){ 
  comment.info <- paste('Output produced from ',downscaling.method,' downscaling ',sep='')
  }
  if(!is.na(kfold)){
  comment.info <- paste(comment.info, '(based on ',kfold,'-fold',' cross-validation).',sep='') 
  }
  if(!is.na(ds.experiment)){
    comment.info <- paste(comment.info, 'with experiment configuration', ds.experiment, ").")
  }
  if(!is.na(predictand)){
  comment.info <- paste(comment.info, 'This is a downscaled estimate of ',predictand,sep='')
  }
  if(!is.na(label.validation)){
  comment.info <- paste(comment.info,' for the ', label.validation,' experiment',sep='')
  }
  if(!is.na(label.training)){
  comment.info <- paste(comment.info,' having done training using the ',label.training, ' time series',sep='')
  }
  if(!is.na(predictor)){
  comment.info <- paste(comment.info, ', predictor(s) used: ',predictor,sep='')
  }
  ##info attribute
  info <- ""
  if(!is.na(time.masks)){
    info <- paste("Path to time mask files:", time.masks, "\n", sep=" ")
  }
  if(ds.arguments!='na'){
    argnames <- ls.str(args)
    argstring <- paste(argnames, args, sep="=", collapse=", ")
    info <- paste(info, "Arguments used in downscaling function:", argstring, "\n", sep=" ")
  }
  if(post.process!=""){
    info <- paste(info, "Processing options: ", post.process, "\n", sep="")
  }
  history <- paste('File processed at ',institution,'  using FUDGE (Framework For Unified Downscaling of GCMs Empirically) developed at GFDL, version: ', version ,' on ', date(), sep='')
  if(file.exists(filename)){ 
    print("File already exists. Open in WRITE MODE")
    nc.object = nc_open(filename,write=TRUE)
  }else{
    print(paste('!!ERROR:  File ',filename,' does not exist for writing global attributes. Quitting..',sep=''))
    stop("error code returned: Invalid file specified")
  }
  ncatt_put(nc.object, 0 , "title", title )
  ncatt_put(nc.object, 0 , "history", history )
  print("debug1")
  ncatt_put(nc.object, 0 , "institution", institution )
  print("debug2")
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
  nc_close(nc.object) 
  return(filename)
}

