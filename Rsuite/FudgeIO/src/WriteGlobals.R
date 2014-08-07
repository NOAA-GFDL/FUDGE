WriteGlobals <- function(filename,count.dep.samples,count.indep.samples,kfold,predictand=NA,predictor=NA,label.training=NA,downscaling.method=NA,reference=NA,label.validation=NA,institution='NOAA/GFDL',version='undefined',title="undefined"){
  #'Adds global attributes to existing netCDF dataset 
  info <- ""
  if(!is.na(downscaling.method)){ 
  info <- paste('Output produced from ',downscaling.method,' downscaling ',sep='')
  }
  if(!is.na(kfold)){
  info <- paste(info, '(based on ',kfold,'-fold',' cross-validation).',sep='') 
  }
  if(!is.na(predictand)){
  info <- paste(info, 'This is a downscaled estimate of ',predictand,sep='')
  }
  if(!is.na(label.validation)){
  info <- paste(info,' for the ', label.validation,' experiment',sep='')
  }
  if(!is.na(label.training)){
  info <- paste(info,' having done training using the ',label.training, ' time series',sep='')
  }
  if(!is.na(predictor)){
  info <- paste(info, 'and predictor(s) ',predictor,sep='')
  }
  history <- paste('File processed at ',institution,'  using FUDGE (Framework For Unified Downscaling of GCMs Empirically) developed at GFDL, version:  ', version ,' on ', date(), sep='')
  if(file.exists(filename)){ 
    print("File already exists. Open in WRITE MODE")
    nc.object = open.ncdf(filename,write=TRUE)
  }else{
    print(paste('!!ERROR:  File ',filename,' does not exist for writing global attributes. Quitting..',sep=''))
    stop("error code returned: Invalid file specified")
  }
  att.put.ncdf(nc.object, 0 , "title", title )
  att.put.ncdf(nc.object, 0 , "history", history )
  print("debug1")
  att.put.ncdf(nc.object, 0 , "institution", institution )
  print("debug2")
  if(info != ""){
  att.put.ncdf(nc.object, 0 , "comment", info )
  }
  #' TODO info versus comment global attribute
  if(!is.na(reference)){
  att.put.ncdf(nc.object, 0 , "references", references )
  }
  close.ncdf(nc.object) 
  return(filename)
}

