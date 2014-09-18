#Aparna Radhakrishnan, 08/06/2014
WriteGlobals <- function(filename,kfold,predictand=NA,predictor=NA,
                         label.training=NA,downscaling.method=NA,reference=NA,label.validation=NA,
                         institution='NOAA/GFDL',version='undefined',title="undefined"){
#a1r: removing count.dep.samples=NA,count.indep.samples=NA from function params
  #'Adds global attributes to existing netCDF dataset 
  info <- ""
  if(!is.na(downscaling.method)){ 
  info <- paste('Output produced from ',downscaling.method,' downscaling ',sep='')
  }
  if(!is.na(kfold)){
  info <- paste(info, '(based on ',kfold,'-fold',' cross-validation).',sep='') 
  }
  if(!is.na(ds.experiment)){
    info <- paste(info, 'with experiment configuration', ds.experiment, ").")
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
  info <- paste(info, ', predictor(s) used: ',predictor,sep='')
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
  if(info != ""){
  ncatt_put(nc.object, 0 , "comment", info )
  }
  #' TODO info versus comment global attribute
  if(!is.na(reference)){
  ncatt_put(nc.object, 0 , "references", reference )
  }
  nc_close(nc.object) 
  return(filename)
}

