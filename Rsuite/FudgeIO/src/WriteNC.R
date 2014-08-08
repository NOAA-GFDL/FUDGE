WriteNC <-  function(filename,data.array,var.name,xlon,ylat, time.index.start, time.index.end, 
                     start.year="undefined",units ,calendar,lname=var.name,cfname=var.name) {
  #'Creates file filename (netCDF type) with the variable  var.name along with the 
  #'coordinate variables in the netCDF file, CF standard name, long names.
  #' Data to populate the file is read from data.array.
  time1 <- 1:((time.index.end - time.index.start)+1)
  y <- ncdim_def("lat","degrees_north",ylat)
  if(exists("xlon") & (xlon != '')){
    x <- ncdim_def("lon","degrees_east",xlon)
  }
  tunit <- paste('days since ',start.year,'-01-01 12:00:00',sep='')
  print(tunit) 
  t1 <- ncdim_def("time",tunit,time1,unlim=TRUE)
  #' If CFNAME undefined in the call, pull information from CF.R. Use default otherwise. 
  print(cfname)
  if(cfname == var.name){
    source("CF.R")
    cflist <- GetCFName(var.name)
    if(mode(cflist) == "list"){
      cfname <- cflist$cfname
      cflongname <- cflist$cflongname
      print(paste("cfname:",cfname,sep=''))
    }else{
      print("CF.R does not contain this variable. Using default values")
    }
  }
  if(exists("xlon") & (xlon != '')){
    var.dat <- ncvar_def(var.name,units,list(x,y,t1),1.e30,longname=cflongname,prec="double")
  }else{
    var.dat <- ncvar_def(var.name,units,list(y,t1),1.e30,longname=cflongname,prec="double")
  }

  nc.obj <- nc_create(filename,var.dat)
  ncvar_put(nc.obj, var.dat, data.array)
  #' gets CF mappings from CF.R if user does not pass these 

  ncatt_put(nc.obj,"time","calendar",calendar)
  ncatt_put(nc.obj,var.dat,"units",units)
  ncatt_put(nc.obj,var.dat,"standard_name",cfname)
  nc_close(nc.obj)
  return(filename)
}
