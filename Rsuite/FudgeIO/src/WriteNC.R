# Aparna Radhakrishnan 08/04/2014
WriteNC <-  function(filename,data.array,var.name,xlon,ylat, time.index.start, time.index.end, 
                     start.year="undefined",units ,calendar,lname=var.name,cfname=var.name) {
  #'Creates file filename (netCDF type) with the variable  var.name along with the 
  #'coordinate variables in the netCDF file, CF standard name, long names.
  #' Data to populate the file is read from data.array.
  #print(data.array)
  FUDGEROOT = Sys.getenv(c("FUDGEROOT"))

  time1 <- time.index.start:time.index.end 
  y <- ncdim_def("lat","degrees_north",ylat)
  if(exists("xlon") & (xlon != '')){
    x <- ncdim_def("lon","degrees_east",xlon)
  }
  #tunit <- paste('days since ',start.year,'-01-01 12:00:00',sep='')
  tunit <- origin
  print(tunit) 
  t1 <- ncdim_def("time",tunit,time1,unlim=TRUE)
  #' If CFNAME undefined in the call, pull information from CF.R. Use default otherwise. 
  print(cfname)
  if(cfname == var.name){
    source(paste(FUDGEROOT,"Rsuite/FudgeIO/src/","CF.R",sep=""))
    cflist <- GetCFName(var.name)
    if(is.list(cflist)){     ###CEW: Changed because was throwing a warning when cflist != "none"
      cfname <- cflist$cfname
      lname <- cflist$cflongname
      print(paste("cfname:",cfname,sep=''))
    }else{
      print("CF.R does not contain this variable. Using default values")
    }
  }
  if(exists("xlon") & (xlon != '')){
    var.dat <- ncvar_def(var.name,units,list(x,y,t1),1.e20,longname=lname,prec="double")
  }else{
    var.dat <- ncvar_def(var.name,units,list(y,t1),1.e20,longname=lname,prec="double")
  }

  print("creating nc objects")
  nc.obj <- nc_create(filename,var.dat)
  print("placing nc vars")
  ncvar_put(nc.obj, var.dat, data.array)
  print("placing nc variables")
  # gets CF mappings from CF.R if user does not pass these 
  #TODO create grid coordinate bounds variables  
 
  # lets make it close to CF compliancy,shall we
  ncatt_put(nc.obj,"time","calendar",calendar)
  ncatt_put(nc.obj,"time","standard_name","time")
  ncatt_put(nc.obj,"time","axis",'T')
  ncatt_put(nc.obj,"lat","axis",'Y')
  ncatt_put(nc.obj,"lon","axis",'X')
  ncatt_put(nc.obj,"lat","standard_name","latitude")
  ncatt_put(nc.obj,"lat","long_name","latitude")
  ncatt_put(nc.obj,"lon","standard_name","longitude")
  ncatt_put(nc.obj,"lon","long_name","longitude")
  ncatt_put(nc.obj,var.dat,"units",units)
  ncatt_put(nc.obj,var.dat,"standard_name",cfname)
 ########### write grid coordinate bounds ####################

 #############################################################  
  nc_close(nc.obj)
  return(filename)
}
