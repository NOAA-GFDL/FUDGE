WriteNC <-  function(filename,data.array,var.name,xlon,ylat,time.index.start,time.index.end,start.year="undefined",units,calendar,lname=var.name,cfname=var.name) {
  #'Creates file filename (netCDF type) with the variable  var.name along with the coordinate variables in the netCDF file, CF standard name, long names. Data is read from data.array.
  time1 <- 1:((time.index.end - time.index.start)+1)
  y <- dim.def.ncdf("lat","degrees_north",ylat)
  if(exists("xlon") & (xlon != '')){
    x <- dim.def.ncdf("lon","degrees_east",xlon)
  }
  tunit <- paste('days since ',start.year,'-01-01 12:00:00',sep='')
  print(tunit) 
  t1 <- dim.def.ncdf("time",tunit,time1,unlim=TRUE)
  #' If CFNAME undefined in the call, pull information from CF.R. Use default otherwise. 
  print(cfname)
  if(cfname == var.name){
    source("/home/a1r/gitlab/fudge2014/Rsuite/FudgeIO/src/CF.R")
    cflist <- GetCFName(var.name)
    if(cflist == "none"){
      print("CF.R does not contain this variable. Using default values")
    }else{
    cfname <- cflist$cfname
    cflongname <- cflist$cflongname
    print(paste("cfname:",cfname,sep=''))
    }
  }
  if(exists("xlon") & (xlon != '')){
    var.dat <- var.def.ncdf(var.name,units,list(x,y,t1),1.e30,longname=cflongname,prec="double")
  }else{
    var.dat <- var.def.ncdf(var.name,units,list(y,t1),1.e30,longname=cflongname,prec="double")
  }

  nc.obj <- create.ncdf(filename,var.dat)
  put.var.ncdf(nc.obj, var.dat, data.array)
  #' gets CF mappings from CF.R if user does not pass these 

  att.put.ncdf(nc.obj,t1,"calendar",calendar)
  att.put.ncdf(nc.obj,var.dat,"units",units)
  att.put.ncdf(nc.obj,var.dat,"standard_name",cfname)
  close.ncdf(nc.obj)
  return(filename)
}
