OpenNC <- function(indir, in.filename, ilon=NA,jlat=NA) {
  #' Function: OpenNC.R
  #' Opens an existing file for reading (or writing)
  #' Ability to open minifiles
  #' Returns the netCDF file object (nc.object)
  #' uses nc_open from ncdf4
  #' ilon jlat are optional. If your file is a minifile with a "lon" index appended to the end, then #the function constructs the file name based on that and the "lat" range. Otherwise, filename is used as-is.
  if ((is.na(ilon)) & (is.na(jlat))) {
    print("..")
    fileid <- '' 
  }else if ((!is.na(ilon)) & (!is.na(jlat))) {
    #' the file is a minifile/OneD/ZeroD
    #' For OneD, ylat example: J454-567
    #' For ZeroD, ylat example: J511 
    fsuffix=paste("J",jlat,sep='')  
    fileid=paste(".I",ilon,"_",fsuffix,".nc",sep='')
    }else if((is.na(ilon)) | (is.na(jlat))) {
      print("Insufficient options. Please pass xlon and ylat. Program quitting")
      quit("no")
    }
  filename <- paste(indir,in.filename,fileid,sep='')
  print(filename) 
  nc.object = nc_open(filename) 
  return(nc.object)
}  
