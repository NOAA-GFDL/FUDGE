#'WriteSelectGlobals.R
#'Writes a selected section of global attributes to a netCDF file
#'@param filename: the name of the netCDf file to which the values are being written.
#'@param atts: A named list where the entries are the values of attributes to be written, and the names
#'are the attributes to which to write the values.
#'@param append: If the attributes currently exist in the file, controsl whether to overwrite the attribute
#'or append the new result to the end. Can be either a single boolean or a vector of booleans of the same
#'length as atts.
#'@example WriteSelectGlobals(file=newfile.nc, atts=list(history='no history yet', comment='sample file'), append=TRUE)
#'
#'Carolyn Whitlock, March 2015

WriteSelectGlobals <- function(filename, atts, append=TRUE){
  if (file.exists(filename)){
    nc.object <- nc_open(filename, write=TRUE)
  }else{
    stop(paste("Error in WriteSelectGlobals: file", filename, "does not exist!"))
  }
  #account for single value for append
  if(length(append==1)){
    append <- rep(append, length(atts))
  }else{
    if(length(atts)!=length(append)){
      nc_close(nc.object)
      stop(paste("Error in WriteSelectGlobals: length of atts and append", 
                 "are not the same, and length of append is not 1."))
    }
  }
  #Now, get to the looping over the filename
  for(i in 1:length(atts)){
    att.name <- names(atts[i])
    att.val <- atts[[i]]
    print(att.name)
    if(append[i]==TRUE){
      new.str <- ncatt_get(nc.object, 0, att.name)
      if(new.str$hasatt==TRUE){
        att.val <- paste(new.str$val, att.val, sep=";")
      }
    }
    ncatt_put(nc.object, 0, att.name, att.val)
  }
  nc_close(nc.object)
}