#'ReadMaskNC.R
#'Reads in a NetCDF file representing one or more masks  - variables
#'where all values for which a calculation will be preformed are 1, 
#'and all missing values are NA
#'@param mask.nc: A ncdf4 obejct returned by OpenNC
#'@param var.name: The variable name within the file. If not used, 
#'the function defaults to returning all variables identified as 
#'masks within the file (i.e. containing the word 'mask' in the var name)
#'@param verbose: Whether or not to print debugging information. Defaults
#'to FALSE.
#'@return A list containing the data from the file under data, and the
#'timeseries and lat/lon coordinates of the file underneath dim. Also
#'contains a 'filename' attribute associated with the list, and a
#''calendar' attribute associated with the timeseries origin. 
#'@examples
#'
ReadMaskNC <- function(mask.nc,var.name=NA,verbose=FALSE, get.bounds.vars=FALSE) {
  message('Obtaining mask vars')
  mask.var <- names(mask.nc$var)[which(regexpr(pattern="mask", names(mask.nc$var)) != -1)]
  if(identical(mask.var, character(0))){
    stop(paste("Mask name error: no variable within the file", mask.nc$filename, 
               "has a name that matches the pattern 'mask'. "))
  }
  mask.list <- list()
  for (name in 1:length(mask.var)){
    mask.name <- mask.var[name]
    if(verbose){
      message(paste("Obtaining", mask.name, ":mask", name, "of", length(mask.var)))
    }
    mask <- ncvar_get(mask.nc,mask.name, collapse_degen=FALSE) #verbose adds too much info
    mask.list[[mask.name]] <- mask
  }    
  listout <- list('masks' = mask.list)
  attr(listout, "filename") <- mask.nc$filename
  nc_close(mask.nc)
  return(listout)
}

create.ncvar.list <- function(mask.nc, varname, dim.string){
  #'Creates a list with elements named in a manner appropriate
  #'for a NetCDF variable. 
  return(list('name' = varname, 
              'units' = mask.nc$var[[varname]]$units, 
              'dim' = dim.string,
              'longname' = mask.nc$var[[varname]]$longname,
              'prec' = correct.int(mask.nc$var[[varname]]$prec) ))
}

correct.int <- function(string){
  if(string=="int"){
    return("integer")
  }else{
    return(string)
  }
}
