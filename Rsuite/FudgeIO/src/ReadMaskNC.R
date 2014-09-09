#'ReadMaskNC.R
#'Reads in a NetCDF file representing one or more masks  - variables
#'where all values data is expected to be calculated are 1, and all
#'missing values are NA. 
#'@param mask.nc: A ncdf4 obejct returned by OpenNC
#'@param var.name: The variable name within the file. If not used, 
#'the function defaults to returning all variables identified as 
#'masks within the file
#'@param verbose: Whether or not to print debugging information. Defaults
#'to FALSE.
#'@return A list containing the data from the file under data, and the
#'timeseries and lat/lon coordinates of the file underneath dim. Also
#'contains a 'filename' attribute associated with the list, and a
#''calendar' attribute associated with the timeseries origin. 
#'@examples
#'
ReadMaskNC <- function(mask.nc,var.name=NA,verbose=FALSE) {
    message('Obtaining mask vars')
    mask.var <- names(mask.nc$var)[which(regexpr(pattern="mask", names(mask.nc$var)) != -1)]
    if(is.null(mask.var)){
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
    message('All mask vars obtained; starting dimension vars')
    dimvec <- c("lat", "lon", "time", "bnds", "bounds")
    dimvec.writestring <- c('y', 'x', 't1', 'bnds')
    dim.list <- list()
    for (dim in 1:length(dimvec)){
#      dimvar <- ncvar_get(mask.nc, dimvec[dim], collapse_degen=FALSE, verbose=verbose)
      dimname <- dimvec[dim]
      dimvar <- mask.nc$dim[[dimname]]$vals
      if (!is.null(dimvar))
        if (dimname=='time'){
          #grab calendar
          calendar <- mask.nc$dim$time$calendar
          #grab origin for later use
          origin <- mask.nc$dim$time$units
          dim.list$time <- CreateTimeseries(dimvar, origin, calendar, sourcefile = mask.nc$filename)
          dim.list$tseries <- dimvar
          attr(dim.list$tseries, "origin") <- origin
          message(paste("Adding time dimension"))
#          print(paste("origin: ", attr(dim.list$tseries, "origin")))
        }else if (dimname=='bnds' || dimname=='bounds'){
          dim.index <- length(dimvec)-2
          for (i in 1:dim.index){
            bnds.var <- paste(dimvec[i], "_", dimname, sep="")
            
          }
        }else{
          dim.list[[dimname]] <- dimvar
          message(paste("Adding", dimname, 'dimension'))
        }
    }
  #######################################################
  listout <- list('masks' = mask.list, 'dim' = dim.list)
    attr(listout, "filename") <- mask.nc$filename
  nc_close(mask.nc)
  return(listout)
}

create.ncvar.list <- function(mask.nc, varname, dim.string){
  #'Creates a list with elements named in a manner appropriate
  #'for a NetCDF variable. 
  return(list('name' = varname, 
              'units' = mask.nc$var[varname]$units, 
              'dim' = dim.list,
              'longname' = mask.nc$var[varname]$longname,
              'prec' = mask.nc$var[varname]$prec))
}