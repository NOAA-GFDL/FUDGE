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
  message('All mask vars obtained; starting dimension vars')
  dimvec <- c("lat", "lon", "time")
  dimvec.writestring <- c('y', 'x', 't1')
  offsets <- c("j_offset", "i_offset")
  dim.list <- list()
  var.list <- list()
  for (dim in 1:length(dimvec)){
    #      dimvar <- ncvar_get(mask.nc, dimvec[dim], collapse_degen=FALSE, verbose=verbose)
    dimname <- dimvec[dim]
    dimvar <- mask.nc$dim[[dimname]]$vals
    dim.var.list <- list()
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
        #If selected, look for metadata variables
        if ("bnds" %in% names(mask.nc$dim) && get.bounds.vars==TRUE){             # || "bounds" %in% names(mask.nc$dim)
          message("Searching for time bounds")
          bounds.var <- paste(dimname, "_", "bnds", sep="")
          if (bounds.var %in% names(mask.nc$var) && get.bounds.vars==TRUE){
            var.list[[bounds.var]]$vals <- ncvar_get(mask.nc, bounds.var)
            #Create a string of the form "c(bnds, varname.of.bnds)"
            #dim.string <- paste("c(bnds,", dimvec.writestring[dim], ")", sep="")
            dim.string <- dimvec.writestring[dim]
            var.list[[bounds.var]]$info <- create.ncvar.list(mask.nc, bounds.var, dim.string)
          }else{
            message('No var time_bnds found within file despite bnds dim; proceeding without it')
          }
        }
        #Assign the var list back into the dimension structure
        #dim.list[[dimname]]$vars <- dim.var.list
      }else{
        dim.list[[dimname]] <- dimvar
        message(paste("Adding", dimname, 'dimension'))
        ##If selected, look for metadata variables (i.e. lat_bnds, j_offset)
        if ("bnds" %in% names(mask.nc$dim) && get.bounds.vars==TRUE){             # || "bounds" %in% names(mask.nc$dim)
          bounds.var <- paste(dimname, "_", "bnds", sep="")
          print(bounds.var)
          print((bounds.var %in% names(mask.nc$var)))
          if (bounds.var %in% names(mask.nc$var)){
            var.list[[bounds.var]]$vals <- ncvar_get(mask.nc, bounds.var)
            #dim.list[[dimname]]$vars$vals[[bounds.var]] <- ncvar_get(mask.nc, bounds.var)
            #Create a string of the form "c(bnds, varname.of.bnds)"
            #dim.string <- paste("c(bnds,", dimvec.writestring[dim], ")", sep="")
            dim.string <- dimvec.writestring[dim]
            var.list[[bounds.var]]$info <- create.ncvar.list(mask.nc, bounds.var, dim.string)
          }else{
            message(paste('No var ', dimname, "_bnds found within file despite bnds dim; proceeding without it", sep=""))
          }
        }
        #Determine if there is an i or j offset that could be used
        if (offsets[dim] %in% names(mask.nc$var)){
          var.list[[offsets[dim]]]$vals <- ncvar_get(mask.nc, offsets[dim])
          dim.string <- "NULL"
          var.list[[offsets[dim]]]$info <- create.ncvar.list(mask.nc, offsets[dim], dim.string)
          attr(var.list[[offsets[dim]]], "comments") <- ncatt_get(mask.nc, offsets[dim], "comments")$value
          attr(var.list[[offsets[dim]]], "missing_value") <- ncatt_get(mask.nc, offsets[dim], "missing_value")$value
        }
        #Assign the var list back into the dimension structure
        #dim.list$vars <- dim.var.list
      }
  }
  #######################################################
  listout <- list('masks' = mask.list, 'dim' = dim.list, 'vars'=var.list)
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
