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
ReadMaskNC <- function(mask.nc,var.name=NA,verbose=FALSE, 
                       dim='none', allmask=FALSE, subset=list()) {
  if (verbose) { message('Obtaining mask vars') }
  mask.var <- names(mask.nc$var)[which(regexpr(pattern="mask", names(mask.nc$var), 
                                               ignore.case=TRUE) != -1)]
  if(identical(mask.var, character(0))){
    stop(paste("Mask name error: no variable within the file", mask.nc$filename, 
               "has a name that matches the pattern 'mask'. "))
  }
  ###bugfix 12-11 for making sure that the seasonal climo axis matches the order
  ###in which masks are applies
  ###Note: this does not include the change in the fudge branch that 
  ###based things off of the season instead.
  if(sum(sapply(c('djf', 'mam', 'jja', 'son'), grepl,
               paste(mask.var, collapse=";")))==4){
    #if the mask id strings are present for all vars
    message('enforcing order of seasonal mask vars')
    seas.order = sapply(X=c('djf', 'mam', 'jja', 'son'), grep, mask.var)
    mask.var <- mask.var[seas.order]
  }
  mask.list <- list()
  for (name in 1:length(mask.var)){
    mask.name <- mask.var[name]
    if(verbose){ message(paste("Obtaining", mask.name, ":mask", name, "of", length(mask.var))) }
    mask <- obtain.fudge.mask.var(mask.nc, mask.name, subset=subset)
    
    #mask <- ncvar_get(mask.nc,mask.name, collapse_degen=FALSE)
    if(allmask){
      #Allmasks are required to be of 4 dimensions for proper data comparison
      if(verbose) { message("Adjusting dimensions to fit the FUDGE framework") }
      if(length(dim(mask)) < 4){
        message('correcting to 4-dimensional output')
        #Determine dimensions present
        dim.names <- c('lon','lat','ensmem','time')
        check.dimlen <- function(dim.name,nc.obj){
          if(!is.null(mask.nc$dim[[dim.name]])){
            return(mask.nc$dim[[dim.name]][['len']])
          }else{
            return(1)
          }}
        true.dims <- as.vector(unlist(sapply(dim.names,'check.dimlen',mask.nc)))
        print(true.dims)
        dim(mask) <- true.dims
      }
    }
    mask.list[[mask.name]] <- mask
  }    
  listout <- list('masks' = mask.list)
  attr(listout, "filename") <- mask.nc$filename
  base.name <- basename(mask.nc$filename)
  #Changes needed to account for the needs of the function that makes more masks
  #No worries; funcitons should be sourced in here anyway
  if(verbose){message(paste('dim:',dim))}
  if(!('none'%in%dim)){
    verbose = TRUE
    print(paste('mask.var:', mask.var[1]))
    dim.list <- list()
    for(d in 1:length(dim)){
      temp.list <- switch(dim[d], 
                          "spatial"=get.space.vars(mask.nc, mask.var[1], verbose=verbose), 
                          "temporal"=get.time.vars(mask.nc, mask.var[1], verbose=verbose), 
                          "ensemble" = get.ens.dim(mask.nc, mask.var[1], verbose=verbose),
                          #If other arg or "nothing", do nothing
                          list("dim"=list("none"), 'vars'=list("none"))
      )
      dim.list$dim <- c(dim.list$dim, temp.list$dim)
      dim.list$vars <- c(dim.list$vars, temp.list$vars)
    }
    listout$dim <- dim.list$dim
    listout$vars <- dim.list$vars
  }
  #nc_close(mask.nc)
  return(listout)
}

correct.int <- function(string){
  if(string=="int"){
    return("integer")
  }else{
    return(string)
  }
}
