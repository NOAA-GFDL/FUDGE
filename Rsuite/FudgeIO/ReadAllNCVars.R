# Aparna Radhakrishnan 08/04/2014
ReadAllNCVars <- function(nc.object,#dim='none',
                          is.dimvar=FALSE,
                   verbose=FALSE, close_nc=TRUE) {
  #'Reads data from every variable of a netCDF file object
  #'Returns all netCDF variable data and, if dim!= 'none', one or more dimensions of that
  #'netCDF object
  #'-----Arguments-----
  #'@param nc.object: A NetCDF object returned from nc_open of R's ncdf4 package
  #'
  #'@returns An environment containing the following elements: 
  #'  $dim: The dimension(s) present in the file, of class ncdim4
  #'  $vars: All variables present in the input files. 
  #'
  #'@include ncdf4

  data.list <- list()
  dim.list <- list()
    var.names <- names(nc.object$var)
    for(n in var.names){
      loop.var <- nc.object$var[[n]]
      data.list[[n]] <- ncvar_get(nc.object, n, collapse_degen=FALSE)
      att.vector <- c(loop.var$units, loop.var$longname, loop.var$missval, 
                      loop.var$prec)
      att.vector[5] <- paste(names(nc.object$dim)[(loop.var$dimids)+1], collapse=",")
      names(att.vector) <- c("units", "longname", "missval", 
                             "prec", "dimids") #"var_type"
      att.vector[att.vector=='int'] <- "integer"
      for(a in 1:length(att.vector)){
        attr(data.list[[n]], which=names(att.vector)[[a]]) <- att.vector[[a]]
      }
      comments <- ncatt_get(nc.object, loop.var, 'comments')
      if(comments$hasatt){
        attr(data.list[[n]], which='comments') <- comments$value
      }
    }
    #Obtain all dimensions as well
    dim.names <- names(nc.object$dim)
    dim.names <- dim.names[dim.names!='bnds']
    for(n in dim.names){
      dim.list[[n]] <- nc.object$dim[[n]]
      #Make sure that original file is being included, in order to support attribute cloning
      if(verbose) { print(paste("writing filename attribute", nc.object$filename)) }
      attr(dim.list[[n]], "filename") <- nc.object$filename
    }
    if(close_nc){
      nc_close(nc.object)
    }
    #Create and return as an environment
        envout =  new.env(parent = emptyenv())
        envout$dim=dim.list
        envout$vars=data.list
        ###Add attributes for later QC checking against each other
        attr(envout, "calendar") <- nc.object$dim$time$calendar
        attr(envout, "filename") <- nc.object$filename
    return(envout)
}