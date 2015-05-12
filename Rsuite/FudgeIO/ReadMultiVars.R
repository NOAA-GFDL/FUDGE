#'ReadMultiVars.R
#'Reads in data from a list of netCDF files 
#'covering the same area and same time series (as determined by the paths to the input files)
#'and writes the files as a single object with variables and dims cloned from the 
#'first file, and a $clim.in value
#'that consists of the various components of the 
ReadMultiVars <- function(file.prefix, file.suffix, blank.filename, var.list, dim=NA, 
                          add.ens.dim=FALSE, 
                          check.for.ens = FALSE, ens.member = "",
                          verbose=FALSE){
  #First, determine what the dimensions of the input files will be. 
  #To do that, you first need to create the input .nc objects, and look
  #At their dimensions. 
  nc.list <- list()
  dim.list <- list()
  if(check.for.ens){
    #Check for the 'to' string that indicates an ensemble file; 
    if(grepl("_r[0-9]to*", ens.member)){
      ens.regex <- gregexpr("_r[0-9]to*", ens.member)
      ens.string <- substr(ens.member, ens.regex[[1]] + 1, nchar(ens.member))
      ens.start <- as.numeric(substr(ens.string, 2, 2))
      ens.end <- as.numeric(substr(ens.string, 5, 5))
      ens.vector <- paste("r", seq(ens.start:ens.end), substr(ens.sring, 6, nchar(ens.sring)))
      #adjust the filename accordingly if present
      blank.filename <- sub(".nc", "_ensmbl.nc", blank.filename)
    }
    
  }
  for(var in var.list){
    file.name <- sub(pattern="VAR", replacement=var, x=blank.filename)
    file.path <- paste(file.prefix, var, file.suffix, sep="/")
    file.path <- paste(file.path, file.name, sep="/")
    if(verbose){print(file.path)}
    nc.list[[length(nc.list) + 1]] <- nc_open(file.path)
    dim.list[[length(dim.list) + 1]] <- obtain.nc.dim(nc.list[[length(nc.list)]], var=var, verbose=verbose)
  }
  #Do a brief consistency check to make sure that all the spatial and temporal dims will match each other
  #Question: is everything going to be forced into 4 dimensions? Ask again later.
  #For now, it's probably not unsafe to assume that everything will have 3 dims, and that multivariate is the 
  #way to go for the future
  if(length(dim.list)!=1){
    #Only run check if there are multiple vars
    for(d in 1:(length(dim.list)-1)){
      if(sum(dim.list[[d]]!=dim.list[[d+1]])!=0){
        #If all dims are not equal, first check for degenerate dims
        d1.select <- dim.list[[d]][c(1, 2, length(dim.list[[d]]))] #only *really* need to make sure that x, y, t are equal
        d2.select <- dim.list[[d + 1]][c(1, 2, length(dim.list[[d + 1]]))]
        if(sum(d1.select!=d2.select)==0){
          message(paste("Warning in ReadMultiVars: dim.list", d, " obtained from var", var.list[[d]], "was", paste(dim.list[[d]], collapse=" "), 
                        "and contained different degenrate dimensions than dim.list", 
                        d+1, "obtained from var", var.list[[d+1]], ", which was", paste(dim.list[[d+1]], collapse=" ")))
        }else{
        stop(paste("Error in ReadMultiVars: dim.list", d, " obtained from var", var.list[[d]], "was", paste(dim.list[[d]], collapse=" "), 
                   "and contained substantially different dimensions than dim.list", 
                   d+1, "obtained from var", var.list[[d+1]], ", which was", paste(dim.list[[d+1]], collapse=" ")))
        }
      }
    }
  }
  #Create vector to store all var values
  all.dim <- c(dim.list[[1]][1:3]) # No longer adding a var/ensemble dimension
  print(all.dim)
  data.array <- array(rep(NA, prod(all.dim)), dim=all.dim)
  #Then, once the dimensions are determined, look over the files and obtain the vars, 
  #slotting them into the relevant bits one by one. 
  #Don't forget the var naming conventions at this point: var is the first part of the 
  #name, rip is the second, and point identity is the third.
  #Initialize table of relevant attributes (because this is going to be important later)
#   att.table <- list(var.name=unlist(var.list),    #Names of the variables. If muti files, will need editing.
#                     #$var.longname=unlist(var.list),#Determined from the input file. cfname in other places.
#                     var.units="",                 #Units for each variable. Same caveat applies as above.
#                     var.prec="",                  #Precision when writing to file. Maybe not a big deal, but keep losing it.
#                     point="",                     #The part of the x,y coordinate scheme that can be deduced. Not implemented yet.
#                     p.rep="",                     #Physics rip used. Not used yet, and might not get used at all.
#                     var = ""                      #The name of the variable used. Just the shortname, no additions used.    
#   )
  data.list <- rep(list(data.array), length(var.list))
  #Make new names if ensembel members are being used as well
  if(check.for.ens)
  names(data.list) <- unlist(var.list)
    att_table <- list()
  for(v in 1:length(var.list)){
    var=var.list[[v]]
    if(verbose){print(paste("Reading in var", var))}
    if(v==1){
      out.list <- ReadNC(nc.list[[v]], var=var, dim=dim, force_3_dimensions=TRUE) #No longer adding ensemble dimension
      data.list[[v]] <- out.list$clim.in
      att_table[[var]] <- out.list$att_table[[var]]
    }else{
      temp.list <- ReadNC(nc.list[[v]], var=var, dim='none', force_3_dimensions=TRUE)
      data.list[[v]] <- temp.list$clim.in
      att_table[[var]] <- temp.list$att_table[[var]]
    }
  }
  #Assign and exist
  out.list$clim.in <- data.list
  out.list$att_table <- att_table
  return(out.list)
}

obtain.nc.dim <- function(nc.object, var, verbose=FALSE){
  #Obtains the dimensions of a netCDf object 
  #related to a specific var; used to pre-allocate
  #arrays based on current vector dims
  if (verbose) {print(paste("size of var", var, "in", nc.object$filename, ":", 
                            paste(nc.object$var[[var]]$size, collapse=" "))) }
  return(nc.object$var[[var]]$size)
}