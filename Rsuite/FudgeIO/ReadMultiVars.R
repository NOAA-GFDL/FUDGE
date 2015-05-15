#'ReadMultiVars.R
#'Reads in data from a list of netCDF files 
#'covering the same area and same time series (as determined by the paths to the input files)
#'and writes the files as a single object with variables and dims cloned from the 
#'first file, and a $clim.in value
#'that consists of the various components of the 
ReadMultiVars <- function(file.prefix, file.suffix, blank.filename, var.list, dim=NA, 
                          add.ens.dim=FALSE, 
                          #check.for.ens = FALSE, ens.member = "",
                          rip.list = list('fake_rip'), rip.suffix = '', 
                          verbose=FALSE){
  #First, determine what the dimensions of the input files will be. 
  #To do that, you first need to create the input .nc objects, and look
  #At their dimensions. 
  nc.list <- list()
  dim.list <- list()
  if(length(rip.list) > 1){
    multi.rip <- TRUE
    rip.count <- length(rip.list)
  }else{
    multi.rip <- FALSE
    rip.count <- 1
  }
  #Always add rip.suffix; if not present, it's an empty string
  temp.split <- unlist(strsplit(blank.filename, "\\."))
  blank.filename <- paste(paste0(temp.split[1], rip.suffix), temp.split[2], temp.split[3], sep=".")

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
#                     rip="",                     #Physics rip used. Not used yet, and might not get used at all.
#                     var = ""                      #The name of the variable used. Just the shortname, no additions used.    
#   )

  data.list <- list() #Need to turn on intialization of arrays again at some point
    #rep(list(data.array), (length(var.list)*rip.count))
  if(multi.rip){
    #Initialize a temporary list for adding structures back into data.list gracefully
    temp.list <- rep(list(data.array), (length(var.list)*rip.count))
  }
    att_table <- list()
  #This is a mess, and I'm not sure how to straighten it out.
  for(v in 1:length(var.list)){
    var=var.list[[v]]
    var.names <- paste0(var, ".", rip.list)
    #print(paste('variable names:', paste(var.names, collapse=", ")))
    if(verbose){print(paste("Reading in var", var))}
    if(v==1){
      #First time through, you want to grab the dimensions; second time through, that is not neccessary
      out.list <- ReadNC(nc.list[[v]], var=var, dim=dim, force_3_dimensions=(!multi.rip)) #No longer adding ensemble dimension
      #Contents at this point: 
      # 4-dimensional dataset
      # att table containing some, but not all, of the elements that you need
      if(multi.rip){
        #var.names <- paste0(var, ".", rip.list)
        names(temp.list) <- var.names
        temp.att.table <- rep(list(out.list$att_table), rip.count)
        names(temp.att.table) <- var.names
        for(i in 1:rip.count){
          data.list[[var.names[i]]] <- out.list$clim.in[,,i, ,drop=F]  #This assignment needs to preserve degenerate dims
          #Assign and replace the dimensions to match x, y, t rather than x, y, ens
          current.dim <- dim(data.list[[var.names[i]]])
          dim(data.list[[var.names[i]]]) <- c(current.dim[1], current.dim[2], current.dim[length(current.dim)])
          att_table[[var.names[i]]] <- out.list$att_table[[1]] #This assignment is problematic - needs to assign
          #list contents, not the list itself
          att_table[[var.names[i]]]$rip <- rip.list[[i]]
        }
      }else{
        data.list[[var.names]] <- out.list$clim.in
        att_table[[var.names]] <- out.list$att_table[[var]]
        att_table[[var.names]]$rip <- rip.list[[1]]
      }
    }else{
      temp.out.list <- ReadNC(nc.list[[v]], var=var, dim='none', force_3_dimensions=(!multi.rip))
      if(multi.rip){
        #var.names <- paste0(var, ".", rip.list)
        names(temp.list) <- var.names
        temp.att.table <- rep(list(temp.out.list$att_table), rip.count)
        names(temp.att.table) <- var.names
        #Mass assignment does not work well. 
        for(i in 1:rip.count){
          data.list[[var.names[i]]] <- temp.out.list$clim.in[,,i,]
          att_table[[var.names[i]]] <- temp.out.list$att_table
          att_table[[var.names[i]]]$rip <- rip.list[[i]]
        }
      }else{
        data.list[[var.names]] <- temp.out.list$clim.in   ##v
        att_table[[var.names]] <- temp.out.list$att_table[[var]] #var
        att_table[[var.names]]$rip <- rip.list[[1]]
      }
    }
    #If you have multi ensembles included, be sure
    #to add the var to the 
  }
  #Assign and exit
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