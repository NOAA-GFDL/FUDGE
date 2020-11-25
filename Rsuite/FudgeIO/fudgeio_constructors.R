build.fudge.object <- function(filenames, varnames, #start.indices=NA, count.indices=NA, 
                               subset=list(), dim=NA, 
                               var_classification=F, verbose=F){
  #'Builds the data structures that we are intersted in
  #'for getting data in a form that's parsable by FUDGE
  
  if (length(filenames) != length(varnames)) {
    stop(paste("Error in build.fudge.object: different numbers of filenames and vars!"))
  }

  #Enforce common order when getting dimensions in from file
  #Hint: it's the order that we are going to be writing files in, 
  #not the order that we are going to be using for calculation.
  #Possibly. I need to talk to someone about overheads for aperm
  #in R. Aperm's going to remporarily create another array of the same
  #length in R - memory's not been a concern yet, but it could be for 
  #multiple points.
  if (!is.na(dim[1])){
    dim.order <- sapply(X=c('spatial', 'space', 
                            'ensemble', 'ensmem',
                            'n', 'other',
                            'time', 'temporal'), grep, dim)
    dim.order <- unlist(dim.order[sapply(dim.order, 'length') > 0], use.names=F)
    dim <- dim[dim.order]
  }
  
  fudge.data <- list()
  fudge.metadata <- list()
  for (v in 1:length(varnames)){
    filename <- filenames[[v]]
    nc.var <- varnames[[v]]
    nc.obj <- nc_open(filename)
    #We are going to buils separate structures for data and metadata
    
    #data: 
    var.data <- obtain.fudge.var(nc.obj, nc.var, subset)
    
    #metadata:
    var.metadata <- obtain.fudge.var.metadata(nc.obj, nc.var)
    var.metadata[['dim']] <- obtain.fudge.var.dimensions(nc.obj, nc.var, subset=subset, select=dim)
    var.metadata[['vars']] <- obtain.fudge.var.metavars(nc.obj, nc.var, subset=subset, select=dim)
    #...assume that the combining steps can be taken before writing to file?
    #that is, you decide which vars get cloned from at that point?
    nc_close(nc.obj)
    
    check.for.ens.dim <- F
    if (nc.var %in% names(fudge.data)){
      ##For the moment, assume that you are only going to merge across the 
      ##ensemble dimension - which is close to what is needed in FUDGE.
      fudge.data[[nc.var]] <- abind(fudge.data[[nc.var]], var.data, along=5)
      ##Append any non-identcal metadata attributes
      ##For now, don't even bother trying on the dims and vars
      avoid.merge <- c('dim', 'vars')
      meta.atts <- names(var.metadata)[ !(names(var.metadata) %in% avoid.merge)]
      for (el in meta.atts){
        if (fudge.metadata[[nc.var]][[el]] != var.metadata[[el]])
          check.for.ens.dim <- T
        fudge.metadata[[nc.var]][[el]] <- paste0(fudge.metadata[[nc.var]][[el]], 
                                                 ",", var.metadata[[el]])
      }
    }else{
      fudge.data[[nc.var]] <- var.data
      fudge.metadata[[nc.var]] <- var.metadata
    }
  }
  if (check.for.ens.dim){
    get.dim.names()
    #Remove ensemble dim if already exists
    if ( any(names(fudge.metadata[[nc.var]]$dim) %in% ens.dims) ){
      dim.match <- names(fudge.metadata[[nc.var]]$dim) %in% ens.dims
      fudge.metadata[[nc.var]]$dim <- fudge.metadata[[nc.var]]$dim[-dim.match]
    }
    #Add new, guaranteed-correct ensemble dim
    ens.dim <- ncdim_def('ensmem', units="", vals=1:dim(fudge.data)[5])
    time.loc <- which(names(fudge.metadata[[nc.var]]$dim) %in% ens.dims)[1]
    if (length(time.loc)==0){time.loc <- 1}
    n.loc <- which(names(fudge.metadata[[nc.var]]$dim) %in% ens.dims)[1]
    if (length(n.loc)==0){n.loc <- length(fudge.metadata[[nc.var]]$dim)}
    fudge.metadata[[nc.var]]$dim <- c(fudge.metadata[[nc.var]]$dim[1:time.loc], 
                                      list('ensmem' = ens.dim), 
                                      fudge.metadata[[nc.var]]$dim[time.loc+1:n.loc])
  }
  #In the main FUDGE workflow, we are ALWAYS going to want a pointer to the
  #metadata object - it has the potential to get large and messy
  
  #Add metadata propertioes of var based off of lookup table
  if (var_classification){
    #print('var clasification added')
    var.table <- paste0(FUDGEROOT, "Rsuite/aux/var_att_sheet.csv")
    if (! file.exists(var.table)){
      stop(paste("Error in build.fudge.object: var_classification was true, but", 
                 var.table, "was not found!"))
    }else{
      #open table
      input <- read.csv(var.table, header=T, stringsAsFactors=F)
      table_varnames <- input$var_name
      properties <- c('is_precip')
      defaults <- list(is_precip = F)
      for (var in varnames){
        if (! var %in% table_varnames){
          print(paste(var, 'not found in lookup table; using defaults'))
          for (el in properties){
            fudge.metadata[[var]][[el]] <- defaults[[el]]
          }
        }else{
          var.idx <- which(table_varnames==var)
          for (el in properties){
            fudge.metadata[[var]][[el]] <- input[[el]][[var.idx]]
          }
        }
      }
    }
  }
  return(list(data=fudge.data, metadata=fudge.metadata))
}

build.legacy.fudge.object <- function(filename, nc.var, 
                                      start.indices=NA, count.indices=NA, 
                                      dim=NA, verbose=F){
  #'Builds the data structures that we are intersted in
  #'for getting data in a form that's parsable by FUDGE
  
  fudge.output <- list()
  nc.obj <- nc_open(filename)
  #We are going to buils separate structures for data and metadata   
  #data: 
  fudge.output$clim.in <- obtain.fudge.var.old(nc.obj, nc.var)
  #metadata:
  att_table <- obtain.fudge.var.metadata(nc.obj, nc.var)
  attr(fudge.output$clim.in, 'prec') <- att_table$prec
  fudge.output <- c(fudge.output, att_table)
  fudge.output$dim <- obtain.fudge.var.dimensions(nc.obj, nc.var, dim)
  fudge.output$vars <- obtain.fudge.var.metavars(nc.obj, nc.var, dim)
  #...assume that the combining steps can be taken before writing to file?
  #that is, you decide which vars get cloned from at that point?
  nc_close(nc.obj)
  return(fudge.output)
}
