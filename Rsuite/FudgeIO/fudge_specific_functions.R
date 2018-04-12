#fudge-specific_functions.R
#grouping of functions that are only going to be used by FUDGE;
#includes padding subset data into larger chunks for looping

build.FUDGE.dataset <- function(data.path, varlist, subset, dim, 
                                x_by, y_by, geomasks=c(), 
                                dim.pad='NA', 
                                file_format = 'netcdf'){
  #Wrapper function for reading in and verifying input data.
  #calls build.fudge.object (general io constructor)
  #and then pads the result with chosen pad, if the subset overlaps the end of the dataset
  if (file_format == 'netcdf'){
    out <- build.fudge.object(data.path, var=varlist, subset=lapply(subset, '[[', 'subset'), dim=dim, 
                              var_classification=T)
  }else if (file_format == 'csv'){
    out <- build.fudge.object.csv(data.path, var=varlist, dim=dim)
  }else{
    stop(paste("Error in build.FUDGE.dataset: file format", file_fomat, 'is unknown!'))
  }
  #Sptaial masks get more complicated - stepping stone towards having multiple
  #spatial masks for multiple vars
  if (length(geomasks) > 0){
    apply_geomasks=T
    if (length(geomasks)==1 && length(varlist) > 1){
      message("Repeating geomask for all vars in dataset")
      geomasks <- rep(geomasks[[1]], length(varlist))
    }
  }else{
    apply_geomasks=F
  }
  if (apply_geomasks){
    for (v in 1:length(varlist)){
      vname <- varlist[[v]]
      #Okay - resetting the subset in response to the dimensions of the 
      #variable in use needs to take place in here. 
      #...only trigger if this is known to be of greater than length 1?
      #...or return note from dataset building that fewer dims than expected
      #were available. I don't think that you can apply the mask subset on this 
      #dataset anyway - not sure how the x,y slicing is going to work.
      #so only apply the mask if the dataset says that there's more than
      #one x and y value available?
      geomask.subset <- 1:dim(out$data[[vname]])[1] #only worried about x subset
      if (geomasks[[v]] != 'none'){
        geomask <- ReadMaskNC(nc_open(geomasks[[v]]), subset=lapply(subset, '[[', 'subset')) 
        out$data[[vname]] <- apply.any.mask(data=out$data[[vname]], 
                                            mask=geomask$masks[[1]], dim='spatial')
      }
    }
  }
  
  #Get variable dimensions before padding (used for getting output dims)
  print("getting orig.dims")
  orig.dims <- lapply(out$data, 'dim')
  out$orig_dims <- orig.dims
  
  #If by > 1, always pad each var along the y axis
  #This goes for vars meant to be 1-d (like ENSO)
  #...upon reading this, I am no longer so sure of that. This seems like the 
  #sort of thing that needs to be tested.
  if (subset$y$olap){
    print('padding subset with missing values')
    out$data <- lapply(out$data, 'pad.fudge.var.dim', pad.dim='y', pad.by=subset$y$olap_by,
                       pad.type=dim.pad, pad.loc=subset$y$olap_loc)
  }
  if (subset$x$olap){
    #And if the subset overlaps the max x indices, pad along the x axis
    #This does not occur for vars that are meant to be 1-d (like ENSO)
    out$data <- lapply(out$data, 'pad.fudge.var.dim', pad.dim='x', pad.by=subset$x$olap_by, 
                       pad.type=x.pad, pad.loc=subset$x$olap_loc)
  }
  return(out)
}

get.FUDGE.filepath <- function(basedir,variable,freq,model,scenario,
                               ds.region,start.period,end.period, 
                               suffix="", is_OH=F){
  #'Gets the path to the full data from the information passed in via the 
  #'runcode as of 9-14-16. Slightly different for OH b/c of desire
  #'to not have to revise the runcode
  root.filename <- paste0(paste(variable, freq, model, scenario, ds.region, sep="_"), 
                          "_", start.period, "-", end.period)
  all.filename <- paste0(root.filename, suffix, ".nc")
  if (is_OH){
    print(basedir)
    basedir <- head(unlist(strsplit(basedir, "/")), -3)
    basedir <- paste(basedir, collapse="/")
  }
  return(file.path(basedir, variable, ds.region, all.filename))
}
