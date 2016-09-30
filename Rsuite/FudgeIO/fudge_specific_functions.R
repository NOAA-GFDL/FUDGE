#fudge-specific_functions.R
#grouping of functions that are only going to be used by FUDGE;
#includes padding subset data into larger chunks for looping

build.FUDGE.dataset <- function(data.path, varlist, subset, dim, input_by, spat.mask.nc, x.pad='NA'){
  #Wrapper function for reading in and verifying input data.
  #calls build.fudge.object (general io constructor)
  #and then pads the result with chosen pad, if the subset overlaps the end of the dataset
  print(summary(subset))
  print(lapply(subset, '[[', 'subset'))
  out <- build.fudge.object(data.path, var=varlist, subset=lapply(subset, '[[', 'subset'), dim=dim)
  message("Applying spatial mask to dataset")
  if(!is.null(spat.mask.nc)){
    out.spatmask <- ReadMaskNC(spat.mask.nc, subset=lapply(subset, '[', 'subset'))
    out <- apply.fudge.spatmask(data=out, mask=out.spatmask$masks[[1]])
  }
  #If by > 1, always pad each var along the y axis
  #This goes for vars meant to be 1-d (like ENSO)
  if (input_by > 1){
    print('padding subset with missing values')
    out$data <- lapply(out$data, 'pad.fudge.var.dim', pad.dim='y', pad.by=input_by, #pad.by=(input_by-1)/2, 
                       pad.type=x.pad)
    if (subset$x$olap){
      #And if the subset overlaps the max x indices, pad along the x axis
      #This does not occur for vars that are meant to be 1-d (like ENSO)
      out$data <- lapply(out$data, 'pad.fudge.var.dim', pad.dim='x', pad.by=subset$x$olap_by, 
                         pad.type=x.pad, pad.x.loc=subset$x$olap_loc)
    }
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
