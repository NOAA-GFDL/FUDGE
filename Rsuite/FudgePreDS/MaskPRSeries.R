#'MaskPRSeries.R
#'Function for setting all values of precip
#'less than a threshold to NA
#'------Parameters related to the data being masked-------
#'@param ref.data, adjust.data, future.data: The precipitation data
#'for the target and predictor (ref and adjust), as well as an optional
#'future dataset (adjust.fufutre, currently used for CDFt)
#'@param ref.units, adjust.unts, future.units: The units of 
#'precipitation. Defaults to kg m-2 s-1 for all pr units. 
#'
#'------Parameters related to the internal masking options-----
#'@param opt.wetday: The kind of threshold to apply. Currently accepts
#''us_trace' (0.01 in/day), 'global_trace' (0.1 mm/day), 'zero'
#'(no precipitation), and a user-generated option of the same units
#'as preciptation (i.e. 0.5). If units provided are NULL (as may be the 
#'case for synthetic data), will only accept 'zero' or a user-defined
#'threshold.
#'@param lopt.drizzle : The option for applying a drizzle adjustment. 
#'Converts the parameter for trace precipitation in the adjusted data
#'to the 
#'@param lopt.conserve: The option for preserving trace precipitation.
#'If present, the total amount of precipitation is conserved in the input 
#'datasets; missing trace precipitation is averaged over the days with
#'precipitation greater than the threshold.
#'@param zero.to.na: Converts days with precipitation below the threshold
#'to NA instead of 0, so that downscaling only takes place on those days 
#'with precipitation greater than the trace. Days are converted back to 0
#'after downscaling based on the future predictor (CF, MF) mask during
#'post-downscaling adjustment.
#'
#'TODO: Add *major* lat/lon coordiante agreement QC checks
#'

AdjustWetdays <- function(ref.data, ref.units=NULL, 
                          adjust.data=NA, adjust.units=NULL, 
                          future.data=NA, future.units=NULL,
                          opt.wetday, lopt.drizzle=FALSE, lopt.conserve=FALSE,
                          zero.to.na=FALSE, adj.method='none',
                          lopt.graphics=FALSE, verbose=FALSE){
  
  #Convert the threshold and create the reference mask
  wetday.convert <- convert.pr.threshold(opt.wetday, ref.units)
  ref.wetdays <- ref.data > wetday.convert
#   print(ref.wetdays[7890:7896])
#   print(ref.data[7890:7896])
  
  #Initialize the adjusted reference vectors
  if(verbose){print('Initializing reference vectors')}
  ###hastily commented out 9-24 to examine the data in more detail
  adjust.wetdays <- MaskPRSeries(adjust.data, adjust.units, opt.wetday)
  future.wetdays <- MaskPRSeries(future.data, future.units, opt.wetday)
  
  #   adjust.wetdays <- adjust.data > wetday.convert
  #   future.wetdays <- future.data > wetday.convert
  
  ##Loop over all lat/lon points available in the input datasets
  if(verbose){print(paste('Looping over data of dimensions', paste(dim(ref.data), collapse=" ")))}
  
  #  for (i in 1:dim(ref.data)[1]){
  #    for (j in 1:dim(ref.data)[2]){ 
  ##Okay, try this based off of reference indexes instead - 
  #no explict gather step
  ref.idx <- !is.na(ref.data)
  adj.idx <- !is.na(adjust.data)
  fut.idx <- !is.na(future.data)
  #also need to get roughly where ensembles are located in future
#   fut.which.idx <- array(1:length(future.data), dim=dim(future.data))
#   fut.which.idx <- apply()
  loop.ref <- ref.data[ref.idx]
  loop.adj <- adjust.data[adj.idx]
  loop.fut <- future.data[fut.idx]
  loop.ref.wetdays <- ref.wetdays[ref.idx]
  loop.adj.wetdays <- adjust.wetdays[adj.idx]
  loop.fut.wetdays <- future.wetdays[fut.idx]
  #...honestly, looking at the rest of the code the vector compression steps might be worth it
  #I think there is a high likelihood that you need to do the mask looping in here; 
  #the altered pr wetday behavior in the missing value functions might require it
  
#initializing adj.thold so we can return regardless
adj.thold = wetday.convert
if (lopt.drizzle == TRUE  && any(ref.idx)){  #Avoid running if all NA values present
  fraction.wet.ref <- sum(loop.ref.wetdays) / length(loop.ref.wetdays)
  fraction.wet.adj <- sum(loop.adj.wetdays) / length(loop.adj.wetdays)
  
  # Perform the following calculations and adjustments to the GCM time series
  # only if the user has asked for the drizzle adjusment to be applied 
  if (fraction.wet.adj > fraction.wet.ref) {
    message('adjusting MH/MF threshold')
    #Find the threshold for a drizzle adjustment
    #adj.method="orig"
    adj.thold <- switch(adj.method, 
                        "approx" = find.thold.orig(fraction.wet.ref, loop.ref, loop.adj), 
                        "exact"  = find.thold.kd(fraction.wet.ref, loop.ref, loop.adj))
    
    #If it would result in changed results, apply
    loop.adj.wetdays <- adjust.data[adj.idx] > adj.thold
    loop.fut.wetdays <- future.data[fut.idx] > adj.thold
    
  }
} else {
  print('wetday convert:')
  print(wetday.convert)
  loop.adj.wetdays <-    adjust.data[adj.idx] > wetday.convert
  loop.fut.wetdays <-    future.data[fut.idx] > wetday.convert
}

#   ###Now consider the conserve option
#   if(lopt.conserve==TRUE){
#     loop.ref <- conserve.prseries(loop.ref, loop.ref.wetdays, pool_ens=T) 
#     loop.adj <- conserve.prseries(loop.adj, loop.adj.wetdays, pool_ens=T)
#     loop.fut <- conserve.prseries(loop.fut, loop.fut.wetdays, pool_ens=T)
#   } 
  ref.data[ref.idx] <- loop.ref
  # loop.ref.wetdays never gets modified 
  adjust.data[adj.idx] <- loop.adj 
  adjust.wetdays[adj.idx] <- loop.adj.wetdays 
  future.data[fut.idx] <- loop.fut
  future.wetdays[fut.idx] <- loop.fut.wetdays
  ###Now consider the conserve option
  if(lopt.conserve==TRUE){
    ref.data <- conserve.prseries(ref.data, ref.wetdays, pool_ens=T) 
    adjust.data <- conserve.prseries(adjust.data, adjust.wetdays, pool_ens=T)
    if(!is.null(dim(future.data))){
      nrips <- dim(future.data)[4]
      for(n in 1:nrips){
        future.data[,,,n,,] <- conserve.prseries(future.data[,,,n,, ,drop=F],
                                                 future.wetdays[,,,n,, ,drop=F], pool_ens=T)
      }
    }else{
      future.data <- conserve.prseries(future.data, future.wetdays, pool_ens=T)
    }
  } 
print("adjusted.thold:")
print(adj.thold)
  #Return both data and the mask of adjustments made to that data
  out.list <- list("ref" = list("data"=as.numeric(ref.wetdays)*ref.data, "pr_mask"=ref.wetdays), 
                   "adjust" = list("data" = as.numeric(adjust.wetdays)*adjust.data, "pr_mask" = adjust.wetdays), 
                   "future" = list("data" = as.numeric(future.wetdays)*future.data, "pr_mask" = future.wetdays), 
                   'adjusted.thold' = adj.thold)
  if(zero.to.na){
    #All 0 values in the data should be NA values instead (needed to avoid calculations in )
    out.list$ref$data[out.list$ref$pr_mask==0] <- NA
    out.list$adjust$data[out.list$adjust$pr_mask==0] <- NA
    out.list$future$data[out.list$future$pr_mask==0] <- NA
  }
  return(out.list)
}

conserve.prseries <- function(data, mask, pool_ens=F){
  #Conserves total mass of precipitation for a timeseries
  #by summing all precipitation below the trace threshold
  #and dividing the result over all days for which pr was
  #greater than the threshold.
  #Edited 11-14-2016 to account for multi-ensemble data - 
  #unless we are doing something interesting with pooling, 
  #conserve opts should not conserve over ensembles.
  if(is.null(dim(data))){
    total.trace.pr <- sum(data[mask==FALSE], na.rm=T)
    if(total.trace.pr > 0){
      pr.adjust <- total.trace.pr/sum(mask, na.rm=T)
      data[mask==TRUE]  <- data[mask==TRUE] + pr.adjust
    }else{
      #Do nothing (justified by a few papers that should be cited later)
    }
  }else{
    #If dims are not null, assume 6-d input structure
    #in which case, conserving over nonspatial points is probably a 
    #bad idea
    data.dim <- dim(data)
    print(paste("dims of data in conserve.prseries:", paste(data.dim, collapse=",")))
    for(x in 1:data.dim[1]){
      for(y in 1:data.dim[2]){
        loop.data <- data[x,y,,,,]
        loop.mask <- mask[x,y,,,,]
        total.trace.pr <- sum(loop.data[loop.mask==FALSE], na.rm=T)
        if(total.trace.pr > 0){
          pr.adjust <- total.trace.pr/sum(loop.mask, na.rm=T)
          loop.data[loop.mask==TRUE]  <- loop.data[loop.mask==TRUE] + pr.adjust
        }else{
          #Do nothing (justified by a few papers that should be cited later)
        }
        data[x,y,,,,] <- loop.data
      }
    }
  }
  return(data)
}


MaskPRSeries <- function(data, units, index){
  #'Converts all values within the dataset
  #'less than the threshold to an NA, 
  #'depending upon the argument passed. 
  #'Requires the Udunits2 package to 
  #'work. 
  print(paste("Entering masking function with threshold", index))
  units <- units.CF.convert(units)
  #Set options for determining what qualifies as a 'wet day'
  zero.thold = 0
  if(!is.null(units)){
    us.trace.thold = ud.convert(0.01, "inches/day", units)
    global.trace.thold = ud.convert(0.1, "mm/day", units)
  }else{
    if (index=='us_trace'||index=='global_trace'){
      stop(paste("Error in MaskPRSeries: argument units was null,", 
                 "and is required for calculating index", index))
    }
  }
  switch(index, 
         'zero' = return(data > zero.thold),
         'us_trace' = return(data > us.trace.thold), 
         'global_trace' = return(data > global.trace.thold), 
         return((data > index))) #If own threshold supplied,
  #It is assumed that you know the units  
}

convert.pr.threshold <- function(index, units){
  #'Converts a threshold to the units of the input
  #'dataset. 
  #'Requires the Udunits2 package to work. 
  #stop('this is where the errors are')
  if(!is.null(units)){
    units <- units.CF.convert(units)
    #Set options for determining what qualifies as a 'wet day'
    zero.thold = 0
    us.trace.thold = ud.convert(0.01, "inches/day", units)
    global.trace.thold = ud.convert(0.1, "mm/day", units)
  }else{
    #Null case can pop up in synthetic data and should be dealt with
    if (index=='us_trace'||index=='global_trace'){
      stop(paste("Error in MaskPRSeries: argument units was null,", 
                 "and is required for calculating index", index))
    }else{
      #Zero is only constant that needs to be set
      zero.thold=0
    }
  }
  switch(index, 
         'zero' = return(zero.thold + (2 * .Machine$double.eps)),
         'us_trace' = return(us.trace.thold), 
         'global_trace' = return(global.trace.thold), 
         return((index))) #If own threshold supplied,
  #It is assumed that you know the units  
}

replace.vals<-function(series, tf.index){
  #Replaces all values for which the index is
  #true with a NA
  out<- series
  out[tf.index==TRUE]<-NA
  return(out)
}

units.CF.convert <- function(unit.string){
  #Performs simple conversions needed between CF-compliant
  #units and the udunits2 package. 
  unitlist<-sub('kg m-2 s-1', "l m-2 s-1", unit.string)
  #unitlist<-sub('C', "°C", unitlist)
}
