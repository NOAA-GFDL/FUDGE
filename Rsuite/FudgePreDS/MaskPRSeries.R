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
                          zero.to.na=FALSE,
                          lopt.graphics=FALSE, verbose=FALSE){
  
  #Convert the threshold and create the reference mask
  wetday.convert <- convert.threshold(opt.wetday, ref.units)
  ref.wetdays <- ref.data > wetday.convert
  print(ref.wetdays[7890:7896])
  print(ref.data[7890:7896])
  
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
  loop.ref <- ref.data[ref.idx]
  loop.adj <- adjust.data[ref.idx]
  loop.fut <- future.data[fut.idx]
  loop.ref.wetdays <- ref.wetdays[ref.idx]
  print(loop.ref.wetdays[7890:7896])
  print(loop.ref[7890:7896])
  loop.adj.wetdays <- adjust.wetdays[adj.idx]
  loop.fut.wetdays <- future.wetdays[fut.idx]
  print(length(loop.ref))
  print(length(ref.idx))
  print(summary(ref.idx))
  print(sum(ref.idx))
  #...honestly, looking at the rest of the code the vector compression steps might be worth it
  #I think there is a high likelihood that you need to do the mask looping in here; 
  #the altered pr wetday behavior in the missing value functions might require it
  
  
  if (lopt.drizzle == TRUE  && any(ref.idx)){  #Avoid running if all NA values present
    fraction.wet.ref <- sum(loop.ref.wetdays) / length(loop.ref.wetdays)
    fraction.wet.adj <- sum(loop.adj.wetdays) / length(loop.adj.wetdays)
    
    # Perform the following calculations and adjustments to the GCM time series
    # only if the user has asked for the drizzle adjusment to be applied 
    if (fraction.wet.adj > fraction.wet.ref) {
      #Find the threshold for a drizzle adjustment
      first.above.threshold <- quantile(loop.ref,  probs=(1.0-fraction.wet.ref),na.rm=TRUE)
      print(first.above.threshold)
      small.fraction <- 1.0/sum(ref.idx)
      last.below.threshold <- quantile(loop.ref, probs=(1.0-fraction.wet.ref-small.fraction), na.rm=TRUE)
      num.zero.in.adjusted <- ((1.0-fraction.wet.ref)*sum(adj.idx)) - 1
      print(num.zero.in.adjusted)
      threshold.wetday.adj <- quantile(loop.adj, names = FALSE,
                                       probs=(num.zero.in.adjusted/sum(adj.idx)), 
                                       na.rm=TRUE)
      if(verbose){print(paste("threshold:", threshold.wetday.adj))}
      #If it would result in changed results, apply
      loop.adj.wetdays <- adjust.data[adj.idx] > threshold.wetday.adj
      loop.fut.wetdays <- future.data[fut.idx] > threshold.wetday.adj
    }
  } else {
    print('wetday convert:')
    print(wetday.convert)
    loop.adj.wetdays <-    adjust.data[adj.idx] > wetday.convert
    loop.fut.wetdays <-    future.data[fut.idx] > wetday.convert
  }
  ###Now consider the conserve option
  if(lopt.conserve==TRUE){
    loop.ref <- conserve.prseries(loop.ref, loop.ref.wetdays) 
    loop.adj <- conserve.prseries(loop.adj, loop.adj.wetdays)
    loop.fut <- conserve.prseries(loop.fut, loop.fut.wetdays)
  } 
  ref.data[ref.idx] <- loop.ref
  print(ref.data[7890:7896])
  print(ref.wetdays[7890:7896])
  print((as.numeric(ref.wetdays)*ref.data)[7890:7896])
  # loop.ref.wetdays never gets modified 
  adjust.data[adj.idx] <- loop.adj 
  adjust.wetdays[adj.idx] <- loop.adj.wetdays 
  future.data[fut.idx] <- loop.fut
  future.wetdays[fut.idx] <- loop.fut.wetdays
  #Return both data and the mask of adjustments made to that data
  out.list <- list("ref" = list("data"=as.numeric(ref.wetdays)*ref.data, "pr_mask"=ref.wetdays), 
                   "adjust" = list("data" = as.numeric(adjust.wetdays)*adjust.data, "pr_mask" = adjust.wetdays), 
                   "future" = list("data" = as.numeric(future.wetdays)*future.data, "pr_mask" = future.wetdays))
  if(zero.to.na){
    #All 0 values in the data should be NA values instead (needed to avoid calculations in )
    out.list$ref$data[out.list$ref$pr_mask==0] <- NA
    out.list$adjust$data[out.list$adjust$pr_mask==0] <- NA
    out.list$future$data[out.list$future$pr_mask==0] <- NA
  }
  return(out.list)
}

conserve.prseries <- function(data, mask){
  #Conserves total mass of precipitation for a timeseries
  #by summing all precipitation below the trace threshold
  #and dividing the result over all days for which pr was
  #greater than the threshold.
  total.trace.pr <- sum(data[mask==FALSE])
  if(total.trace.pr > 0){
    pr.adjust <- total.trace.pr/sum(mask)
    data[mask==TRUE]  <- data[mask==TRUE] + pr.adjust
  }else{
    #Do nothing (justified by a few papers that should be cited later)
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

convert.threshold <- function(index, units){
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
         'zero' = return(zero.thold),
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
