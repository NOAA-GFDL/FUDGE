
index.simple <- function(tmasks, time_mask_overlap, ds){
  #Uses a simple time indexing - assumes that you are getting
  #back a non-overlapping set of points from DS that can map back
  #onto a future time series.
  return(ds)
}

index.qm <- function(index.mask, data, olap=T){
  #For quantile mapping methods, there is a chance that you are
  #downscaling on an overlapping mask, which needs to be trimmed
  #back down to size.
  if (olap){
    data <- apply.any.mask(data=data, mask=index.mask)
  }
  return(data)
}

index.delta <- function(tmasks, OH.idx, data){
  #Delta indexing is complicated - it needs to correspond to a future timeseries,
  #but downscaling is done with OH. Thus, you need to break the 
  #...okay - for the moment, assume that the OH indexing vector 
  #is returned as part of the downscaing function.
  #EURGH. Any masks that you get back from the training QC are going to have
  #completely different indexing.
  #So this may need to go within CallDSMethod instead, 
  #which implies that you pass the time masks there.
#  if (ds$orientation='OH'){
#    oh.idx <- ds$oh.idx 
  ###This assumes that you feed in a single ensemble of data at a time
  out <- rep(NA, length(tmasks$MF$masks[[w]]))
  hist_days <- (!is.na(hist.leapday.mask[OH.idx]) & !is.na(tmasks$OH$mask[[w]][OH.idx]))
  fut_days <- (!is.na(fut.leapday.mask) & !is.na(tmasks$trim$mask[[w]]))
  fut_leapdays <- which(is.na(fut.leapday.mask))
  day_diff <- sum(hist_days) - sum(fut_days)
  if (day_diff != 0){
    print(length(hist_days))
    print(length(fut_days))
    print(sum(hist_days))
    print(sum(fut_days))
    print(rle(as.numeric(hist_days)))
    print(rle(as.numeric(fut_days)))
    stop('examine output of index.delta')
  }
  out[fut_days] <- data[hist_days]
  #Repeat Feb.28 for Feb. 29
  out[fut_leapdays] <- out[fut_leapdays-1]
  return(out)
}

reconcile.hist.fut.leapdays <- function(hist_mask, fut_mask, hist_data, fut_data){
  #Using pre-calcualted masks of Febrary 29ths, 
  #adds historical data to the same calendar pattern
  #as the future data, removing leapdays in hist when they
  #do ot synMH and doubling Feb. 28 when there are leapdays
  #in future not present in hist
  #Assumes that missing values have not been removed from vectors
  #and that your historic and future datasets can be resolved to the
  #same length
  out <- fut_data
  #get normal days in both datasets
  hist_days <- (!is.na(hist_mask) & !is.na(hist_data))
  fut_days <- (!is.na(fut_mask) & !is.na(fut_data))
  #select leapdays in future
  fut_leapdays <- which(is.na(fut_mask))
  day_diff = sum(hist_days) - sum(fut_days)
  if (day_diff!=0){
    #If there are uneven numbers of leap days in the sample
    stop('examine current output!')
    if (day_diff < 0){
      #if there are more nonleap days in future, repeat last sample in historical
      hist_data <- c(hist_data, tail(hist_data, abs(day_diff)))
    }else{
      #If there are fewer nonleap days in future, remove last sample in historical
      hist_data <- hist_data[1:(length(hist_data)-day_diff)]
    }
  }
  out[fut_days] <- hist_data[hist_days]
  #Repeat Feb.28 for Feb. 29
  out[fut_leapdays] <- out[(fut_leapdays-1)]
  return(out)
}