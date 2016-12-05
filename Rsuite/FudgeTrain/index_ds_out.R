
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
    data <- apply.any.mask(data, index.mask, dim='time')
  }
  return(data)
}

index.delta <- function(hist_leapdays, fut_leapdays, hist_mask, fut_mask, data){
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
  hist_days <- (!is.na(hist_leapdays) & !is.na(hist_mask))
  fut_days <- (!is.na(fut_leapdays) & !is.na(fut_mask))
  fut_leapdays <- which(is.na(fut_leapdays))
  day_diff <- sum(hist_days) - sum(fut_days)
  if (day_diff != 0){
    print(length(hist_days))
    print(length(fut_days))
    print(sum(hist_days))
    print(sum(fut_days))
    print(rle(as.numeric(hist_days)))
    print(rle(as.numeric(fut_days)))
    stop(paste('Error in index.delta! number of present days in future and historical samples', 
               'did not match!'))
  }
  out[fut_days] <- data[hist_days]
  #Repeat Feb.28 for Feb. 29
  out[fut_leapdays] <- out[fut_leapdays-1]
  return(out)
}

index.oh.qm <- function(index.mask, data, olap=T){
  #For quantile mapping methods, there is a chance that you are
  #downscaling on an overlapping mask, which needs to be trimmed
  #back down to size.
  if (olap){
    data <- apply.any.mask(data=data, mask=index.mask)
  }
  return(data)
}