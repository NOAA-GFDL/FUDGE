
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
  data <- unlist(as.vector(data))
  return(data)
}

index.delta <- function(hist_leapdays, fut_leapdays, 
                        hist_mask, fut_mask, 
                        data, 
                        hist_year_div, fut_year_div){
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
  if (hist_year_div[1] == 0){
    #synthetic data check
    return(data)
  }else{
    out <- rep(NA, length(tmasks$MF$masks[[w]]))
    if (!is.null(hist_mask)){
      hist_days <- (!is.na(hist_leapdays) & !is.na(hist_mask))
    }else{
      hist_days <- !is.na(hist_leapdays)
    }
    if (!is.null(fut_mask)){
      fut_days <- (!is.na(fut_leapdays) & !is.na(fut_mask))
    }else{
      fut_days <- !is.na(fut_leapdays)
    }
    hist.which <- 1:length(hist_days)
    fut.which <- 1:length(fut_days)
    for (n in 1:length(fut_year_div)){
      if (n == 1){
        #oh_segment <- hist.which[c(hist_days[1 : hist_year_div[n]])]
        oh_segment <- hist.which[1:hist_year_div[n]]
        mf_segment <- fut.which[1:fut_year_div[n]]
      }else{
        oh_segment <- hist.which[c( (hist_year_div[n-1] + 1) : hist_year_div[n])]
        mf_segment <- fut.which[c( (fut_year_div[n-1] + 1) : fut_year_div[n] )]
      }
      #The two mask length segments should be the same length
      #initialize mapped.data based off of length of mf
      mapped.data <- rep(NA, times = length(mf_segment))
      if (is_leapyear(mf_segment) & !is_leapyear(oh_segment)){
        #This is now the leapday repetition step - as of 2017-02-14, 
        #leapdays are only repeated when there is a conflict between the
        #leapday modulo in the historical and future.
        oh_segment <- c(oh_segment[1:59], oh_segment[59], oh_segment[60:365])
        mapped.data <- data[oh_segment]
        
      }else if (is_leapyear(oh_segment) & !is_leapyear(mf_segment)){
        #mf_segment <- c(mf_segment[1:59], NA, mf_segment[60:365])
        oh_segment <- c(oh_segment[1:59], oh_segment[61:366])
        mapped.data <- data[oh_segment]
      }else{
        #They are exactly the same length, there is no need to worry about 
        #where the pre-masked leapdays are located
        mapped.data <- data[oh_segment]
      }      
      #Both segments map to a year of the MF data
      if (n == 1){
        out[ c(1 : fut_year_div[1])] <- mapped.data
      }else{
        out[ c((fut_year_div[n-1] + 1) : fut_year_div[n]) ] <- mapped.data
      }
      #stop('examine this output')
    }
  }
  #   #Repeat Feb.28 for Feb. 29
  #   fut_leapdays <- which(is.na(fut_leapdays))
  #   out[fut_leapdays] <- out[fut_leapdays-1]
  return(out)
}

#Checks for the length of the leapyear segment in the MF workflow
is_leapyear <- function(x){length(x) == 366}


# index.delta <- function(hist_leapdays, fut_leapdays, hist_mask, fut_mask, data){
#   #Delta indexing is complicated - it needs to correspond to a future timeseries,
#   #but downscaling is done with OH. Thus, we repeat the OH indexing/data
#   #to make up the length of MF, repeating leapdays as they were present
#   #in the MF data.
#   ###This assumes that you feed in a single ensemble of data at a time
#   out <- rep(NA, length(tmasks$MF$masks[[w]]))
#   hist_days <- (!is.na(hist_leapdays) & !is.na(hist_mask))
#   fut_days <- (!is.na(fut_leapdays) & !is.na(fut_mask))
#   fut_leapdays <- which(is.na(fut_leapdays))
#   day_diff <- sum(hist_days) - sum(fut_days)
#   if (day_diff != 0){
#     if (w == 1){
#       print(length(hist_days))
#       print(length(fut_days))
#       print("hist and fut non-leap day count:")
#       print(sum(hist_days))
#       print(sum(fut_days))
#       print(rle(as.numeric(hist_days)))
#       print(rle(as.numeric(fut_days)))
#       #This used to be an error; now kept as a warning
#       warning(paste('Warning in index.delta! number of present days in', 
#                     'future and historical samples', 
#                     'did not match!'))
#     }
#   }
#   #out[fut_days] <- data[hist_days]
#   # 2018-02-06:
#   # The above does not work for the year cycles that we are interested in.
#   # Using the below approach to keep those cases consistent.
#   #out[hist_days] <- data[hist_days]
#   #out[!(fut_days)] <- NA #essentially time-trimming mask
#   #... I think the above approach was problematic when working with March
#   # of Julian calendars. I *think* this approach is going to work better,
#   # but I need to check on some of the details.j
#   num.fut <- sum(fut_days)
#   which.hist <- which(hist_days)[1:num.fut]
#   out[fut_days] <- data[which.hist]
#   #Repeat Feb.28 for Feb. 29
#   out[fut_leapdays] <- out[fut_leapdays-1]
#   return(out)
# }

index.oh.qm <- function(index.mask, data, olap=T){
  #For quantile mapping methods, there is a chance that you are
  #downscaling on an overlapping mask, which needs to be trimmed
  #back down to size.
  if (olap){
    data <- apply.any.mask(data=data, mask=index.mask)
  }
  return(data)
}
