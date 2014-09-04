#'K.FoldMasker
#'Takes a timeseries or an integer representing a timeseries length
#'and returns a set of k masks that each partition the vector
#'into a set of length length(vector)/(k-1) and a set of length
#'length(vector)/1. In the event of a length(predictor)%%k!=0, 
#' the remainder is passed to the first section for which the
#' decimal round to 1; if the byyear flag is set to TRUE (its default), 
#' the timeseries vector passed will be used to make sure that
#' no kfold masks subset a year.
#' Yes, it's nonrandom. This way, it's reproducable.
#' @param timeseries: 
#' @param vector.length: 
#' @param k: 
#' @param byyear: defaults to TRUE, which requires the timeseries.
#' 
#' 
#' Note: was in the process of moditying to do teh crossvalidation by year
#' properly. Got distracted and told that this might not be all that important.
#' Left it to work on later.


K.FoldMasker<-function(timeseries, vector.length, k, byyear = TRUE){
  if (byyear){
    year.factor <- as.factor(strftime(timeseries,"%Y"))
    vector.length <- length(levels(year.factor))
  }
  p.index <- indices.calc(vector.length,k)
  temp <- rep(TRUE, vector.length)   #Obtain k vectors of p.len
  p.masks <- as.list(rep(list(temp), k))    #for which all values are true
  for (i in 1:k){
    p.masks[[i]][ (p.index[i]+1):p.index[i+1] ] <- FALSE #Set all values in the i-th partition
  } 
  if(byyear){
    
  }else{
                                                 #of the i-th mask to FALSE
    return(p.masks)
  }
}

indices.calc <- function(val, k){
  #Calculates indices of a vector
  #for subsetting a vector of length val
  #into k partitions of equal length
  #or as close as integers allow.
  ret<-rep(0,k)
  for (i in 1:k){
    ret[i+1] <- as.integer((val/k) * i)
  }
#   print(paste("indices over which to subset within the data:
#               "))
  return(ret)
}