#MaskMerge.R
#Carolyn Whitlock, August 2014

#' MaskMerge
#' Merges two (or more) masked files of the same dimensions together into a single series, 
#' returning errors if the data collides and the 'collide' tag is set to FALSE
#' @param args: a list of one or more vectors of data to be merged. Returns an error if the
#' vectors are not of the same length.
#' @param collide=FALSE : whether or not to check for collisions between the data to be merged.
#' Defaults to FALSE. 
#' @examples 
#' k0 <- CrossValidate(train_predictor, train_target, esd_gen, 0, "ESD.Train.totally.fake")
#' k4 <- CrossValidate(train_predictor, train_target, esd_gen, 4, "ESD.Train.totally.fake")
#' @references \url{link to the FUDGE API documentation}
#' 
MaskMerge <- function(args, collide=FALSE){
 
  merged.series <- rep(NA, length(args[[1]]))
  if(collide==TRUE){
    checkvec<-rep(0, length(merged.series))
  }
  #Enter main masking loop
  for (i in 1:length(args)){
    if (i%%10==0 || i==1){
      print(paste("merging mask", i, "of", length(args)))
    }
    dim(args[[i]]) <- c(length(args[[i]]))
    if(length(args[[i]])!=length(merged.series)){
      stop(paste("Masked dataset merging error: masked dataset", i, "named", names(args[i]), "was of length",
                 length(args[[i]]), "not expected length of", length(merged.series)))
    }
    merged.series <- MergeSeries(args[[i]],merged.series)
    ####Commented out (but useful for debugging)
#     print(paste("summary of the", i, "th argument into the merging function:"))
#     print(summary(args[[i]]))
#     print(paste("the length of the merged series is", length(merged.series)))
    if(collide==TRUE){
#      print("Creating check vector")
      checkvec <- checkvec + create.checkvector(args[[i]])
    }
  }
  if(collide==TRUE && sum(checkvec)!=length(merged.series)){
    if(sum(checkvec) > length(merged.series)){
      stop(paste("Mask collision error: the vectors specified as args to MaskMerge collide in at least", 
                 sum(checkvec)-length(merged.series),"places"))
    }
  }
#   print("summary of the merged data:")
#   print(summary(merged.series))
  return(merged.series)
}


#Adds two numbers, replacing NA with a value
#if only one val is NA, and keeping the NA
#if both values are NA
#and returns the result in a 1-D form
MergeSeries<-function(data, merge.data){
  for (i in 1:length(data)){
    if(is.na(data[i])&&is.na(merge.data[i])){
      merge.data[i]<-NA
      ####Commented out, but useful for debugging
      #     }else if (!is.na(data[i]) && !is.na(merge.data[i])){
      #       stop(paste("Merge collision error in index", i))
    }
    else{
      merge.data[i] <- sum(c(data[i], merge.data[i]), na.rm=TRUE)
      }
  }
  return(merge.data)
}

#Converts NAs to 0, all non-NA values to 1
#and returns the result in a 1-D form
create.checkvector<-function(dataset){
  dataset2 <- dataset
  dataset2[is.na(dataset)]<-0
  dataset2[!is.na(dataset)]<-1
  return(dataset2)
}