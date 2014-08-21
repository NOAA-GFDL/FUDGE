#MaskMerge.R

#Merges two (or more) masked files of the same dimensions together into a single series.
#Masked dataseries should be passed to the function as a list.
#Optional argument flag "collide" checks for collisions if set to true.
MaskMerge <- function(args, collide=FALSE){
 
  merged.series <- rep(NA, length(args[[1]]))
  if(collide==TRUE){
    checkvec<-rep(0, length(merged.series))
  }
  
#   plot(seq(1:50769), merged.series, type = "n", main="Where are the time windows on the fake masked data?")
  plot.colors <- rainbow(12)
  #Enter main masking loop
  for (i in 1:length(args)){
    print(paste("merging mask", i, "of", length(args)))
    dim(args[[i]]) <- c(length(args[[i]]))
    if(length(args[[i]])!=length(merged.series)){
      stop(paste("Masked dataset merging error: masked dataset", names(args[i]), "was of length",
                 length(args[[i]]), "not expected length of", length(merged.series)))
    }
    #print(args[[i]])
    merged.series <- MergeSeries(args[[i]],merged.series)
    print(paste("summary of the", i, "th argument into the merging function:"))
    print(summary(args[[i]]))
    print(paste("the length of the merged series is", length(merged.series)))
    if(collide==TRUE){
      print("Creating check vector")
      checkvec <- checkvec + create.checkvector(args[[i]])
    }
  }
  if(collide==TRUE && sum(checkvec)!=length(merged.series)){
    if(sum(checkvec) > length(merged.series)){
      stop(paste("Mask collision error: the vectors specified as args to MaskMerge collide in at least", 
                 sum(checkvec)-length(merged.series),"places"))
    }
  }
  print("summary of the merged data:")
  print(summary(merged.series))
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
    }else if (!is.na(data[i]) && !is.na(merge.data[i])){
      stop(paste("Merge collision error in index", i))
    }else{
      merge.data[i] <- sum(c(data[i], merge.data[i]), na.rm=TRUE)
    }
  }
  return(merge.data)
}

#Converts NAs to 0, all non-NA values to 1
#and returns the result in a 1-D form
create.checkvector<-function(dataset){
#   print(paste("mode of the dataset", mode(dataset)))
#   print(paste("number of NAs in dataset:", sum(is.na(dataset))))
#   print(paste("number of not-NAs in dataset:", sum(!is.na(dataset))))
  dataset2 <- dataset
  dataset2[is.na(dataset)]<-0
  dataset2[!is.na(dataset)]<-1
#   print(paste("sum of NAs present:", length(dataset2[dataset2==0])))
#   print(paste("Sum of NAs not present:", sum(dataset2)))
  return(dataset2)
}