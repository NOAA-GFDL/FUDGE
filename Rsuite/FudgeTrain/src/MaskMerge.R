#MaskMerge.R

#Merges two (or more) masked files of the same dimensions together into a single series.
#Masked dataseries should be passed to the function as a list.
#Optional argument flag "collide" checks for collisions if set to true.
MaskMerge <- function(args, collide=FALSE){
  #Assume that all files have the same dimension, and store it for later.
  #Depracated in this version because nothing has had a dim so far, and nothing probably ever will.
#   output.dim <- dim(args[[1]])
#   if(is.null(output.dim)){
#     merged.series <- rep(0, length(args[[1]]))
#   }else{
#   merged.series <- rep(0, prod(output.dim))
#   }
  merged.series <- rep(0, length(args[[1]]))
  if(collide==TRUE){
    checkvec<-rep(0, length(merged.series))
  }
  #Enter main masking loop
  for (i in 1:length(args)){
    print(paste("merging mask", i, "of", length(args)))
    dim(args[[i]]) <- c(length(args[[i]]))
    if(length(args[[i]])!=length(merged.series)){
      stop(paste("Masked dataset merging error: masked dataset", names(args[i]), "was of length",
                 length(args[[i]]), "not expected length of", length(merged.series)))
    }
    merged.series <- merged.series + convert.NAs(args[[i]])
    if(collide==TRUE){
      checkvec <- checkvec + create.checkvector(args[[i]])
    }
  }
  if(collide==TRUE && sum(checkvec)!=length(merged.series)){
    if(sum(checkvec) > length(merged.series)){
      stop(paste("Mask collision error: the vectors specified as args to MaskMerge collide in at least", 
                 sum(checkvec)-length(merged.series),"places"))
    }
  }
  return(merged.series)
}


#Converts NAs to 0
#and returns the result in a 1-D form
convert.NAs<-function(dataset){
  dataset2<-dataset
  dataset2[is.na(dataset)]<-0
  return(as.vector(dataset2))
}

#Converts NAs to 0, all non-NA values to 1
#and returns the result in a 1-D form
create.checkvector<-function(dataset){
  dataset2<-dataset
  dataset2[is.na(dataset)]<-0
  dataset2[!is.na(dataset)]<-1
  return(as.vector(dataset2))
}