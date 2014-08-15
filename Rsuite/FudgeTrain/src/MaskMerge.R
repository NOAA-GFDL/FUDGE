#MaskMerge.R

#Merges two (or more) masked files of the same dimensions together into a single series.
#Masked dataseries should be passed to the function as a list.
MaskMerge <- function(args){
  #Assume that all files have the same dimension, and store it for later.
  output.dim <- dim(args[[1]])
  if(is.null(output.dim)){
    merged.series <- rep(0, length(args[[1]]))
  }else{
  merged.series <- rep(0, prod(output.dim))
  }
  for (i in 1:length(args)){
    print(paste("merging mask", i, "of", length(args)))
    dim(args[[i]]) <- c(length(args[[i]]))
    if(length(args[[i]])!=length(merged.series)){
      stop(paste("Masked dataset merging error: masked dataset", names(args[i]), "was of length",
                 length(args[[i]]), "not expected length of", length(merged.series)))
    }
    merged.series <- merged.series + convert.NAs(args[[i]])
  }
  return(merged.series)
}


#Converts NAs to 0, and all non-NA values to 1
#and returns the result in a 1-D form
convert.NAs<-function(dataset){
  dataset2<-dataset
  dataset2[is.na(dataset)]<-0
  return(as.vector(dataset2))
}