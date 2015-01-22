#'Applies spatial masks to a 3-dimensional dataset (x, y, time)
#' @param data: a 3-dimensional dataset with final dimension representing time
#' @param mask: a 2-dimensional dataset with first two dimensions representing x and y coordinates 
#' (most likely latitude and longitude)
#' @return A 3-dimensional array of the same dimensions as the input data, with all x and y-coordinates where
#' the mask contained a "NA" replaced with a "NA".
#' @example insert example here
#' @references \url{link to the FUDGE API documentation}
#' TODO: Type up specifications for input files, because there are 
#' some assumptions being made about what is and is not a mask
ApplySpatialMask<-function(data, mask){   #, maskname="spatial_mask", dataLon, dataLat
  #Assume a 2-D mask and 3-D data
  if(!is.null(mask)){
    if(length(mask[1,])!=length(data[1,,1])||length(mask[,1])!=length(data[,1,1])){
      stop(paste("Spatial mask dimension error: mask was of dimensions", dim(mask)[1], dim(mask)[2], 
                 "and was expected to be of dimensions", data_dim[1], data_dim[2]))
    }
    return(matrimult(data, mask))  
  }else{
    message("No spatial mask included; passing data as-is")
    return(data)
  }
}
#Multiplies the lat./lon. mask by the spatial data at each timestep
#Assumes a 3-D matrix of original data.
#This is a strictly internal method, so it shouldn't need the lovely
#roxygen documentation
matrimult<-function(mat,n){
  ret<-mat
  timedim<-dim(mat)[3]
  for (i in 1:timedim){
    ret[,,i]<-mat[,,i]*n
  }
  return(ret)
}