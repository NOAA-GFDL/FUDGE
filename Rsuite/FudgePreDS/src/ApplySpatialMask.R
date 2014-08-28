#'Applies spatial masks to a 3-dimensional dataset (x, y, time)
#' @param data: a 3-dimensional dataset with final dimension representing time
#' @param mask.nc.path: a path to an existing ncdf file containing a spatial mask of 
#' the same dimensions as the data being masked.
#' @return A 3-dimensional array of the same dimensions as the input data, with all x and y-coordinates where
#' the mask contained a "NA" replaced with a "NA".
#' @example insert example here
#' @references \url{link to the FUDGE API documentation}
#' TODO: Input error handling code for lack of valid mask names
#' TODO: Type up specifications for input files, because there are 
#' some assumptions being made about what is and is not a mask
ApplySpatialMask<-function(data, mask.nc.path){   #, maskname="spatial_mask", dataLon, dataLat
#  data_dim<-dim(data)
  mask.nc<-nc_open(mask.nc.path) #Assume that mask is a valid path to a formatted mask
  #And grab the first variable with a name that contains the word "mask"
  mask.var <- names(mask.nc$var)[which(regexpr(pattern="mask", names(mask.nc$var)) != -1)]
  if(is.null(mask.var)){
    stop(paste("Mask name error: no variable within the file", mask.nc.path, 
               "has a name that matches the pattern 'mask'. "))
  }
  
  #Furthermore, assument that this logic is unnessecary - there should be an
  #appropriately-formatted mask file present with only the lon/lat coords needed there
  
#   if (!identical(c(length(masknc$dim$lon$vals), length(masknc$dim$lat$vals)), data_dim[1:2])){
#     print("Locating matching x-y coordiantes in data and mask")
#     startLon <- match(dataLon[1], masknc$dim$lon$vals)
#     startLat <- match(dataLat[1], masknc$dim$lat$vals)
#     lonLength <- length(dataLon)
#     latLength <- length(dataLat) #Note: this method assumes that grid steps will be the same. Valid? For now.
#   }else{
#     startLon <- 1
#     startLat <- 1
#     lonLength <- masknc$dim$lon$len
#     latLength<- masknc$dim$lat$len
#   }
#   mask<-ncvar_get(masknc, maskname, 
#                   start=c(startLon, startLat), count=c(lonLength, latLength), collapse_degen=FALSE)
#   nc_close(masknc)
  mask <- ncvar_get(mask.nc, mask.var, collapse_degen=FALSE)
  nc_close(mask.nc)
  #Assume a 2-D mask and 3-=D data
  if(length(mask[1,])!=length(data[1,,1])||length(mask[,1])!=length(data[,1,1])){
    stop(paste("Spatial mask dimension error: mask was of dimensions", dim(mask)[1], dim(mask)[2], 
               "and was expected to be of dimensions", data_dim[1], data_dim[2]))
  }
  return(matrimult(data, mask))  
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
