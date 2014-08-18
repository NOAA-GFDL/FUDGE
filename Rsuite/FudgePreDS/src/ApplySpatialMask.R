#'Applies spatial masks to a 3-dimensional dataset (x, y, time)
#' @param data: a 3-dimensional dataset with final dimension representing time
#' @param masknc: a path to an existing ncdf file containing a spatial mask of 
#' the same (or containing) dimensions as the data being masked. CF formatting is
#' assumed (existance of lon and lat dimensions)
#' @param maskname
#' @param dataLon
#' @param dataLat
#' @return A 3-dimensional array of the same dimensions as the input data, with all x and y-coordinates where
#' the mask contained a "NA" replaced with a "NA".
#' @example insert example here
#' @references \url{link to the FUDGE API documentation}
ApplySpatialMask<-function(data, masknc, maskname="spatial_mask", dataLon, dataLat){
  data_dim<-dim(data)
  masknc<-nc_open(masknc) #Assume that mask is a valid path to a formatted mask
  if (!identical(c(length(masknc$dim$lon$vals), length(masknc$dim$lat$vals)), data_dim[1:2])){
    print("Locating matching x-y coordiantes in data and mask")
    startLon <- match(dataLon[1], masknc$dim$lon$vals)
    startLat <- match(dataLat[1], masknc$dim$lat$vals)
    lonLength <- length(dataLon)
    latLength <- length(dataLat) #Note: this method assumes that grid steps will be the same. Valid? For now.
  }else{
    startLon <- 1
    startLat <- 1
    lonLength <- masknc$dim$lon$len
    latLength<- masknc$dim$lat$len
  }
  mask<-ncvar_get(masknc, maskname, 
                  start=c(startLon, startLat), count=c(lonLength, latLength), collapse_degen=FALSE)
  nc_close(masknc)
  #message("debug")
  #message(length(mask[1,]))
  #message(length(data[1,,1]))
  #message(length(mask[,1]))
  #message(length(data[,1,1])) 
  if(length(mask[1,])!=length(data[1,,1])||length(mask[,1])!=length(data[,1,1])){
    stop(paste(".Spatial mask dimension error: mask was of dimensions", dim(mask), 
               "and was expected to be of dimensions", data_dim[1:2]))
  }
  return(matrimult(data, mask))  
}

#Multiplies the lat./lon. mask by the spatial data at each timestep
#Assumes a 3-D matrix of original data.
#This is a strictly internal method, so it shouldn't need the lovely
#roxygen documentation
matrimult<-function(mat,n){
  message("matrimult starts")
  ret<-mat
  timedim<-dim(mat)[3]
  for (i in 1:timedim){
    ret[,,i]<-mat[,,i]*n
  }
  message("matrimult ends")
  return(ret)
}
