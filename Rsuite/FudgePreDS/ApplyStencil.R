

apply.stencil <- function(data, stencil){
  #This is basically a convenience wrapper for the 
  #apply.any.mask function. There's a chance that we are going to
  #be doing more complicated things with the masks in the future, 
  #however, and that probably merits a separate function.
  
  #Convert 0 to NA on the stencil
  stencil[stencil==0] <- NA
  #convert any 3s to 1
  stencil[stencil==3] <- 1
  tmp <- apply.any.mask(mask=stencil, data=data, 'spatial')
  return(tmp)
}