

apply.stencil <- function(data, stencil, stencil_name){
  #This is basically a convenience wrapper for the 
  #apply.any.mask function. There's a chance that we are going to
  #be doing more complicated things with the masks in the future, 
  #however, and that probably merits a separate function.
  
#  Not doing this here anymore; do it when reading in the stencil
#   #Convert 0 to NA on the stencil
#   stencil[stencil==0] <- NA
#   #convert any 3s to 1
#   stencil[stencil==3] <- 1
  print(stencil)
  if (stencil_name != "none" && length(stencil) > 1){
    print("applying stencil mask")
    tmp <- apply.any.mask(mask=stencil, data=data, 'spatial')
    print(summary(tmp))
    return(tmp)
  }else{
    return(data)
  }
}