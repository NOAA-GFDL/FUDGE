

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
  #print(stencil)
  if (stencil_name != "none" && length(stencil) > 1){
    print("applying stencil mask")
    tmp <- apply.any.mask(mask=stencil, data=data, 'spatial')
    return(tmp)
  }else{
    return(data)
  }
}

get.prime.predictor <- function(prime_var, aux.ref, ...){
  #Given a var of interest and a list of stencils, 
  #reads in the prime predictor from the stencil list.
  # (i.e. the predictor that is identified with a 3).
  #If stencil=="none", then the singular var point is returned.
  #Check to make sure MH and MF are identical
  if (aux.ref$MH$stencil != aux.ref$MF$stencil){
    stop(paste("Error in get.prime.predictor: MH and MF stencils were not identical!"))
  }
  print("in stencil")
  prime.stencil <- aux.ref$MF$stencil
  print(prime.stencil)
  if (prime.stencil != "none"){
    print(prime.stencil)
  prime.stencil[prime.stencil != 3] <- NA
  tmp.list <- list()
  tmp.list[[prime_var]] <- prime.stencil
  tmp.data <- compress.xy.data(tmp.list)
  if (length(tmp.data) == 0){
    stop(paste("Error in get.prime.predictor: var", prime_var, 
               "does not have a prime predictor (3) specified in the stencil!"))
  }
    return(names(tmp.data))
  }else{
    return(paste0(prime_var, ".0.0"))
  }
}

FUDGE.error.function <- function(){
  #'Prints out information needed for debugging calls from FUDGE
  #'in the production workflow - namely, your ival, jval, and twindow coords.
  
  cat(paste("ival:", ival), "\n")
  cat(paste("jval:", jval), '\n')
  cat(paste("w:", w), '\n')
  
  print(str(as.list(eval('match.call', parent.frame(n=2)))))
  if (!interactive()){
    on.exit(traceback(2)) #skips first two entries on the stack (i.e. call to tb)
    q('no', 1, FALSE)
  }
}
