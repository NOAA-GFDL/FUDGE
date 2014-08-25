TrainDriver <- function(istart,loop.start,loop.end,target.masked.in,hist.masked.in,fut.masked.in,ds.method,k=0,time.steps){
#' Function to loop through spatially,temporally and call the Training guts. 
#' @param  loop.start: J loop start index
#' @param loop.end: J loop end index
#' @param input masked datasets 
#' @param ds.method: name of the downscaling method 


#### this example is for applying CDFT (no cross-validation,K=0)

# Initialize ds.vector 
ds.vector =  array(NA,dim=c(istart,loop.end,time.steps))

#TODO cew,a1r temporal mask application, xval function merging

#### Loop(1) through J subset ######################### 
#TODO loop.start,loop.end could be derived from mask lat dimension
for(jindex in loop.start:loop.end){ 
  if(!is.na(target.masked.in[1,jindex,1])){
#### Loop(2) through time domain (TODO call ApplyTemporal Masks). eg month-wise looping/downscaling  #### 
#### Loop(3) TODO  k-fold xval loop through k-cases -- call x-val function, determine dependant,independant samples to be passed   ##########
    if(grepl('CDFt', ds.method)){
      list.CDFt.result <- CDFt(target.masked.in[1,jindex,],hist.masked.in[1,jindex,],fut.masked.in[1,jindex,],npas = npas)
      ds.vector[istart,jindex,] <- list.CDFt.result$DS
      }else{
      stop("Method not supported yet")
      }
  }
}
####### Loop(1) ends ###################################
return(ds.vector)
############## end of TrainDriver.R ############################
}
