#'CreateQCMask.R
#'
#'Creates a mask of the downscaled data for which a 1 means that the data
#'passes the QC check, and a 0 means that the data does not pass the QC
#'check. 
#'
#'@param data
#'@param qc.data
#'@param qc.test
#'
#'@returns A vector of values for the time series at the individual x,y, point
#'with 0 for all values that did not pass the test and 1 for all values that did.
#'

# CreateQCMask <- function(data, qc.data=NULL, qc.test='sdev', var='tasmax', 
#                          hist.pred=NULL, hist.targ=NULL, fut.pred=NULL, 
#                          time.window=NULL, time.data.window=NULL){
#   status=1
#   qc.mask <- data
#   #Loop by spatial coordiantes
#   for (i in 1:dim(data)[1]){
#     for (j in 1:dim(data)[2]){
#       print(paste("on coordinate i coord", i, "of", dim(data)[1], 
#                   'and j coord', j, "of", dim(data)[2], "."))
#       qc.mask[i,j,]<- QCDSValues(data[i,j,], qc.test=qc.test, # qc.data[i,j,],
#                                 hist.pred = hist.pred[i,j,], hist.targ = hist.targ[i,j,], fut.pred = fut.pred[i,j,], 
#                                 time.window = time.window, time.data.window = time.data.window, var='tasmax')
#     }
#   }
#   
#   return(qc.mask)
# }

QCDSValues<-function(data, qc.data=NULL, qc.test, hist.pred=NULL, hist.targ=NULL, fut.pred=NULL, 
                     var='tasmax',time.window=NULL, time.data.window=NULL){
  switch(qc.test, 
         'sdev' = return(callSdev(data, qc.data)),
         'sdev2' = return(callSdev2(data, qc.data)),
         'kdAdjust' = return(callKDAdjust(data, hist.pred, hist.targ, fut.pred)),
         stop(paste('QC Method Error: method', qc.test, 
                    "is not supported for CreateQCMask. Please check your input."))
  )
}

callSdev <- function(data, qc.data){
  #returns TRUE if more than half of the values in data
  #differ from qc.data by less than the standard deviation
  #of qc.data
  qc.stdev <- sd(qc.data, na.rm=FALSE)
  stdev.vec <- abs(qc.data-data) > qc.stdev
  print(sum(stdev.vec)/(length(data)/2))
  return( sum(stdev.vec) >= (length(data)/2) )
}

callSdev2 <- function(data, qc.data){
  #returns TRUE if more than half of the values in data
  #differ from qc.data by less than half the standard deviation
  #of qc.data
  qc.stdev <- sd(qc.data, na.rm=FALSE)
  stdev.vec <- abs(qc.data-data) > (qc.stdev/10)
  print(sum(stdev.vec)/length((data)/2))
  return( sum(stdev.vec) >= (length(data)/2) )
}

callKDAdjust <- function(data, hist.pred, hist.targ, fut.pred, var='tasmax', 
                            time.window=NULL, time.data.window=NULL){
  #Set corrective error factor: 
  if(var=='pr'){
    correct.factor <- 6e-04
  }else{
    correct.factor <- 6
  }

    #compute difference for all time values
    hist.bias <- mean(hist.pred-hist.targ)
    fut.targ <- fut.pred-hist.bias
    out.vec <- ifelse( (abs(data-fut.targ) <= correct.factor), 
                       yes=1, no=0) #is.negative(data-fut.targ)
    return(out.vec)
}

is.negative <- function(num){
  #assumes no 0 values are passed 
  return(ifelse(num > 0, 1, -1))
}