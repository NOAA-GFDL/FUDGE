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
#'@returns A status saying whether the test completed itself successfully.
#'It also writes out the QC mask file as a correctly formatted NetCDF file
#'with attributes and dimensions cloned from the NetCDf file provided for 
#'data.
#'

CreateQCMask <- function(data, qc.data, qc.test='sdev', var='tasmax'){
  library(ncdf4)
  status=1
  if(dim(data)!=dim(qc.data)){
    stop(paste("QC Dimension Error: dataset data had dimensions of", 
               paste(dim(data), collapse=" "), "while dataset qc.test had dimensions of", 
               paste(dim(qc.data), collapse=" "), "."))
  }
  qc.mask <- rep(NA, prod(dim(data)[1:2]))
  dim(qc.mask)<-dim(data)[1:2]
  #Loop by spatial coordiantes
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[2]){
      print(paste("on coordinate i coord", i, "of", dim(data)[1], 
                  'and j coord', j, "of", dim(data)[2], "."))
      qc.mask[i,j]<- QCDSValues(data[i,j,], qc.data[i,j,], qc.test)
    }
  }
  
  return(qc.mask)
}

QCDSValues<-function(data, qc.data, qc.test){
  switch(qc.test, 
         'sdev' = return(callSdev(data, qc.data)),
         'sdev2' = return(callSdev2(data, qc.data)),
         stop(paste('QC Method Error: method', qc.test, 
                    "is not supported for CreateQCMask. Please check your input."))
  )
}

# throwQCError <- function(qc.test){
#   stop(paste('QC Method Error: method', qc.test, 
#              "is not supported for CreateQCMask. Please check your input."))
# }

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
  
  if(!is.null(time.window)){
    #Currently do nothing
    out.vector <- rep(NA, length(data))
    dim(out.vector) <- dim(data)
    for (t in 1:dim(time.window)[3]){
      loop.window <- as.numeric(time.window[,,t])
      loop.data.window <- as.numeric(time.data.window[,,t])
      hist.bias <- mean( (loop.window*hist.pred)-(loop.window*hist.targ) )
      fut.targ <- (loop.data.window*fut.pred)- hist.bias
      #return error if > abs(6), otherwise return 0
      out.vector[!is.na(loop.data.window)] <- (ifelse((abs(data-fut.targ) < correct.factor), 
                                                       (data-fut.targ), 0) )
    }
    return(out.vector)
  }else{
    #sum over all available time windows (window=annual)
    hist.bias <- mean(hist.pred-hist.targ)
    fut.targ <- fut.pred-hist.bias
    out.vec <- ifelse( (abs(data-fut.targ) < correct.factor), 
                       (data-fut.targ), 0)
    return(out.vec)
  }
}

is.negative <- function(num){
  #assumes no 0 values are passed 
  return(ifelse(num > 0, 1, -1))
}