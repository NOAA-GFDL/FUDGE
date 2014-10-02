#'QCDSValues.R
#'
#'Creates a mask of the same dimensions as the downscaled data 
#'for which the default behavior is that a 1 means that the data
#'passes the QC check, and a 0 means that the data does not pass the QC
#'check. This behavior can change depending upon the QC function in question,
#'but this is the general behavior to keep in mind.
#'
#'@param data: The data undergoing a qc check.
#'@param qc.data: The test being performed. Is used
#'to call the qc-specific test.
#'@param qc.test
#'@param var: The variable being downscaled. 
#'------Parameters required for kdAdjust-------
#'@param hist.pred
#'@param hist.targ
#'@param fut.pred
#'------Parameters related to time windowing----
#'
#'@returns A vector of values for the time series at the individual x,y, point
#'with 0 for all values that did not pass the test and 1 for all values that did.
#'


QCDSValues<-function(data, qc.data=NULL, qc.test, hist.pred=NULL, hist.targ=NULL, fut.pred=NULL, 
                     var='tasmax'){
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
                       yes=0, no=round.negative(data-fut.targ)) #round.negative(data-fut.targ)
    return(out.vec)
}

round.negative <- function(num){
  #assumes no 0 values are passed 
  return(ifelse(num > 0, 1, -1))
}