#'QCDSValues.R
#'
#'Creates a mask of the same dimensions as the downscaled data 
#'for which the default behavior is that a 1 means that the data
#'passes the QC check, and a 0 means that the data does not pass the QC
#'check. This behavior can change depending upon the QC function in question,
#'but this is the general behavior to keep in mind.
#'
#'@param s5.instructions: A list of commands controlling which adjustment steps
#'are preformed, and in what order, as well as whether ar QC mask is to be calculated
#'at any point. Consists of a list of lists with elements of the form
#'list(type='SBiasCorr', qc.mask='on', adjust.out='off', args=list('na'))
#'@param var: The variable being downscaled. 
#'------Parameters required for kdAdjust-------
#'#'@param data: The data undergoing a qc check/adjustment steps
#'@param hist.pred: The historic predictor of a dataset
#'@param hist.targ: 
#'@param fut.pred: 
#'------Parameters related to time windowing----
#'
#'@returns A vector of values for the time series at the individual x,y, point
#'with 0 for all values that did not pass the test and 1 for all values that did.
#'CEW edit 10-22 to incorporate the proposed looping structure
callS5Adjustment<-function(s5.instructions=list('na'),
  #s5.method='totally.fake',s5.args='na',
                   data = NA, #downscaled data - from this run or another
                   hist.pred = NA, 
                   hist.targ = NA, 
                   fut.pred = NA, 
                   create.qc.mask=FALSE, create.adjust.out=FALSE){
#   function(qc.test, data, hist.pred=NULL, hist.targ=NULL, fut.pred=NULL, var='tasmax', 
#                      qc.data=NULL, qc.mask=NULL){
#  element.list <- list( list("s5.method" = s5.method, "s5.args" = s5.args, 
 #                      'create.qc.mask' = create.qc.mask, 'create.adjust.out' = create.adjust.out))
  input<- list('hist.pred' = hist.pred, 'hist.targ' = hist.targ, 'fut.pred' = fut.pred)
  qc.mask <- NULL  #If no mask generated, will keep being null forever
  adjusted.output <- list("ds.out" = data, "qc.mask" = qc.mask)
  for(element in 1:length(s5.instructions)){
    test <- s5.instructions[[element]]
    print(summary(test$type))
    #message(test)
    adjusted.output <- switch(test$type, 
                              'sdev' = return(callSdev(test, input, adjusted.output)),
                              'sdev2' = return(callSdev2(test,  input, adjusted.output)),
                              'SBiasCorr' = return(callSBCorr(test,  input, adjusted.output)),
                              'Nothing' = return(callNoMethod(test, input, adjusted.output)),
                              stop(paste('Adjustment Method Error: method', test$s5.method, 
                                         "is not supported for callS5Adjustment. Please check your input.")))
  }
  return(adjusted.output)
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

callSBCorr <- function(test, input, adjusted.output){
  #Set corrective error factor:
  print("entering simple bias correction func")
  print(test$qc_options)
  if(!is.null(test$qc_options$toplim) && !is.null(test$qc_options$botlim)){
    toplim <- test$qc_options$toplim
    botlim <- test$qc_options$botlim
  }else{
    stop("Section 5 Adjustment Error: Arguments toplim and botlim are not present for the SBiasCorr function. Please check your XML.")
  }
    #compute difference for all time values
    hist.bias <- mean(input$hist.pred-input$hist.targ)
    fut.targ <- input$fut.pred-hist.bias
  mask.vec <- ifelse( (botlim <= (adjusted.output$ds.out-fut.targ) & (adjusted.output$ds.out-fut.targ) < toplim), 
                      yes=1, no=0)
  out.list <- adjusted.output #Everything should be returned as-is, unless something neat happens
  print(test$qc.mask)
  if(test$qc.mask=='on'){
    out.list$qc.mask <- mask.vec
  }
  if(test$adjust.out=='on'){ #The 'off/na thing is distracting  ##Switched from !='na' to 'on'
#    adjust.vec <- ifelse( (abs(adjusted.output$data-fut.targ) <= correct.factor), 
#                          yes=adjusted.output$data, no=fut.targ)
    adjust.vec <- ifelse( (mask.vec==1), yes=adjusted.output$ds.out, no=fut.targ)
    out.list$ds.out <- adjust.vec
  }else{
    #You don't need to do anything - it is already done!
    #out.list$ds.out<- data
  }
    return(out.list)
}

round.negative <- function(num){
  #assumes no 0 values are passed 
  return(ifelse(num > 0, 1, -1))
}
