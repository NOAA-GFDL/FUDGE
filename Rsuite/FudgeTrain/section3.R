#'calls3Adjustment.R
#'
#'Initial stab at a pre-processing function called the same way
#'that the post-processing functions are. It's going to be needed
#'for the transforms soon, anyway.
#'IMPORTANT DIFFERENCE: There will never be any masks on these, 
#'or at least not for the first six months. 
#'
#'Creates a mask of the same dimensions as the downscaled data 
#'for which the default behavior is that a 1 means that the data
#'passes the QC check, and a 0 means that the data does not pass the QC
#'check. This behavior can change depending upon the QC function in question,
#'but this is the general behavior to keep in mind.
#'
#'@param s3.instructions: A list of commands controlling which adjustment steps
#'are preformed, and in what order, as well as whether ar QC mask is to be calculated
#'at any point. Consists of a list of lists with elements of the form
#'list(type='SBiasCorr', qc.mask='on', adjust.out='off', args=list('na'))
#'@param var: The variable being downscaled. 
#'------Parameters required for SBiasCorr-------
#'#'@param data: The data undergoing a qc check/adjustment steps
#'@param hist.pred: The historic predictor of a dataset
#'@param hist.targ: 
#'@param fut.pred: 
#'------Parameters related to time windowing----
#'
#'@returns A vector of values for the time series at the individual x,y, point
#'with 0 for all values that did not pass the test and 1 for all values that did.
#'CEW edit 10-22 to incorporate the proposed looping structure
callS3Adjustment<-function(s3.instructions=list('na'),
                   data = NA, #downscaled data - from this run or another
                   hist.pred = NA, 
                   hist.targ = NA, 
                   fut.pred = NA, 
                   create.qc.mask=FALSE, create.adjust.out=FALSE, 
                   s5.instructions=list('na')){
  #Create list of variables that will not change with iteration
  #input<- list('hist.pred' = hist.pred, 'hist.targ' = hist.targ, 'fut.pred' = fut.pred)
  #Define data that will change with iteration
  adjusted.list <- list(input=list('hist.pred' = hist.pred, 'hist.targ' = hist.targ, 'fut.pred' = fut.pred), 
                        s5.list=s5.list)
  
  for(element in 1:length(s3.instructions)){
    test <- s5.instructions[[element]]
    #Note that BOTH ELEMENTS get returned for the adjusted output. 
    #The transforms may have elements that will depend on the conditions of the initial transform, 
    #and the order of the backtransform is going to be dependant on the order of the elements
    #that go into it. 
    adjusted.list <- switch(test$type,
                              'sdev' = return(callSdev(test, adjusted.list$input, adjusted.list$s5.list)),
                              'pr' = return(callPR(test, adjusted.list$input, adjusted.list$s5.list)),
                              #'sdev2' = return(callSdev2(test,  input, adjusted.output)),
                              #'SBiasCorr' = return(callSBCorr(test,  input, adjusted.output)),
                              #'flag.neg' = return(callFlagNegativeValues(test, input, adjusted.output)),
                              #'Nothing' = return(callNoMethod(test, input, adjusted.output)),
                              stop(paste('Adjustment Method Error: method', test$s5.method, 
                                         "is not supported for callS5Adjustment. Please check your input.")))
  }
  return(adjusted.list)
}

callPR <- function(test, input, adjusted.output){
  #Outputs a set of adjusted input datasets
  #as output by the precipitation adjustment
  #functions
  
  #Obtain function args
  if('pr_threshold_in'%in%test && 'pr_freqadj_in'%in%test
     && 'pr_conserve_in'%in%test){
  pr.mask.opt = pr_opts$pr_threshold_in
  lopt.drizzle = pr_opts$pr_freqadj_in=='on'
  lopt.conserve= pr_opts$pr_conserve_in=='on'
  }else{
    stop(paste("Precipitation Pre-Processing Argument Error: one or more of pr_threshold_in, 
               ir_freqadj_in, or pr_conserve_in not present in arguments to precipitation
               pre-processing function."))
  }
  
  #at the end, instructions are unchanged
  return(list('input'=AdjustWetdays(ref.data=input$hist.targ, ref.units=attr(input$hist.targ, "units"), 
                                  adjust.data=input$hist.pred, adjust.units=attr(input$hist.pred, "units"), 
                                  adjust.future=input$fut.pred, adjust.future.units=attr(input$fut.pred, "units"),
                                  opt.wetday=test$opt.wetday, 
                                  lopt.drizzle=test$lopt.drizzle, 
                                  lopt.conserve=test$lopt.conserve), 
              's5.list'=s5.list))
  
}

callSdev <- function(test, input, adjusted.output){
  #Outputs a mask where NA values show flagged data
  #and ones show good data
  #with the test defined as output within
  #two standard deviations of the total downscaled output
  out.sdev <- sd(adjusted.output$ds.out)
  out.comp <- out.sedev*2
  out.mean <- mean(adjusted.output$ds.out)
  mask.vec <- ifelse( (out.comp <= abs(adjusted.output$ds.out-mean)), 
                      yes=1, no=NA)
  out.list <- adjusted.output #Everything should be returned as-is, unless something neat happens
  if(test$qc.mask=='on'){
    out.list$qc.mask <- mask.vec
  }
  if(test$adjust.out=='on'){
    adjust.vec <- ifelse( (is.na(mask.vec)), 
                          yes=ifelse( (1==sign(out.mean-adjusted.output$ds.out)), 
                                      out.mean-out.comp, out.mean+out.comp ), 
                          no=adjusted.output$ds.out)
    out.list$ds.out <- adjust.vec
  }
  return(out.list)
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
  #Outputs a mask where NA values show flagged data and 
  #1's show good data
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
                      yes=1, no=NA)
  out.list <- adjusted.output #Everything should be returned as-is, unless something neat happens
  print(test$qc.mask)
  if(test$qc.mask=='on'){
    out.list$qc.mask <- mask.vec
  }
  if(test$adjust.out=='on'){ #The 'off/na thing is distracting  ##Switched from !='na' to 'on'
#    adjust.vec <- ifelse( (abs(adjusted.output$data-fut.targ) <= correct.factor), 
#                          yes=adjusted.output$data, no=fut.targ)
    adjust.vec <- ifelse( (is.na(mask.vec)), yes=fut.targ, no=adjusted.output$ds.out)
    out.list$ds.out <- adjust.vec
  }else{
    #You don't need to do anything - it is already done!
    #out.list$ds.out<- data
  }
    return(out.list)
}

callFlagNegativeValues <- function(test, input, adjusted.output){
  #Flags negative values in the downscaled output with NA
  #with the expectation that they may get adjusted later. 
  mask.vec <- ifelse( adjusted.output$ds.out > 0, yes=1, no=NA)
  out.list <- adjusted.output
  if(test$qc.mask=='on'){
    out.list$qc.mask <- mask.vec
  }
  if(test$adjust.out=='on'){
    adjust.vec <- ifelse( (is.na(mask.vec)), yes=0, no=adjusted.output$ds.out)
    out.list$ds.out <- adjust.vec
  }
  return(out.list)
}


########Common Functions########
###Functions that might be used by more than one adjustment function
round.negative <- function(num){
  #assumes no 0 values are passed 
  return(ifelse(num > 0, 1, -1))
}
