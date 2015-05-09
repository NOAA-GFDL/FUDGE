#'section5.R
#'
#'Performs an adjustment on downscaled data after the downscaling
#'has taken place, and/or creates a separate mask of values 
#'for which the default behavior is that a 1 means that the data
#'passes the QC check, and a 0 means that the data does not pass the QC
#'check. This behavior can change depending upon the QC function in question,
#'but this is the general behavior to keep in mind.
#'
#'@param data: The downscaled data
#'@param hist.pred: Historic predictor data used to generate the downscaled data
#'@param hist.targ: Historic target data used to generate the downscaled data
#'@param fut.pred:  Future predictor data used to generate the downscaled data
#'
#'@param s5.instructions: A list of commands controlling which adjustment steps
#'are preformed, and in what order, as well as whether ar QC mask is to be calculated
#'at any point. Consists of a list of lists with elements of the form
#'list(type='SBiasCorr', qc.mask='on', adjust.out='off', args=list('na'))
#'
#'@returns A list containing two elements: 'ds.out' containing the downscaled
#'data that may or may not have been adjusted, and 'qc.mask', containing the qc mask
#'if the mask was generated and NULL if no mask was generated.
#'CEW edit 10-22 to incorporate the proposed looping structure
callS5Adjustment<-function(s5.instructions=list('na'),
                   data = NA, #downscaled data - from this run or another
                   hist.pred = NA, 
                   hist.targ = NA, 
                   fut.pred = NA){
                   #create.qc.mask=FALSE, create.adjust.out=FALSE){

  input<- list('hist.pred' = hist.pred, 'hist.targ' = hist.targ, 'fut.pred' = fut.pred)
  qc.mask <- NULL  #If no mask generated, will keep being null forever
  adjusted.output <- list("ds.out" = data, "qc.mask" = qc.mask)
  for(element in 1:length(s5.instructions)){
    test <- s5.instructions[[element]]
    adjusted.output <- switch(test$type, 
                              'SBiasCorr' = return(callSBCorr(test,  input, adjusted.output)),
                              'flag.neg' = return(callFlagNegativeValues(test, input, adjusted.output)),
                              'PR' = return(callPRPostproc(test, input, adjusted.output)),
                              stop(paste('Adjustment Method Error: method', test$s5.method, 
                                         "is not supported for callS5Adjustment. Please check your input.")))
  }
  return(adjusted.output)
}

callSBCorr <- function(test, input, adjusted.output){
  #Outputs a mask where NA values show flagged data and 
  #1's show good data
  #Set corrective error factor:
  print("entering simple bias correction func")
  if(!is.null(test$qc_args$toplim) && !is.null(test$qc_args$botlim)){
    toplim <- test$qc_args$toplim
    botlim <- test$qc_args$botlim
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

callPRPostproc <- function(test, input, adjusted.output){
  #Performs the adjustments needed for post-downscaling precipitation
  #on the downscaled ouput, including a threshold adjustment for drizzle
  #bias and conservation of the total precipitation per time range
  
  #Find arguments to pre-processing function
  arg.names <- names(test$qc_args) #qc_args #pp_args
  if('thold'%in%arg.names && 'conserve'%in%arg.names){
    #Never adjusting to another frequency at this point in the process
    lopt.drizzle=FALSE
  }else{
    stop("Error in PR post-processing: One or more of thold or conserve not present in arguments to function")
  }
  if('fut.prmask'%in%arg.names){
    message('Applying wetday mask. Output will have at least as many days without precip as the CF datset.')
    adjusted.output$ds.out[test$qc_args$fut.prmask==0] <- 0
    test$qc_args$fut.prmask <- 'calculated from the input pr data'
  }else{
    message('Not applying wetday mask. Output may have fewer days without precipitation than expected.')
  }
  #Obtain mask of days that will be eliminated
  out.mask <- MaskPRSeries(adjusted.output$ds.out, units=attr(input$hist.targ, "units")$value, index=test$qc_args$thold)
  #Apply the conserve option to the data
  if(test$qc_args$conserve=='on'){
    #There has got to be a way to do this with 'apply' and its friends, but I'm not sure that it;s worth it      
    for(i in 1:length(adjusted.output$ds.out[,1,1])){
      for(j in 1:length(adjusted.output$ds.out[1,,1])){
        esd.select <- adjusted.output$ds.out[i,j,]
        mask.select <- out.mask[i,j,]
        esd.select[!is.na(esd.select)]<- conserve.prseries(data=esd.select[!is.na(esd.select)], 
                                                           mask=mask.select[!is.na(mask.select)])
        adjusted.output$ds.out[i,j,]<- esd.select
        #Note: This section will produce negative pr if conserve is set to TRUE and the threshold is ZERO. 
        #However, there are checks external to the function to get that, so it might not be as much of an issue.
      }
    }
  }
  #Apply the mask
  adjusted.output$ds.out <- as.numeric(adjusted.output$ds.out) * out.mask

#   pr.adjusted <- AdjustWetdays(ref.data=input$hist.targ, ref.units=attr(input$hist.targ, "units")$value, 
#                             adjust.data=input$hist.pred, adjust.units=attr(input$hist.pred, "units")$value, 
#                             adjust.future=input$fut.pred, adjust.future.units=attr(input$fut.pred, "units")$value,
#                             opt.wetday=test$qc_args$thold, 
#                             lopt.drizzle=FALSE, 
#                             lopt.conserve=test$qc_args$conserve)
  if(test$qc.mask=='on'){
    adjusted.output$qc.mask <- out.mask
  }
  return(adjusted.output)          
}


round.negative <- function(num){
  #assumes no 0 values are passed 
  return(ifelse(num > 0, 1, -1))
}
