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
#'@param hist.pred: The historic predictor(s) of a downscaling run
#'@param hist.targ: The historic target of a downscaling fun
#'@param fut.pred: The future predcictor(s) of a downscaling run
#'------Parameters related to time windowing----
#'
#'@returns A vector of values for the time series at the individual x,y, point
#'with 0 for all values that did not pass the test and 1 for all values that did.

callS3Adjustment<-function(s3.instructions=list('na'),
                   hist.pred = NA, 
                   hist.targ = NA, 
                   fut.pred = NA, 
                   create.qc.mask=FALSE, create.adjust.out=FALSE, 
                   s5.instructions=list('na')){
  #Create list of variables that will not change with iteration
  #input<- list('hist.pred' = hist.pred, 'hist.targ' = hist.targ, 'fut.pred' = fut.pred)
  #Define data that will change with iteration
  adjusted.list <- list(input=list('hist.pred' = hist.pred, 'hist.targ' = hist.targ, 'fut.pred' = fut.pred), 
                        s5.list=s5.instructions)
  
  for(element in 1:length(s3.instructions)){
    test <- s3.instructions[[element]]
    #Note that BOTH ELEMENTS get returned for the adjusted output. 
    #The transforms may have elements that will depend on the conditions of the initial transform, 
    #and the order of the backtransform is going to be dependant on the order of the elements
    #that go into it. 
    #TODO: ask Carlos about transform behavior. They seem as if they should operate in the same loop as
    #the 
    adjusted.list <- switch(test$type,
                              'PR' = return(callPR(test, adjusted.list$input, adjusted.list$s5.list)),
                              stop(paste('Adjustment Method Error: method', test$s5.method, 
                                         "is not supported for callS5Adjustment. Please check your input.")))
  }
  return(adjusted.list)
}

callPR <- function(test, input, postproc.output){
  #Outputs a set of adjusted input datasets
  #as output by the precipitation adjustment
  #functions
  #Note: you get truly SPECTACULAR fatal errors if one of the units values
  #passed to the function is null. 

  pr.names <- (names(test$pp.args))
  #Obtain function args
  if('thold'%in%pr.names && 'freqadj'%in%pr.names && 'conserve'%in%pr.names){
    threshold   =  test$pp.args$thold
    lopt.drizzle  = ( test$pp.args$freqadj =='on')
    lopt.conserve = ( test$pp.args$conserve =='on')
  }else{
    stop(paste("Precipitation Pre-Processing Argument Error: one or more of pr_threshold_in, 
               ir_freqadj_in, or pr_conserve_in not present in arguments to precipitation
               pre-processing function."))
  }
  
  #at the end, instructions are unchanged
  temp.out <- AdjustWetdays(ref.data=input$hist.targ, ref.units=attr(input$hist.targ, "units")$value, 
                                         adjust.data=input$hist.pred, adjust.units=attr(input$hist.pred, "units")$value, 
                                         adjust.future=input$fut.pred, adjust.future.units=attr(input$fut.pred, "units")$value,
                                         opt.wetday=threshold, 
                                         lopt.drizzle=lopt.drizzle, 
                                         lopt.conserve=lopt.conserve)
  return(list('input'=list('hist.pred' = temp.out$ref$data, 'hist.targ' = temp.out$adjust$data, 'fut.pred' = temp.out$future$data),
              's5.list'=postproc.output))
}