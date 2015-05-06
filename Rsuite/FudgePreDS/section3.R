#'section3.R
#'
#'Adjust the historic predictor, historic target and future predictor datasets
#'input into it, in accoradance with a list of instructions. Also ouputs a list of
#'changed instructions for the section 5 adjustments
#'
#'@param s3.instructions: A list of commands controlling which adjustment steps
#'are preformed, and in what order, as well as whether ar QC mask is to be calculated
#'at any point. Consists of a list of lists with elements of the form
#'list(type='SBiasCorr', qc.mask='on', adjust.out='off', args=list('na'))
#'@param hist.pred: The historic predictor(s) of a downscaling run
#'@param hist.targ: The historic target of a downscaling fun
#'@param fut.pred: The future predcictor(s) of a downscaling run
#'@param s5.instructions: The list of instructions for a section 5 adjustment. May be 
#'changed by steps taken during the section 3 adjustment (such as adding a 
#'backtransform to a transrorm)
#'
#'@returns A list containing the elements: 
#'hist.pred: the adjusted historic predictor dataset
#'hist.targ: the adjusted historic target dataset
#'fut.pred: the adjusted future predictor dataset
#'s5.instructions: the modified instructions for the section 5 adjustments to take place later.

callS3Adjustment<-function(s3.instructions=list('na'),
                   hist.pred = NA, 
                   hist.targ = NA, 
                   fut.pred = NA, 
                   s5.instructions=list('na')){
  #Define data that will change with iteration
  adjusted.list <- list(input=list('hist.pred' = hist.pred, 'hist.targ' = hist.targ, 'fut.pred' = fut.pred), 
                        s5.list=s5.instructions)
  
  for(element in 1:length(s3.instructions)){
    test <- s3.instructions[[element]]
    print(test)
    #Note that BOTH ELEMENTS get returned for the adjusted output. 
    #The transforms may have elements that will depend on the conditions of the initial transform, 
    #and the order of the backtransform is going to be dependant on the order of the elements
    #that go into it. 
    adjusted.list <- switch(test$type,
                              'PR' = return(callPRPreproc(test, adjusted.list$input, adjusted.list$s5.list)),
                              stop(paste('Adjustment Method Error: method', test$s5.method, 
                                         "is not supported for callS5Adjustment. Please check your input.")))
  }
  return(adjusted.list)
}

callPRPreproc <- function(test, input, postproc.output){
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
  if('apply_0_mask'%in%pr.names){
    apply.0.mask = (test$pp.args$apply_0_mask=='on')
  }else{
    message("Assuming that an old XML is being used; default behavior in place")
    apply.0.mask=FALSE
  }
  #at the end, instructions are unchanged
  temp.out <- AdjustWetdays(ref.data=input$hist.targ, ref.units=attr(input$hist.targ, "units")$value, 
                                         adjust.data=input$hist.pred, adjust.units=attr(input$hist.pred, "units")$value, 
                                         adjust.future=input$fut.pred, adjust.future.units=attr(input$fut.pred, "units")$value,
                                         opt.wetday=threshold, 
                                         lopt.drizzle=lopt.drizzle, 
                                         lopt.conserve=lopt.conserve, 
                            zero.to.na=apply.0.mask)
  if(apply.0.mask){
    fut.prmask <- temp.out$future$pr_mask
    pr_mod <- postproc.output$propts
    pr_mod$qc_args$fut.prmask <- fut.prmask
    postproc.output$propts <- pr_mod
  }
  return(list('input'=list('hist.targ' = temp.out$ref$data, 'hist.pred' = temp.out$adjust$data, 'fut.pred' = temp.out$future$data),
              's5.list'=postproc.output))
}