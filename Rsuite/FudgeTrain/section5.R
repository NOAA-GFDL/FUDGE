#'section5.R
#'
#'Calls post-downscaling adjustments on the downcsaled data, in keeping with section
#'5 of the FUDGE schematic. 
#'
#'------Parameters related to instructions -------
#'@param s5.instructions: A list of commands controlling which adjustment steps
#'are preformed, and in what order, as well as whether ar QC mask is to be calculated
#'at any point. Consists of a list of lists with elements of the form
#'list(type='SBiasCorr', qc.mask='on', adjust.out='off', args=list('na'))
#'@param var: The variable being downscaled. 
#'------Parameters related to datasets -------
#'@param data: The downscaled data undergoing the qc and adjustment steps
#'@param hist.pred: The historic predictor used to generate the downscaled data
#'@param hist.targ: The historic target used to generate the downscaled data
#'@param fut.pred:  The future predictor used to generate the downscaled data
#'
#'@returns a list containing two components: 
#'ds.out: An array of the same dimensions as the input downscaled data, adjusted by the section
#'5 functions. Is equal to the input downscaled data if no adjustments took place in section 5.
#'qc.mask: A mask of the same dimensions of the input downscaled data, in which every 1 represents 
#'an unflagged value, and every NA represents a value flagged as suspicious. Is NULL if no qc mask 
#'is generated from section 5. 
callS5Adjustment<-function(s5.instructions=list('na'),
                   data = NA, #downscaled data - from this run or another
                           data.atts = NA,
                   hist.pred = NA, 
                   hist.targ = NA, 
                   fut.pred = NA, 
                   create.qc.mask=FALSE, create.adjust.out=FALSE){
  
  input<- list('hist.pred' = hist.pred, 'hist.targ' = hist.targ, 'fut.pred' = fut.pred, 'data.atts'=data.atts)
  qc.mask <- NULL  #If no mask generated, will keep being null forever
  adjusted.output <- list("ds.out" = data, "qc.mask" = qc.mask)
  for(element in 1:length(s5.instructions)){
    test <- s5.instructions[[element]]
    adjusted.output <- switch(test$type, 
                              'sdev' = return(callSdev(test, input, adjusted.output)),
                              'sdev2' = return(callSdev2(test,  input, adjusted.output)),
                              'SBiasCorr' = return(callSBCorr(test,  input, adjusted.output)),
                              'flag.neg' = return(callFlagNegativeValues(test, input, adjusted.output)),
                              'PR' = return(callPRPostproc(test, input, adjusted.output)),
                              'Nothing' = return(callNoMethod(test, input, adjusted.output)),
                              stop(paste('Adjustment Method Error: method', test$s5.method, 
                                         "is not supported for callS5Adjustment. Please check your input.")))
  }
  return(adjusted.output)
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

callSBCorr <- function(test, input, adjusted.output, verbose=FALSE){
  #Outputs a mask where NA values show flagged data and 
  #1's show good data
  #'Assumes one predictor variable and one target variable
  
  if(verbose){print("entering simple bias correction function")}
  if(!is.null(test$qc_args$toplim) && !is.null(test$qc_args$botlim)){
    toplim <- test$qc_args$toplim
    botlim <- test$qc_args$botlim
  }else{
    stop("Section 5 Adjustment Error: Arguments toplim and botlim are not present for the SBiasCorr function. Please check your XML.")
  }
  #Unlist the predictors
  hist.pred <- unlist(input$hist.pred)
  fut.pred <- unlist(input$fut.pred)
#       save(file="~/Code/test_multivar.R", list=c('test', 'input', 'adjusted.output', 'hist.pred', 'fut.pred'))
#       print(paste("save.file:~/Code/test_multivar.R"))
#       stop('examine input results')
    #compute difference for all time values
    hist.bias <- mean(hist.pred-input$hist.targ)
    fut.targ <- fut.pred-hist.bias
  mask.vec <- ifelse( (botlim <= (adjusted.output$ds.out-fut.targ) & (adjusted.output$ds.out-fut.targ) < toplim), 
                      yes=1, no=NA)
  out.list <- adjusted.output #Everything should be returned as-is, unless something neat happens
  if(test$qc.mask=='on'){
    out.list$qc.mask <- mask.vec
  }
  if(test$adjust.out=='on'){ #The 'off/na thing is distracting  ##Switched from !='na' to 'on'
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
  #Edited 5-13 for initial try at adding looping by RIP to the scheme invoked
  rip.list <- lapply(input$data.atts$fut.pred, '[[', 'rip')
  rips <- unique(rip.list)
  #Loop by rip - remember, s3 and s5 get indexed by rip, ds out components get indexed by var.name
  for (rip in rips){
    #create sppropriate variable name
    ds.var <- paste0(input$data.atts$fut.pred$hist.targ, ".", rip)
    #Find arguments to pre-processing function
    arg.names <- names(test$qc_args) #qc_args #pp_args
    if('thold'%in%arg.names && 'conserve'%in%arg.names){
      #Never adjusting to another frequency at this point in the process
      lopt.drizzle=FALSE
    }else{
      stop("Error in PR post-processing: One or more of thold or conserve not present in arguments to function")
    }
    if(test$qc_args$apply_0_mask=='on'){
      message('Applying wetday mask. Output will have at least as many days without precip as the CF datset.')
      temp.adjust <- adjusted.output$ds.out[[ds.var]]
      temp.adjust[test$qc_args$fut.prmask[[rip]]==0] <- 0
      adjusted.output$ds.out[[ds.var]] <- temp.adjust
      test$qc_args$fut.prmask[[rip]] <- 'calculated from the input pr data'
    }else{
      message('Not applying wetday mask. Output may have fewer days without precipitation than expected.')
    }
    
    #Obtain mask of days that will be eliminated
    out.mask <- MaskPRSeries(adjusted.output$ds.out[[ds.var]], units=input$data.atts[[1]]$units, index=test$qc_args$thold)
    #Apply the conserve option to the data
    if(test$qc_args$conserve=='on'){
      #There has got to be a way to do this with 'apply' and its friends, but I'm not sure that it;s worth it
      temp.adjust <- adjusted.output$ds.out[[ds.var]]
      for(i in 1:length(adjusted.output$ds.out[,1,1])){
        for(j in 1:length(adjusted.output$ds.out[1,,1])){
          esd.select <- temp.adjust[i,j,]
          mask.select <- out.mask[i,j,]
          esd.select[!is.na(esd.select)]<- conserve.prseries(data=esd.select[!is.na(esd.select)], 
                                                             mask=mask.select[!is.na(mask.select)])
          temp.adjust[i,j,]<- esd.select
          #Note: This section will produce negative pr if conserve is set to TRUE and the threshold is ZERO. 
          #However, there are checks external to the function to get that, so it might not be as much of an issue.
        }
      }
    }
    adjusted.output$ds.out[[ds.var]] <- temp.adjust
    #Apply the mask
    adjusted.output$ds.out[[ds.var]] <- as.numeric(adjusted.output$ds.out[[ds.var]]) * out.mask
    if(test$qc.mask=='on'){
      adjusted.output$qc.mask[[ds.var]] <- out.mask
    }
  }
  return(adjusted.output)          
}


round.negative <- function(num){
  #assumes no 0 values are passed 
  return(ifelse(num > 0, 1, -1))
}
