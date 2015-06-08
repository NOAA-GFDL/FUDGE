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
                              'add.one' = return(callAddOne(test, input, adjusted.output)), 
                              'add.five' = return(callAddFive(test, input, adjusted.output)), 
                              'bcsd.correct' = return(callBCSDCorrect(test, input, adjusted.output)),
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
  #'But multiple ensemble members are supported
  
  if(verbose){print("entering simple bias correction function")}
  if(!is.null(test$qc_args$toplim) && !is.null(test$qc_args$botlim)){
    toplim <- test$qc_args$toplim
    botlim <- test$qc_args$botlim
  }else{
    stop("Section 5 Adjustment Error: Arguments toplim and botlim are not present for the SBiasCorr function. Please check your XML.")
  }
  target.var <- unlist(lapply(input$data.atts$hist.targ, "[[", 'var'))
  ds.vars <- names(adjusted.output$ds.out)
  #Unlist the historic predictors 
  hist.select <- c(lapply(input$data.atts$hist.pred, "[[", 'var')==target.var)
  hist.pred <- unlist(input$hist.pred[hist.select])
  out.list <- adjusted.output #Everything should be returned as-is, unless something neat happens
  for (ds.var in ds.vars){
    #There can be more than pne future predictor for the ensemble
    fut.pred <- unlist(input$fut.pred[[ds.var]])
    #compute difference for all time values
    hist.bias <- mean(hist.pred-input$hist.targ)
    fut.targ <- fut.pred-hist.bias
    mask.vec <- ifelse( (botlim <= (adjusted.output$ds.out[[ds.var]]-fut.targ) & 
                           (adjusted.output$ds.out[[ds.var]]-fut.targ) < toplim), 
                        yes=1, no=NA)
    if(test$qc.mask=='on'){
      out.list$qc.mask[[ds.var]] <- mask.vec
    }
    if(test$adjust.out=='on'){ #The 'off/na thing is distracting  ##Switched from !='na' to 'on'
      adjust.vec <- ifelse( (is.na(mask.vec)), yes=fut.targ, no=adjusted.output$ds.out[[ds.var]])
      out.list$ds.out[[ds.var]] <- adjust.vec
    }
  }
    return(out.list)
}

callFlagNegativeValues <- function(test, input, adjusted.output){
  #Flags negative values in the downscaled output with NA
  #with the expectation that they may get adjusted later. 
  
  #Set up ds vars to index by 
#   rip.list <- lapply(input$data.atts$fut.pred, '[[', 'rip')
#   rips <- unique(rip.list)
#   ds.vars <- paste0(input$data.atts$hist.targ$var, ".", rip.list)
#   print(ds.vars)
  #You don't actually need the RIPs; just the variable names for ds data
  ds.vars <- names(adjusted.output$ds.out)
  #Initialize output structure
  out.list <- adjusted.output
  for(ds.var in ds.vars){
    mask.vec <- ifelse(adjusted.output$ds.out[[ds.var]] > 0, yes=1, no=NA)
    if(test$qc.mask=='on'){
      out.list$qc.mask[[ds.var]] <- mask.vec
    }
    if(test$adjust.out=='on'){
      adjust.vec <- ifelse( (is.na(mask.vec)), yes=0, no=adjusted.output$ds.out[[ds.var]])
      out.list$ds.out[[ds.var]] <- adjust.vec
    }
  }
  return(out.list)
}

callPRPostproc <- function(test, input, adjusted.output){
  #Performs the adjustments needed for post-downscaling precipitation
  #on the downscaled ouput, including a threshold adjustment for drizzle
  #bias and conservation of the total precipitation per time range
  #Edited 5-13 for initial try at adding looping by RIP to the scheme invoked
  rip.list <- lapply(input$data.atts$fut.pred, '[[', 'rip')
  print(input$data.atts)
  rips <- unique(rip.list)
  print(rip.list)
  #stop ('examine structure of att table and query accordingly')
  #Loop by rip - remember, s3 and s5, and ds.out all get indexed by var.rip
  for (rip in rips){
    #create sppropriate variable name
    target.var <- names(input$data.atts$hist.targ)
    ds.var <- paste0(target.var, ".", rip) #There shou
    #Find arguments to pre-processing function
    print(test)
    print(test$qc_args)
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
    out.mask <- MaskPRSeries(adjusted.output$ds.out[[ds.var]], units=input$data.atts$hist.targ[[target.var]]$units, index=test$qc_args$thold)
    #Apply the conserve option to the data
    print(summary(as.vector(adjusted.output$ds.out[[ds.var]][!is.na(adjusted.output$ds.out[[ds.var]])])))
    if(test$qc_args$conserve=='on'){
      #There has got to be a way to do this with 'apply' and its friends, but I'm not sure that it;s worth it
      temp.adjust <- adjusted.output$ds.out[[ds.var]]
      ds.dim <- dim(temp.adjust)
      print(ds.dim)
      for(i in 1:ds.dim[1]){
        for(j in 1:ds.dim[2]){
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
    print(summary(as.vector(adjusted.output$ds.out[[ds.var]][!is.na(adjusted.output$ds.out[[ds.var]])])))
    #Apply the mask
    adjusted.output$ds.out[[ds.var]] <- as.numeric(adjusted.output$ds.out[[ds.var]]) * out.mask
    if(test$qc.mask=='on'){
      adjusted.output$qc.mask[[ds.var]] <- out.mask
    }
  }
  return(adjusted.output)          
}

callAddOne <- function(test, input, adjusted.output){
  #'Test method for playing with the mechanincs of calling
  #'multiple adjustment steps on the downscaled output
  #'  rip.list <- lapply(input$data.atts$fut.pred, '[[', 'rip')
  rips <- unique(lapply(input$data.atts$fut.pred, "[[", 'rip'))
  #Loop by rip - remember, s3 and s5, and ds.out all get indexed by var.rip
  for (rip in rips){
    #create sppropriate variable name
    ds.var <- paste0(input$data.atts$hist.targ$var, ".", rip)
    #Find arguments to pre-processing function
    #There are no arguments to be applied
    if(test$adjust.out=='on'){
    adjusted.output$ds.out[[ds.var]] <- (adjusted.output$ds.out[[ds.var]] + 1)
    }
    if(test$qc.mask=='on'){
      adjusted.output$qc.mask[[ds.var]] <- NA #more or less blank code for 
    }
  }
  return(adjusted.output) 
}

callAddFive <- function(test, input, adjusted.output){
  #'Test method for playing with the mechanincs of calling
  #'multiple adjustment steps on the downscaled output
  #'  rip.list <- lapply(input$data.atts$fut.pred, '[[', 'rip')
  rips <- unique(lapply(input$data.atts$fut.pred, "[[", 'rip'))
  #Loop by rip - remember, s3 and s5, and ds.out all get indexed by var.rip
  for (rip in rips){
    #create sppropriate variable name
    ds.var <- paste0(input$data.atts$hist.targ$var, ".", rip)
    #Find arguments to pre-processing function
    #There are no arguments to be applied
    if(test$adjust.out=='on'){
      adjusted.output$ds.out[[ds.var]] <- (adjusted.output$ds.out[[ds.var]] + 5)
    }
    if(test$qc.mask=='on'){
      adjusted.output$qc.mask[[ds.var]] <- NA #more or less blank code for 
    }
  }
  return(adjusted.output) 
}

callBCSDCorrect <- function(test, input, adjusted.output){
  #'Calls a BCSD-style adjustment on quantile-mapped code
  #'based off of the limits to the adjustment of the 
  #'MH and MF datasets
  rips <- unique(lapply(input$data.atts$fut.pred, "[[", 'rip'))
  targ.var <- names(input$data.atts$hist.targ)
  for(rip in rips){
    ds.var <- paste0(targ.var, ".", rip)
    fut.pred <- ds.var
    hist.pred <- names(input$hist.pred)[lapply(input$data.atts$hist.pred, 
                                         "[[", 'var')==targ.var]
    ds.correct <- bcsd.correct(model.hist = input$hist.pred[[hist.pred]], 
                               obs.hist = input$hist.targ, 
                               model.fut = input$fut.pred[[fut.pred]], 
                               obs.fut = adjusted.output$ds.out[[ds.var]])
    if(test$qc.mask=='on'){
      adjusted.output$qc.mask[[ds.var]] <- ifelse(ds.correct==adjusted.output$ds.out[[ds.var]], 
                                                  NA, 1)
    }
    if(test$adjust.out=='on'){
      adjusted.output$ds.out[[ds.var]] <- ds.correct
    }
  }
  return(adjusted.output)
}

bcsd.correct <- function(model.hist, obs.hist,  model.fut, obs.fut, opt='mean'){
  #derived from BCSD code provided by Catherine Hejo
  #Correct for maximum range of the distribution
#   model.hist.mean <- runmean(as.vector(model.hist), k=30, endrule="mean")
#   obs.hist.mean <- runmean(as.vector(obs.hist), k=30, endrule="mean")
#   print('runmean calculated')
  largex.tmax <- which(model.fut > max(model.hist))    #List of indices for which dist is stretched
  max.delta.tmax <- max(model.hist) - max(obs.hist)    #Delta calculated as diff of max(hist) - max(obs)
  obs.fut[largex.tmax] <- model.fut[largex.tmax] - max.delta.tmax
  ###Correct for minimum range of the distribution
  smallx.tmax <- which(model.fut < min(model.hist))    #List of indices for which dist is stretched
  min.delta.tmax <- min(model.hist) - min(obs.hist)    #Delta calculated as diff of min(hist) - min(obs)
  obs.fut[smallx.tmax] <- model.fut[smallx.tmax] - min.delta.tmax
  return(obs.fut)
}

round.negative <- function(num){
  #assumes no 0 values are passed 
  return(ifelse(num > 0, 1, -1))
}
