#'section5.R
#'
#'Performs an adjustment on downscaled data after the downscaling
#'has taken place, and/or creates a separate mask of values 
#'for which the default behavior is that a 1 means that the data
#'passes the QC check, and a 0 means that the data does not pass the QC
#'check. This behavior can change depending upon the QC function in question,
#'but this is the general behavior to keep in mind.
#'
#'------Parameters related to adjustment----
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
                           create.qc.mask=FALSE, create.adjust.out=FALSE, 
                           verbose=FALSE){
  input<- list('hist.pred' = hist.pred, 'hist.targ' = hist.targ, 'fut.pred' = fut.pred, 'data.atts'=data.atts)
  qc.mask <- NULL  #If no mask generated, will keep being null forever
  adjusted.output <- list("ds.out" = data, "qc.mask" = qc.mask)
  out <- list('input' = input, 'adjusted.output' = adjusted.output)
  for(element in 1:length(s5.instructions)){
    test <- s5.instructions[[element]]
    if(verbose){
      print(paste("on test", test[[1]]))
      print("MH:")
      print(lapply(out$input$hist.pred, 'summary'))
      print("OH:")
      print(summary(out$input$hist.targ))
      print("MF:")
      print(lapply(out$input$fut.pred, 'summary'))
    }
    out <- switch(test$type, 
                  'sdev' = callSdev(test, out$input, out$adjusted.output),
                  'sdev2' = callSdev2(test,  out$input, out$adjusted.output),
                  'SBiasCorr' = callSBCorr(test,  out$input, out$adjusted.output),
                  'flag.neg' = callFlagNegativeValues(test, out$input, out$adjusted.output),
                  'PR' = callPRPostproc(test, out$input, out$adjusted.output),
                  'Nothing' = callNoMethod(test, out$input, out$adjusted.output),
                  'add.one' = callAddOne(test, out$input, out$adjusted.output), 
                  'add.five' = callAddFive(test, out$input, out$adjusted.output), 
                  'BCQM.tail.adj' = callBCSDCorrect(test, out$input, out$adjusted.output),
                  'simpletailadj' = callTailAdjust(test,  out$input, out$adjusted.output),
                  'backtransform' = callPRBacktransfrom(test, out$input, out$adjusted.output),
                  stop(paste('Adjustment Method Error: method', test$type, 
                             "is not supported for callS5Adjustment. Please check your input.")))
    if(verbose){
      print('test complete')
      print("MH:")
      print(lapply(out$input$hist.pred, 'summary'))
      print("OH:")
      print(summary(out$input$hist.targ))
      print("MF:")
      print(lapply(out$input$fut.pred, 'summary'))
    }
  }
  return(out$adjusted.output)
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
  return(list('input' = input, 'adjusted.output' = out.list))
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
    hist.bias <- mean(hist.pred-input$hist.targ, na.rm=TRUE)
    fut.targ <- fut.pred-hist.bias
    tmp.diff <- as.numeric(adjusted.output$ds.out[[ds.var]]) - fut.targ
    mask.vec <- ifelse( (botlim <= (tmp.diff) & 
                           (tmp.diff) < toplim), 
                        yes=1, no=NA)
    if(test$qc.mask=='on'){
      out.list$qc.mask[[ds.var]] <- mask.vec
    }
    if(test$adjust.out=='on'){
      adjust.vec <- ifelse( (is.na(mask.vec)), yes=fut.targ, no=adjusted.output$ds.out[[ds.var]])
      out.list$ds.out[[ds.var]] <- adjust.vec
    }
  }
    return(list('input' = input, 'adjusted.output' = out.list))
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
#     is.missing  <- ifelse(is.na(adjusted.output$ds.out[[ds.var]]), 0, 1)
#     mask.vec <- is.missing
    mask.vec <- ifelse(adjusted.output$ds.out[[ds.var]] >= 0, yes=1, no=0)
    if(test$qc.mask=='on'){
      out.list$qc.mask[[ds.var]] <- mask.vec
      out.list$qc.mask[[ds.var]][mask.vec==0] <- NA
    }
    if(test$adjust.out=='on'){
      adjust.vec <- ifelse( (mask.vec==0), yes=0, no=adjusted.output$ds.out[[ds.var]])
      #adjust.vec[is.missing==0] <- NA
      out.list$ds.out[[ds.var]] <- adjust.vec
    }
  }
  return(list('input' = input, 'adjusted.output' = out.list))
}

callPRPostproc <- function(test, input, adjusted.output){
  #Performs the adjustments needed for post-downscaling precipitation
  #on the downscaled ouput, including a threshold adjustment for drizzle
  #bias and conservation of the total precipitation per time range
  #Edited 5-13 for initial try at adding looping by RIP to the scheme invoked
  rip.list <- lapply(input$data.atts$fut.pred, '[[', 'rip')
  rips <- unique(unlist(rip.list, use.names=FALSE))
  if(length(rips) == 0){
    stop(paste("Error in callPRPostproc: list of rips is of length 0!"))
  }
  arg.names <- names(test$qc_args)
  #This code is not getting used anywhere else, and I can't remmber why it was supposed to be important
  if(!('thold'%in%arg.names) || !('conserve'%in%arg.names)){
    stop("Error in PR post-processing: One or more of thold or conserve not present in arguments to function")
  }
  #Loop by rip - remember, s3 and s5, and ds.out all get indexed by var.rip
  for (r in 1:length(rips)){
    rip = rips[r]
    #create sppropriate variable name
    target.var <- names(input$data.atts$hist.targ)
    ds.var <- paste0(target.var, ".", rip)
    #Apply the 0 mask, if needed
    if(test$qc_args$ignore0=='on'){
      message(paste('Applying wetday mask for rip', rip,
                    '. Output will have at least as many days without precip as the data used to downscale.'))
      temp.adjust <- adjusted.output$ds.out[[ds.var]]
      #temp.adjust[test$qc_args$fut.prmask[[rip]]==0] <- 0
      #If you downscaled on the future dataset, the adjustment is straightforward
      if(sum(grepl('MF', ds.names)) > 0){
        print("Downscaling took place on the MF dataset")
        temp.adjust[test$qc_args$fut.prmask[[rip]]==0] <- 0
        adjusted.output$ds.out[[ds.var]] <- temp.adjust
        test$qc_args$fut.prmask[[rip]] <- 'calculated from the input pr data' #I have no memory of why this was included here
      }else{
        print("Downscaling took place on the OH dataset; adjusting OH pr mask to fit")
        #delta-specific adjustment for the leapyear indices: 
        print(summary(delta.mask))
#         temp.prmask <- trim.time.mask(data=test$qc_args$targ.prmask, 
#                                                    mask=delta.mask)
        print(dim(test$qc_args$targ.prmask))
        #temp.prmask <- test$qc_args$targ.prmask[,,c(which(!is.na(delta.mask))), drop=FALSE]
        temp.prmask <- trim.time.mask(data=test$qc_args$targ.prmask, mask=delta.mask)
        print(dim(temp.prmask))
        #Note: you can't modify this in-place if you are working with the multi rip case!
        targ.mask <- array(NA, dim=c(dim(temp.adjust)))
        targ.timelen <- dim(temp.prmask)[3]
        fut.timelen <- dim(temp.adjust)[3]
#         print(summary(test$qc_args$targ.prmask))
#         print(dim(test$qc_args$targ.prmask))
#         print(paste('targ.timelen:', targ.timelen))
#         print(paste('fut.timelen:', fut.timelen))
        for(t in seq(from=1, to=fut.timelen, by=targ.timelen)){
          if((t + targ.timelen - 1) > fut.timelen){
            #Final case of the fut.timelen > hist.timelen case:
            t.stop <- fut.timelen
            #t.ref <- fut.timelen #(t + targ.timelen - 1)%%fut.timelen
            t.ref <- fut.timelen - t + 1
          }else{
            t.stop <- t + targ.timelen - 1
            t.ref <- targ.timelen
          }
#            print(paste("t:", t))
#            print(t.stop)
#            print(t.ref)
#           print('counts of days without pr in each:')
#            print(sum(is.na(adjusted.output$ds.out[[ds.var]][,,c(seq(from=t, to=t.stop, by=1))])))
#            print(sum(temp.prmask[,,c(seq(from=1, to=t.ref, by=1))]==0))
          targ.mask[,,c(seq(from=t, to=t.stop))] <- temp.prmask[,,c(seq(from=1, to=t.ref, by=1))]
        }
#         print('summary of target mask:')
#         print(summary(targ.mask))
#         print('summary of downscaled output:')
#         print(summary(as.vector(temp.adjust)))
#         print('entire prmask output:')
#         print(dim(test$qc_args$targ.prmask))
        #Correction 11-4 for pr masks of 0, 1, and NA
        temp.adjust[targ.mask==0] <- 0
        print('summary of temp.adjust after mask application:')
        print(summary(temp.adjust))
        adjusted.output$ds.out[[ds.var]] <- temp.adjust
      }
    }else{
      message('Not applying wetday mask. Output may have fewer days without precipitation than expected.')
    }
    adjusted.pr <- pr.postproc(pr.data  = adjusted.output$ds.out[[ds.var]], 
                               pr.units = input$data.atts$hist.targ[[target.var]]$units, 
                               pr.index = test$qc_args$thold, 
                               pr.conserve = (test$qc_args$conserve=='on'))
    adjusted.output$ds.out[[ds.var]] <- adjusted.pr$ds.out
    if(test$qc.mask=='on'){
      adjusted.output$qc.mask[[ds.var]] <- adjusted.output$qc.mask
    }
    #return(list('input' = input, 'adjusted.output' = adjusted.output))
  }
  return(list('input' = input, 'adjusted.output' = adjusted.output))          
}

pr.postproc <- function(pr.data, pr.units, pr.index, pr.conserve){
  #Internal helper function for reading the pr post-processing function
  #clearly and concisely
  out.mask <- MaskPRSeries(pr.data, pr.units, pr.index)
  if(pr.conserve){
    pr.dim <- dim(pr.data)
    for(i in 1:pr.dim[1]){
      for(j in 1:pr.dim[2]){
        data.vec <- pr.data[i,j,]
        mask.vec <- out.mask[i,j,]
        data.vec[!is.na(data.vec)] <- conserve.prseries(data=data.vec[!is.na(data.vec)], 
                                                        mask=mask.vec[!is.na(mask.vec)])
        pr.data[i,j,] <- data.vec
      }
    }
  }
  pr.data <- as.numeric(pr.data)*out.mask
  return(list(ds.out=pr.data, adjusted.output=out.mask))
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
  return(list('input' = input, 'adjusted.output' = adjusted.output)) 
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
  return(list('input' = input, 'adjusted.output' = adjusted.output)) 
}

callBCSDCorrect <- function(test, input, adjusted.output){
  #'Calls a BCSD-style adjustment on quantile-mapped code
  #'based off of the limits to the adjustment of the 
  #'MH and MF datasets
  rips <- unique(lapply(input$data.atts$fut.pred, "[[", 'rip'))
  targ.var <- names(input$data.atts$hist.targ)
  arg.names <- names(test$qc_args)
  if('ignore0'%in%arg.names){
    #Hasty adjustment to set missing values to 0 in bcsd correction
    ignore0 <- (test$qc_args$ignore0=='on')
  }else{
    ignore0 <-  FALSE
  }
  if(!('sample_size')%in%arg.names){
    sample_size <- 1
  }else if(test$qc_args$sample_size==0){
    sample_size = 1
  }else{
    sample_size = test$qc_args$sample_size
  }
  for(rip in rips){
    ds.var <- paste0(targ.var, ".", rip)
    fut.pred <- ds.var
    hist.pred <- names(input$hist.pred)[lapply(input$data.atts$hist.pred, 
                                         "[[", 'var')==targ.var]
    #print(summary(as.vector(adjusted.output$ds.out[[ds.var]])))
    ds.correct <- bcsd.correct(model.hist = input$hist.pred[[hist.pred]], 
                               obs.hist = input$hist.targ, 
                               model.fut = input$fut.pred[[fut.pred]], 
                               obs.fut = adjusted.output$ds.out[[ds.var]], 
                               sample_size = sample_size)
    #print(summary(as.vector(ds.correct)))
    if(test$qc.mask=='on'){
      qc.tmp <- ifelse(mapply(both.na, ds.correct, as.numeric(adjusted.output$ds.out[[ds.var]])), 
                       1, 0)
      #print(summary(as.vector(qc.tmp)))
      non.missing <- which(qc.tmp==0)
      ds.tmp <- ifelse(ds.correct==as.numeric(adjusted.output$ds.out[[ds.var]]),
                                    1, NA)
      qc.tmp[non.missing] <- ds.tmp[non.missing]
      #print(summary(as.vector(qc.tmp)))
      adjusted.output$qc.mask[[ds.var]] <- qc.tmp
    }
    if(test$adjust.out=='on'){
      adjusted.output$ds.out[[ds.var]] <- as.numeric(ds.correct)
    }
  }
  return(list('input' = input, 'adjusted.output' = adjusted.output))
}

bcsd.correct <- function(model.hist, obs.hist,  model.fut, obs.fut, sample_size=0){
  #derived from BCSD code provided by Catherine Hayhoe
  #Correct for maximum range of the distribution
  sample_size = sample_size - 1
  if(sample_size==0){
    model.hist.val <- max(model.hist, na.rm=TRUE)
    obs.hist.val <- max(obs.hist, na.rm=TRUE)
  }else{
    mh.len <- length(model.hist)
    model.hist.val <- mean(sort(model.hist)[(mh.len-sample_size):mh.len])
    oh.len <- length(obs.hist)
    obs.hist.val <- mean(sort(obs.hist)[(oh.len-sample_size):oh.len])
  }
  largex.tmax <- which(model.fut > max(model.hist))    #List of indices for which dist is stretched
  max.delta.tmax <- model.hist.val - obs.hist.val   #Delta calculated as diff of max(hist) - max(obs)
  print(length(largex.tmax))
  obs.fut[largex.tmax] <- model.fut[largex.tmax] - max.delta.tmax
  ###Correct for minimum range of the distribution
  if(sample_size==0){
    model.hist.val <- min(model.hist, na.rm=TRUE)
    obs.hist.val <- min(obs.hist, na.rm=TRUE)
  }else{
    mh.len <- length(model.hist)
    model.hist.val <- mean(sort(model.hist)[1:sample_size])
    oh.len <- length(obs.hist)
    obs.hist.val <- mean(sort(obs.hist)[1:sample_size])
  }
  smallx.tmax <- which(model.fut < min(model.hist))    #List of indices for which dist is stretched
  min.delta.tmax <- model.hist.val - obs.hist.val    #Delta calculated as diff of min(hist) - min(obs)
  print(length(smallx.tmax))
  obs.fut[smallx.tmax] <- model.fut[smallx.tmax] - min.delta.tmax
  return(obs.fut)
}

callPRBacktransfrom <- function(test, input, adjusted.output){
  #calls the backtransform for the downscaled data
  #Note: this adjusts data only; there is never going to be
  #a qc mask provided from this data (at least for now)
  #ds.vars <- names(adjusted.output$ds.out)
  rips <- unique(lapply(input$data.atts$fut.pred, "[[", 'rip'))
  targ.var <- names(input$data.atts$hist.targ)
  out.list <- adjusted.output
  #print(rips)
#   print('pre_transform:')
#     print(lapply(input$hist.pred, 'summary'))
#     print(summary(input$hist.targ))
#     print(lapply(input$fut.pred, 'summary'))
  for(rip in rips){
    #print(rip)
    ds.var <- paste0(targ.var, ".", rip)
    if(sum(sapply(test$qc_args$vars,'grepl',ds.var))==1){ #ds.vars? I changed from ds.vars to ds.var 10-22
      if(!is.null(test$qc_args$arg)){
        #print(summary(out.list$ds.out[[ds.var]]))
        #save(file='/home/cew/Code/testing/test_out.R', list=c('test', 'out.list', 'ds.var'))
        #print(sumamry(out.list$ds.out[[ds.var]]))
        out.list$ds.out[[ds.var]] <- do.call(test$qc_args$fxn, 
                                             args=list(as.numeric(out.list$ds.out[[ds.var]]), 
                                                       test$qc_args$arg))
        #print(sumamry(out.list$ds.out[[ds.var]]))
        input$fut.pred[[ds.var]] <- do.call(test$qc_args$fxn, 
                                            args=list(as.numeric(input$fut.pred[[ds.var]]), 
                                                      test$qc_args$arg))
      }else{
        out.list$ds.out[[ds.var]] <- do.call(test$qc_args$fxn, 
                                             args=list(out.list$ds.out[[ds.var]]))
        input$fut.pred[[ds.var]] <- do.call(test$qc_args$fxn, 
                                            args=list(input$fut.pred[[ds.var]]))
      }
    }
  }
  hist.pred.var <- names(input$hist.pred)[lapply(input$data.atts$hist.pred, 
                                                 "[[", 'var')==targ.var]
  if(!is.null(test$qc_args$arg)){
    #Get whichever input rip corresponds to the hist pred
    #Adjust the target
    input$hist.pred[[hist.pred.var]] <- do.call(test$qc_args$fxn, 
                                                args=list(as.numeric(input$hist.pred[[hist.pred.var]]), 
                                                          test$qc_args$arg))
    input$hist.targ <- do.call(test$qc_args$fxn, 
                               args=list(as.numeric(input$hist.targ), 
                                         test$qc_args$arg))
  }else{
    input$hist.pred[[hist.pred.var]] <- do.call(test$qc_args$fxn, 
                                               args=list(as.numeric(input$hist.pred[[hist.pred.var]])))
    input$hist.targ <- do.call(test$qc_args$fxn, 
                               args=list(input$hist.targ))
  }
#   print('post_transform:')
#   print(lapply(input$hist.pred, 'summary'))
#   print(summary(input$hist.targ))
#   print(lapply(input$fut.pred, 'summary'))
  #stop('examine at this point')
  return(list('input' = input, 'adjusted.output' = out.list))
}

round.negative <- function(num){
  #assumes no 0 values are passed 
  return(ifelse(num > 0, 1, -1))
}
