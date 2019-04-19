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
callS5Adjustment <- function(s5.instructions = list('na'),
                             data = NA, #downscaled data - from this run or another
                             data.atts = NA,
                             hist.pred = NA, 
                             hist.targ = NA, 
                             fut.pred = NA,
                             ds.atts = ds.atts,
                             missval.args = missval.args,
                             tmasks=list(time_mask_overlap=F),
                             verbose=FALSE){
  input<- list('hist.pred'=hist.pred, 'hist.targ'=hist.targ, 'fut.pred'=fut.pred, 'data.atts'=data.atts)
  qc.mask <- NULL  #If no mask generated, will keep being null forever
  adjusted.output <- list("ds.out" = data, "qc.mask" = qc.mask)
  out <- list('input' = input, 'adjusted.output' = adjusted.output)
  for(element in 1:length(s5.instructions)){
    test <- s5.instructions[[element]]
    #print(test$type)
    out.dim <- unlist(unique(lapply(out$adjusted.output, 'attr', 'dim')))
    #source('~/Code/s8_testing/adjust_pr/pairwise_adjust_pr_s5.R')
    out <- switch(test$type,
                  'sdev' = callSdev(test, out$input, out$adjusted.output),
                  'sdev2' = callSdev2(test, out$input, out$adjusted.output),
                  'SBiasCorr' = callSBCorr(test, out$input, out$adjusted.output),
                  'flag.neg' = callFlagNegativeValues(test, out$input, out$adjusted.output),
                  'pr_out' = callAdjustPrecipS5(test, out$input, out$adjusted.output),
                  'Nothing' = callNoMethod(test, out$input, out$adjusted.output),
                  'add.one' = callAddOne(test, out$input, out$adjusted.output),
                  'add.five' = callAddFive(test, out$input, out$adjusted.output),
                  'BCQM.tail.adj' = callBCSDCorrect(test, out$input, out$adjusted.output),
                  'simpletailadj' = callTailAdjust(test, out$input, out$adjusted.output, triggertype='GfGh', missval.args=missval.args, tmasks=tmasks),
                  'limtailadj'    = callTailAdjust(test, out$input, out$adjusted.output, 
                                                     triggertype='limited', cutoffs=ds.atts),
                  'cdft_simpletailadj' = callTailAdjust(test, out$input, out$adjusted.output, 
                                                        triggertype='cdft_orig', cutoffs=ds.atts),
                  'backtransform' = callPRBacktransfrom(test, out$input, out$adjusted.output),
                  'mapMFOrder' = callMapMFOrder(test, out$input, out$adjusted.output),
                  stop(paste('Adjustment Method Error: method', test$type,
                             "is not supported for callS5Adjustment. Please check your input.")))
#     print('summary of adjusted output:')
#     print(summary(as.vector(out$adjusted.output$ds.out)))
#     print(summary(as.vector(out$adjusted.output$ds.out[!is.na(out$adjusted.output$ds.out)])))  
    #Make sure no dimension information gets lost
    for(el in names(out$input)){
      #print("This step should ensure dim info does not get lost")
      for(var in names(out$input[[el]])){
        dim(out$input[[el]][[var]]) <- dim(input[[el]][[var]])
      }
    }
    #For the moment, asusme that all data is either of output
    #dims or NULL; this should fail spectacularly 
    #if there are outputs with more than one dimensionality
    for(el in names(out$adjusted.output)){
      #print("settings dim vals to out.dim")
      if (!is.null(adjusted.output[[el]])){
        #if (is.null(dim(adjusted.output[[el]]))){
          dim(out$adjusted.output[[el]]) <- out.dim
        #}
      }
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
  if(test$make_qc_mask){
    out.list$qc.mask <- mask.vec
  }
  #Fom now on, we assume we are always adjusting
    adjust.vec <- ifelse( (is.na(mask.vec)),
                          yes=ifelse( (1==sign(out.mean-adjusted.output$ds.out)),
                                      out.mean-out.comp, out.mean+out.comp ),
                          no=adjusted.output$ds.out)
    out.list$ds.out <- adjust.vec
  return(list('input' = input, 'adjusted.output' = out.list))
}

callSBCorr <- function(test, input, adjusted.output, verbose=FALSE){
  #Outputs a mask where NA values show flagged data and
  #1's show good data
  #'Assumes one predictor variable and one target variable
  #'But multiple ensemble members are supported
  
  #print(str(input))
  if(verbose){print("entering simple bias correction function")}
  if(!is.null(test$qc_args$toplim) && !is.null(test$qc_args$botlim)){
    toplim <- test$qc_args$toplim
    botlim <- test$qc_args$botlim
  }else{
    stop("Section 5 Adjustment Error: Arguments toplim and botlim are not present for the SBiasCorr function. Please check your XML.")
  }
  #target.var <- unlist(lapply(input$data.atts$hist.targ, "[[", 'var'))
  target.var <- names(input$hist.targ)[1]
  ds.vars <- names(adjusted.output$ds.out)
  #Unlist the historic predictors
  hist.select <- c(names(input$hist.pred)==target.var)
  hist.pred <- unlist(input$hist.pred[hist.select])
  out.list <- adjusted.output #Everything should be returned as-is, unless something neat happens
  for (ds.var in ds.vars){
    #There can be more than pne future predictor for the ensemble
    fut.pred <- unlist(input$fut.pred[[ds.var]])
    #compute difference for all time values
    hist.bias <- mean(hist.pred-input$hist.targ[[target.var]], na.rm=TRUE)
    fut.targ <- fut.pred-hist.bias
    tmp.diff <- as.numeric(adjusted.output$ds.out) - fut.targ
    mask.vec <- ifelse( (botlim <= (tmp.diff) &
                           (tmp.diff) < toplim),
                        yes=1, no=NA)
    if(test$make_qc_mask){
      out.list$qc.mask[[ds.var]] <- mask.vec
    }
   #assume always adjusting
      adjust.vec <- ifelse( (is.na(mask.vec)), yes=fut.targ, no=adjusted.output$ds.out)
      out.list$ds.out <- adjust.vec
  }
  return(list('input' = input, 'adjusted.output' = out.list))
}

callFlagNegativeValues <- function(test, input, adjusted.output){
  #Flags negative values in the downscaled output with NA
  #with the expectation that they may get adjusted later.
  #You don't actually need the RIPs; just the variable names for ds data
  #Initialize output structure
  out.list <- adjusted.output
  mask.vec <- ifelse(adjusted.output$ds.out >= 0, yes=1, no=0)
  if(test$make_qc_mask){
    print('adding qc mask in flag.neg')
    out.list$qc.mask <- mask.vec
    dim(out.list$qc.mask) <- dim(adjusted.output$ds.out)
    out.list$qc.mask[mask.vec==0] <- NA
  }
  #if(test$adjust.out=='on'){
    adjust.vec <- ifelse( (mask.vec==0), yes=0, no=adjusted.output$ds.out)
    dim(adjust.vec) <- dim(adjusted.output$ds.out)
    out.list$ds.out <- adjust.vec
  #}
  return(list('input' = input, 'adjusted.output' = out.list))
}


callAdjustPrecipS5 <- function(test, input, adjusted.output){
  
  if (test$qc_args$pr_adjmethod %in% c('exact', 'approx')){
    return(callPRPostproc(test, input, adjusted.output))
  }else if (test$qc_args$pr_adjmethod %in% c('eqfreq', 'none')){
    return(adjust_pr_s5(test, input, adjusted.output))
  }else{
    stop(paste("Error in callAdjustPrecipS5: pr_adjmethod should be one of", 
               "exact, approx, or eqfreq, but was", test$qc_args$pr_adjmethod,"!"))
  }
}

callPRPostproc <- function(test, input, adjusted.output){
  #Performs the adjustments needed for post-downscaling precipitation
  #on the downscaled ouput, including a threshold adjustment for drizzle
  #bias and conservation of the total precipitation per time range
  arg.names <- names(test$qc_args)
  #This code is not getting used anywhere else, and I can't remmber why it was supposed to be important
  if(!('thold'%in%arg.names) || !('conserve'%in%arg.names)){
    stop("Error in PR post-processing: One or more of thold or conserve not present in arguments to function")
  }
  if (test$qc_args$ignore0=='on'){
    message(paste('Applying wetday mask to ds output'))
    if (!ds.ref$downscale_oh){
      #non-OH downscaling is straightforward:
      #get center of possible 9-point mask and apply to ds output
      ds.dim <- dim(adjusted.output$ds.out)
      fut.prmask <- get.xy.center(test$qc_args$fut.prmask)
      adjusted.output$ds.out[!fut.prmask] <- 0
      dim(adjusted.output$ds.out) <- ds.dim
    }else{
      #Oh downscaling is more complicated, especially when leapdays get involved
      obs.prmask <- get.xy.center(test$qc_args$targ.prmask)
      oh.timelen <- dim(input$hist.targ[[1]])[6]
      mf.timelen <- dim(input$fut.pred[[1]])[6]
      nrips <- dim(adjusted.output$ds.out)[4]
      start <- 1
      #For better or worse, OH gets adjusted by nrips present
      for(n in 1:nrips){
        #After either step is complete, index the mask to the future timseries
        #using index.delta
        OH.idx <- tmasks$OH_idx[[n]]
        loop.ds.prmask <- index.delta(tmasks$hist.leapmask[OH.idx], 
                                      tmasks$fut.leapmask, 
                                      rep(1, oh.timelen), 
                                      rep(1, mf.timelen), 
                                      obs.prmask[,,,1,,], 
                                      tmasks$idx_year_div[[n]],tmasks$fut.year.div)
        #obs.prmask right now does not support more than one
        #ensemble member.
        adjusted.output$ds.out[,,,n,,][!loop.ds.prmask] <- 0
      }
    }
  }else{
    message("Not applying wetday mask. Output may have more days without pr than expected.")
  }
  adjusted.pr <- pr.postproc(pr.data  = adjusted.output$ds.out, 
                             pr.units = input$data.atts$pr_oh_units, 
                             pr.index = test$qc_args$thold, 
                             pr.conserve = (test$qc_args$conserve=='on'))
  adjusted.output$ds.out <- adjusted.pr$ds.out
  if(test$make_qc_mask){
    adjusted.output$qc.mask <- get.xy.center(adjusted.output$qc.mask)
  }
  return(list('input' = input, 'adjusted.output' = adjusted.output))
}


pr.postproc <- function(pr.data, pr.units, pr.index, pr.conserve){
  #Internal helper function for reading the pr post-processing function
  #clearly and concisely
  #Mask by threshold
  out.mask <- MaskPRSeries(pr.data, pr.units, pr.index)
  if(pr.conserve){
    pr.dim <- dim(pr.data)
    for(i in 1:pr.dim[1]){
      for(j in 1:pr.dim[2]){
        for (n in 1:pr.dim[4]){
          data.vec <- pr.data[i,j,,n,,]
          mask.vec <- out.mask[i,j,,n,,]
          data.vec[!is.na(data.vec)] <- conserve.prseries(data=data.vec[!is.na(data.vec)], 
                                                          mask=mask.vec[!is.na(mask.vec)])
          pr.data[i,j,,n,,] <- data.vec
        }
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
    #if(test$adjust.out=='on'){
      adjusted.output$ds.out <- (adjusted.output$ds.out + 1)
    #}
    if(test$make_qc_mask){
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
    #if(test$adjust.out=='on'){
      adjusted.output$ds.out <- (adjusted.output$ds.out + 5)
    #}
    if(test$make_qc_mask){
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
    ds.correct <- bcsd.correct(model.hist = input$hist.pred[[hist.pred]],
                               obs.hist = input$hist.targ,
                               model.fut = input$fut.pred[[fut.pred]],
                               obs.fut = adjusted.output$ds.out,
                               sample_size = sample_size)
    if(test$make_qc_mask){
      qc.tmp <- ifelse(mapply(both.na, ds.correct, as.numeric(adjusted.output$ds.out)),
                       1, 0)
      #print(summary(as.vector(qc.tmp)))
      non.missing <- which(qc.tmp==0)
      ds.tmp <- ifelse(ds.correct==as.numeric(adjusted.output$ds.out),
                       1, NA)
      qc.tmp[non.missing] <- ds.tmp[non.missing]
      #print(summary(as.vector(qc.tmp)))
      adjusted.output$qc.mask[[ds.var]] <- qc.tmp
    }
    #assume always adjusting
      adjusted.output$ds.out <- as.numeric(ds.correct)
    
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
  obs.fut[smallx.tmax] <- model.fut[smallx.tmax] - min.delta.tmax
  return(obs.fut)
}

callPRBacktransfrom <- function(test, input, adjusted.output){
  #calls the backtransform for the downscaled data
  #Note: this adjusts data only; there is never going to be
  #a qc mask provided from this data (at least for now)
  #ds.vars <- names(adjusted.output$ds.out)
  
  ds.var <- names(input$hist.targ)
  out.list <- adjusted.output
  if(!is.null(test$qc_args$arg)){
    #If there are arguments to the transform, make the call with the args
    out.list$ds.out <- do.call(test$qc_args$fxn, 
                               args=list(as.numeric(out.list$ds.out), 
                                         test$qc_args$arg))
    input$fut.pred[[ds.var]] <- do.call(test$qc_args$fxn, 
                                        args=list(as.numeric(input$fut.pred[[ds.var]]), 
                                                  test$qc_args$arg))
    input$hist.pred[[ds.var]] <- do.call(test$qc_args$fxn, 
                                         args=list(as.numeric(input$hist.pred[[ds.var]]), 
                                                   test$qc_args$arg))
    input$hist.targ[[ds.var]] <- do.call(test$qc_args$fxn, 
                                         args=list(as.numeric(input$hist.targ[[ds.var]]), 
                                                   test$qc_args$arg))
  }else{
    #If there are no arguments to the transform, you make the call differently
    out.list$ds.out <- do.call(test$qc_args$fxn, 
                               args=list(as.numeric(out.list$ds.out)))
    input$fut.pred[[ds.var]] <- do.call(test$qc_args$fxn, 
                                        args=list(as.numeric(input$fut.pred[[ds.var]])))
    input$hist.pred[[ds.var]] <- do.call(test$qc_args$fxn, 
                                         args=list(as.numeric(input$hist.pred[[ds.var]])))
    input$hist.targ[[ds.var]] <- do.call(test$qc_args$fxn, 
                                         args=list(as.numeric(input$hist.targ[[ds.var]])))
  }
  return(list('input' = input, 'adjusted.output' = out.list))
}

round.negative <- function(num){
  #assumes no 0 values are passed
  return(ifelse(num > 0, 1, -1))
}

callMapMFOrder <- function(test, input, adjusted.output){
  #'Maps the downscaled values to the ransk of the
  #'order in MF. Since this is meant to be called as an outloop
  #'section 5 operation, there's less checking of missing values - 
  #'since this is SUPPOSED to be called only for CFQM (which is univariate), 
  #'we should not be too worreid about missing values and making them
  #'blindingly obvious should be sufficient for now.
  out.list <- adjusted.output
  nrips <- dim(input$fut.pred[[1]])[4]
  for (n in 1:nrips){
    #input$fut.pred
    MF.rip <- input$fut.pred[[1]][,,,n,,]
    MF.rank <- rank(MF.rip, na.last=F, ties='min')
    #Determine overlap in MF and DS wetday frequency, if applicable.
    #print(which(is.na(sort(MF.rip, na.last = F))))
    which.dry <- tail(which(is.na(sort(MF.rip, na.last = F))), n=1)

    MF.dry.freq <- which.dry/length(MF.rank)
    OH.dry.freq <- sum((out.list$ds.out[,,,n,,]==0))/length(out.list$ds.out[,,,n,,])
    sort.ds.out <- sort(out.list$ds.out[,,,n,,])
    #out.list$ds.out[,,,n,,] <- sort.ds.out[MF.rank]
    
#     if (MF.dry.freq > OH.dry.freq){
#       #If thre are more dry days in the future than the roughly time-synchronous
#       #historical
#       diff.freq <- MF.dry.freq - OH.dry.freq
#       diff.num  <- floor(diff.freq * length(MF.rank))
#       ds.max <- sort.ds.out[floor(OH.dry.freq * length(MF.rank)) + 1]
#       print("before alter.vals")
#       print(diff.num)
#       print(which.dry)
#       alter.vals <- alter.ds.diff(diff.num, which.dry, 
#                                   sort.ds.out[1], ds.max)
#       sort.ds.out[alter.vals$idx] <- alter.vals$vals
#     }
#     tlev <- 1:length(MF.rank)


    #out.list$ds.out[,,,n,,] <- sort.ds.out[MF.rank]
    x <- out.list$ds.out[,,,n,,]
    y <- MF.rip
# print(summary(x))
# print(length(x))
# print(summary(y))
# print(length(y))
    out.list$ds.out[,,,n,,] <- sort(x)[rank(y)]
#print(summary(out.list$ds.out[,,,n,,]))
#stop('examine this output')

#         plot(tlev, input$fut.pred[[1]][,,,n,,], #ylim=c(0,0.0008), 
#              lty=2,
#              main=paste("rank for ens", n))
#         plot(tlev, out.list$ds.out[,,,n,,], col='red')
#         ds.sort <- sort.ds.out
#         lines(tlev, ds.sort[MF.rank], col='blue')
  }
  return(list('input' = input, 'adjusted.output' = out.list))
}

alter.ds.diff <- function(diff.num, diff.max, min.val, max.val){
  set.seed(ival * 100 + jval * 10 + w)
  ds.idx <- sample(x=diff.max:diff.max+diff.num, size=diff.num, replace=F)
  #ds.vals <- runif(diff.num, min.val, max.val)
  ds.vals <- rep(0, diff.num)
  return(list(idx=ds.idx, vals=ds.vals))
}
