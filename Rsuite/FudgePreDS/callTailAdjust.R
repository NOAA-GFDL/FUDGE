# =============================================================================
#                            MAIN DRIVER FUNCTION
# =============================================================================

# THIS DOES PRELIMINARY SETUP, LOOPS OVER RIPS & CALLS GENERAL TAIL SCHEME
# FUNCTION (BELOW)

callTailAdjust <- function(test, input, adjusted.output, triggertype=NA_character_, 
                           cutoffs=list()){
  #'Calls a BCSD-style adjustment on quantile-mapped code
  #'based off of the limits to the adjustment of the
  #'MH and MF datasets
  targ.var <- names(input$hist.targ)
  arg.names <- names(test$qc_args)
  nrips <- dim(adjusted.output$ds.out)[4]
  if (test$qc.mask=='on'){
    adjusted.output$qc.mask <- array(NA, dim=dim(adjusted.output$ds.out))
  }
  if('ignore0'%in%arg.names){
    #Hasty adjustment to set missing values to 0 in bcsd correction
    ignore0 <- (test$qc_args$ignore0=='on')
  }else{
    ignore0 <-  FALSE
  }
  
  # SET SIZE FOR DELTA SAMPLE
  if(!('lastNpts' %in% arg.names)){
    delta_sample <- 1
  } else if(test$qc_args$lastNpts == 0) {
    delta_sample <- 1
  } else {
    delta_sample <- test$qc_args$lastNpts
  }
  triggertypes <- c('GfGh', 'cdft', 'cdft_orig') # 'OhGh'
  if(is.na(triggertype)){
    stop(paste("Error in callTailAdjust: triggertype must be specified!"))
  } else if(!(triggertype %in% triggertypes)) {
    stop(paste("Error in callTailAdjust: triggertype must be one of",
               paste(triggertypes, collapse=", ")))
  }
  adjmethods <- c('add', 'mult')
  if(!('adjmethod' %in% arg.names)){
    stop(paste("Error in callTailAdjust: adjmethod must be specified!"))
  } else if(!test$qc_args$adjmethod %in% adjmethods) {
    stop(paste("Error in callTailAdjust: adjmethod must be one of",
               paste(adjmethods, collapse=",")))
  } else {
    adjmethod <- test$qc_args$adjmethod
  }
  arg_list = list(delta_sample = delta_sample, 
                  triggertype = triggertype, 
                  adjmethod = adjmethod)
  MH <- input$hist.pred[[targ.var]]
  OH <- input$hist.targ[[targ.var]]
  for(n in 1:nrips){
    MF.slice <- input$fut.pred[[targ.var]][,,,n,,]
    DS.slice <- adjusted.output$ds.out[,,,n,,]
    #Check for attributes
    if(triggertype == 'cdft' || triggertype=='cdft_orig'){
      if(all(c('high_cutoff', 'low_cutoff') %in% names(cutoffs))){
        #cutoffs should be a list containing two named vectors:
        #high_cutoff and low_cutoff
        arg_list = c(arg_list, list(high_cutoff = cutoffs$high_cutoff[n], 
                                    low_cutoff  = cutoffs$low_cutoff[n]))
        if (triggertype == 'cdft'){
          #limited cdft takes new args
          arg_list <- c(arg_list, list(tail_len = test$qc_args$tail_len, 
                                       trigger = test$qc_args$trigger))
        }
      }else{
        stop(paste("Error in callTailAdjust: triggertype of cdft",
                   "specified, but high_cutoff attribute not set in cutoffs!"))
      }
    }
    #print(arg_list)
    ds.correct <- tail.correct(model.hist = MH, 
                               obs.hist = OH, 
                               model.fut = MF.slice, 
                               ds.fut = DS.slice, 
                               args = arg_list)
    if(test$qc.mask=='on'){
      ds.correct$mask[ds.correct$mask==0] <- NA
      adjusted.output$qc.mask[,,,n,,] <- ds.correct$mask
    }
    if(test$adjust.out=='on'){
      adjusted.output$ds.out[,,,n,,] <- as.numeric(ds.correct$ds)
    }
  }
  return(list('input' = input, 'adjusted.output' = adjusted.output))
}

# =============================================================================
#                   TAIL CORRECT FUNCTION [FOR BOTH SCHEMES]
# =============================================================================

# GENERAL TAIL SCHEME FUNCTION THAT CALLS EITHER SIMPLE (I.E. BCQM/PARM)
# SCHEME OR CDFT SPECIFIC SCHEME AND PERFORMS THE ADJUSTMENTS

tail.correct <-  function(model.hist, obs.hist,  model.fut, ds.fut, args){
  #Fast wrapper for the more complicated tail_cutoff function
  
  # CHOSE BETWEEN TYPES OF SCHEMES
  cond.data = switch(args$triggertype,
                     'GfGh' = model.fut,
                     'cdft_orig' = model.fut,
                     'cdft' = model.fut)
  
  # PREPARE DATA
  cond.missvals <- !is.na(cond.data)
  ds.orig <- ds.fut
  model.hist <- remove.missvals(unlist(model.hist))
  obs.hist <- remove.missvals(unlist(obs.hist))
  model.fut <- remove.missvals(unlist(model.fut))
  ds.fut <- remove.missvals(unlist(ds.fut))
  
  # CHECK IF FEASIBLE -- IF SO CALL APPROPRIATE TAIL SCHEME FUNCTION
  if( any(sapply(list(ds.fut, model.hist, obs.hist), length) == 0) ){
    #Bypass for missing data
    message("Insufficient data for tail adjustment")
    tmp.mask=rep(0, length(ds.orig))
  } else {
    #Calculate high and low tails
    
    # CALL CDFT TAIL FUNCTION [EXTERNAL:  cdft_tail_cutoff.R]
#     print('args before triggertype')
#     print(args)
    if (args$triggertype == 'cdft'){
      #print(args)
      #stop('in cdft call')
      print('calling upper')
      hitail = cdft_limited_tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='upper'))
      print('calling lower')
      lotail = cdft_limited_tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='lower'))
    }else if (args$triggertype == 'cdft_orig'){
      print('calling upper')
      hitail = cdft_tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='upper'))
      print('calling lower')
      lotail = cdft_tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='lower'))
    } else {
      print('calling upper')
      hitail = tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='upper'))
      print('calling lower')
      lotail = tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='lower'))
    }
    #Add missing values of condition back into main
    cond.tmp <- cond.missvals
    cond.tmp[cond.tmp] <- hitail$cond
    hitail$cond <- cond.tmp
    cond.tmp <- cond.missvals
    cond.tmp[cond.tmp] <- (lotail$cond)
    lotail$cond <- cond.tmp
    hitail.pts = hitail$cond >= 1
    lotail.pts = lotail$cond >= 1
    
    ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ## PERFORM ADJUSTMENT BY ADDING OFFSET TO POINTS REQUIRING ADJUSTMENT
    if (args$adjmethod == 'add'){
      ds.orig[hitail.pts] = cond.data[hitail.pts] + hitail$offset
      ds.orig[lotail.pts] = cond.data[lotail.pts] + lotail$offset
    } else if (args$adjmethod == 'mult') { # adjmethod is "mult"
      ds.orig[hitail.pts] = cond.data[hitail.pts]*hitail$offset
      ds.orig[lotail.pts] = cond.data[lotail.pts]*lotail$offset
    } else {
      stop("something has gone terribly WRONG with the supplied adjmethod!!")
    }
    ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # CREATE INDICATORS TELLING WHICH POINTS WERE ADJUSTED?
    tmp.mask <- (hitail$cond)*2 + lotail$cond
    tmp.mask[tmp.mask > 4] = 4
  }
  # print(length(ds.orig))
  # print(length(tmp.mask))
  return(list(ds = ds.orig, mask = (tmp.mask)))
}


# =============================================================================
#           TAIL CUTOFF FUNCTION [FOR 'SIMPLE' (I.E. BCQM/PARM) SCHEME
# =============================================================================


# THIS IS THE "SIMPLE TAIL SCHEME" (I.E. FOR BCQM/PARM)

tail_cutoff <- function(MH, OH, MF, DS, args){
  #'Function for switching from one tail adjustment to another
  #'Currently accepts the arguments 'GfGh' and 'OhGh'  # 'OhGh' NO LONGER USED
  trigger.fxn = switch(args$tail,
                       lower='min',
                       upper='max')
  select.cond = switch(args$tail,
                       lower = '>=',
                       upper = '<=')
  adjust.cond = switch(args$tail,
                       lower='<',
                       upper='>')
  delta_sample = args$delta_sample
  delta.by = switch(args$tail,
                    lower=(1),
                    upper=(-1))
  
  
  # CHECKS FOR TYPE OF TAIL SCHEME -- SEEMS LIKE THIS IS NOT NECESSARY SINCE THIS
  # FUNCTION ONLY PERTAINS TO THE SIMPLE TYPE -- SHOULD NEVER GET HERE OTHERWISE
  #   if(args$triggertype=='GfGh'){
  mf.rank = rank(MF)
  MF  = sort(MF)
  DS = sort(DS)
  #mf.idx = tail(which(MF <= max(MH)), 1)
  
  # CREATE LOGICAL VARIABLE FOR POINTS THAT NEED TO BE ADJUSTED?
  mf.points = do.call(select.cond, list(MF, do.call(trigger.fxn, list(MH))))
  #     trigger.cond = do.call(adjust.cond, list(do.call(trigger.fxn, list(MF)),
  #                                              do.call(trigger.fxn, list(MH))))
  
  
  mf.overlap <- max(min(MF),min(MH)) <= min(max(MF),max(MH))
  #print(trigger.cond)
  # CHECKS IF CONDITIONS ARE MET FOR ADJUSTMENT
  if(any(mf.points==TRUE, na.rm=T)){
    # DETERMINE POINTS THAT NEED TO BE ADJUSTED
    mf.idx = do.call(trigger.fxn, list(which(mf.points)))
    #Check on behavior when Max(MH) == max(MF)
    #do.call(trigger.fxn, list(MH))
    mf.tmp = MF[mf.idx]
    ##get delta
    if(mf.overlap){
      delta.idx = seq(from=mf.idx, by=delta.by, length.out=delta_sample)
      # NOT SURE WHAT THIS IS DOING IN RESETTING 'delta.idx'
      # MAYBE FEWER THAN SPECIFIED NUMBER OF VALUES TO USE ARE AVAILABLE?
      if(any(delta.idx < 1)){
        #print(trigger.fxn)
        print("Overlap too small of delta_sample; resizing")
        delta.idx <- delta.idx[delta.idx >= 1]
      }
      ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ## COMPUTES THE DELTA (I.E. ADDITIVE TERM FOR ADJUSTMENT) AS AN AVERAGE
      adjmethod <- args$adjmethod
      print(paste0("Adjusting tail with ", adjmethod))
      ## check arg for delta or mult
      if (adjmethod == 'add') {
        offset <- mean(DS[delta.idx] - MF[delta.idx])
      } else if (args$adjmethod == 'mult'){ # adjmethod is "mult"
        ## Check for division by zero or very small number, replace with "largest seen" ratio
        mult.ifzero <- max(DS[-delta.idx]/MF[-delta.idx])
        mult.vec <- DS[delta.idx]/MF[delta.idx]
        if (any(mult.vec > mult.ifzero, na.rm=T)) {
          print("Some calculated multipliers were too large, replacing with max inside tails.")
          mult.vec[mult.vec > mult.ifzero] <- mult.ifzero
        }
        offset <- mean(mult.vec)
      } else {
        stop("something is wrong with adjmethod supplied")
      }
      ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      #print(paste("Delta:", delta))
      ##get selection
      mf.cond = do.call(adjust.cond, list(MF[mf.rank], mf.tmp))
      
      # UNABLE TO PERFORM TAIL ADJUSTMENT -- INSTEAD USING SIMPLE BIAS CORRECTION
      # THIS ARISES WHEN THE DIFFERENCE IN MEANS BETWEENGCMh & GCMf SI SO LARGE THAT
      # THE DISTRIUBTIONS DON'T OVERLAP -- OCCURS SOMETIMES OVER THE ATLANTIC
      # WHERE THE VARIANCE IS ESPECIALLY SMALL
    } else {
      if (args$adjmethod == 'add') {
        print("Selecting mean(OH) - mean(MH)")
        offset = mean(OH) - mean(MH)
      } else { # alternative correction in "mult" land
        print("Selecting mean(OH)/mean(MH)")
        offset = mean(OH)/mean(MH)
      }
      #print(mf.tmp)
      #print(adjust.cond)
      mf.cond <- do.call(select.cond, list(MF[mf.rank], mf.tmp))
      mf.cond <- mf.cond * 4
    }
    #DS[MF > mf.tmp] = MF[MF > mf.tmp] - delta
    return(list(cond=mf.cond, offset=offset))
  }else{
    print("No adjust")
    cond=rep(FALSE, length(MF))
    offset=0
    return(list(cond=cond, offset=offset))
  }
}
