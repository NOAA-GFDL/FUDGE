# =============================================================================
#                            MAIN DRIVER FUNCTION
# =============================================================================

# THIS DOES PRELIMINARY SETUP, LOOPS OVER RIPS & CALLS GENERAL TAIL SCHEME
# FUNCTION (BELOW)

callTailAdjust <- function(test, input, adjusted.output, triggertype=NA_character_,
                           cutoffs=list(), tmasks=list(time_mask_overlap=F), missval.args=list()){
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
  #Check for OH and MH sample size
  #note: right now, these are always univariate
  if ("pr_opts" %in% names(missval.args)){
#     print("pr_opts found")
#     print(w)
#     print(summary(tmasks$OH$masks))
#     print(summary(tmasks$OH$masks[[1]]))
#     print(summary(tmasks$MH$masks))
    if (tmasks$apply_tmasks){
      oh_pr <- apply.fudge.tmask(data=missval.args$pr_opts$OH, mask=tmasks$OH$masks[[w]])
      oh_ss <- sum(!is.na(oh_pr))
      mh_pr <- apply.fudge.tmask(data=missval.args$pr_opts$MH, mask=tmasks$MH$masks[[w]])
      mh_ss <- sum(!is.na(mh_pr))   
    }
  }else{
    #Assume you are temperature-like
    oh_ss <- sum(!is.na(OH))
    mh_ss <- sum(!is.na(MH))
  }
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
    }else{
      #The simple tail adjust now takes the oh and mh sample size as args
      arg_list <- c(arg_list, list(oh_ss = oh_ss, mh_ss = mh_ss))
    }
    #print(arg_list)
    ds.correct <- tail.correct(model.hist = MH,
                               obs.hist = OH,
                               model.fut = MF.slice,
                               ds.fut = DS.slice,
                               args = arg_list, tmasks=tmasks)
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

tail.correct <-  function(model.hist, obs.hist,  model.fut, ds.fut, args, tmasks){
  #Fast wrapper for the more complicated tail_cutoff function
  
  #Added 12-9-16
  #If you are going to be using overlapping masks,
  #you need to trim your samples for this method
#   if (tmasks$time_mask_overlap){
#     #print(summary(tmasks))
#     model.fut <- apply.any.mask(tmasks$trim$masks[[w]], model.fut, dim='time')
#   }
  #CEW removed 2017-12-6: as section5.R and the tail adjust currently call this
  #function (no tmasks, default=list(timas_mask_overlap=F)), this NEVER
  #gets invoked. Turning off to avoid confusion.
  
  # CHOSE BETWEEN TYPES OF SCHEMES
  cond.data = switch(args$triggertype,
                     'GfGh' = model.fut,
                     'cdft_orig' = model.fut,
                     'cdft' = model.fut)
  
  # PREPARE DATA
  cond.missvals <- !is.na(cond.data)
  ds.orig <- ds.fut
  ds.idx <- is.na(ds.orig)
  #Okay - we need to distinguish between missvals from the masks and missvals
  #due to wetdays present/absent
  model.hist <- remove.missvals(unlist(model.hist))
  obs.hist <- remove.missvals(unlist(obs.hist))
  model.fut <- remove.missvals(unlist(model.fut))
  ds.fut <- remove.missvals(unlist(ds.fut))
  
  # CHECK IF FEASIBLE -- IF SO CALL APPROPRIATE TAIL SCHEME FUNCTION
  if( any(sapply(list(ds.fut, model.fut, model.hist, obs.hist), length) < 2) ){
    #You need at least 2 points to do this tail adjust
    #Bypass for missing data
    message("Insufficient data for tail adjustment")
    tmp.mask=rep(0, length(ds.orig))
  } else {
    #Calculate high and low tails
    if (args$triggertype == 'cdft'){
      #print('calling upper')
      hitail = cdft_limited_tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='upper'))
      #print('calling lower')
      lotail = cdft_limited_tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='lower'))
    }else if (args$triggertype == 'cdft_orig'){
      hitail = cdft_tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='upper'))
      lotail = cdft_tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='lower'))
    } else {
      hitail = tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='upper'))
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
    #tmp.mask <- (hitail$cond)*2 + lotail$cond
    #CEW commented out - we now distinguish cases up to 6
    #tmp.mask[tmp.mask > 4] = 4
    tmp.mask <- hitail$cond + lotail$cond
  }
  return(list(ds = ds.orig, mask = (tmp.mask)))
}


# =============================================================================
#           TAIL CUTOFF FUNCTION [FOR 'SIMPLE' (I.E. BCQM/PARM) SCHEME
# =============================================================================


# THIS IS THE "SIMPLE TAIL SCHEME" (I.E. FOR BCQM/PARM)

tail_cutoff <- function(MH, OH, MF, DS, args){
#'Simple tail adjustment (for BCQM/PARM)
  # Would this be more straightforward than switches? -D1A 2017-12-04
  if (args$tail == "lower") {
    trigger.fxn <- 'min'
    select.cond <- '>='
    adjust.cond <- '<'
    delta.by <- 1
  } else if (args$tail == "upper") {
    trigger.fxn <- 'max'
    select.cond <- '<='
    adjust.cond <- '>'
    delta.by <- -1
  }

  mf.rank = rank(MF, ties='first')
  MF = sort(MF)
  DS = sort(DS)


  # CREATE LOGICAL VARIABLE FOR POINTS THAT NEED TO BE ADJUSTED? ???
  #     mf.points = do.call(select.cond, list(MF, do.call(trigger.fxn, list(MH))))
  # D1A 2017-12-04: review of implementation, this needs to be "inside" the MF distribution
  le.maxMH <- MF <= max(MH, na.rm=TRUE)
  ge.minMH <- MF >= min(MH, na.rm=TRUE)
  mf.points <- le.maxMH & ge.minMH

  # are there...?
  points.to.be.adjusted <- switch(args$tail, lower=!all(ge.minMH), upper=!all(le.maxMH))
  # Do Mh and Mf overlap at all?
  mf.overlap <- max(min(MF),min(MH)) <= min(max(MF),max(MH))
  
  # CHECKS IF CONDITIONS ARE MET FOR ADJUSTMENT
  if(any(points.to.be.adjusted)){
    # DETERMINE POINTS THAT NEED TO BE ADJUSTED
    # which method is used?
    print(paste("adjusting the", args$tail, "tail"))
    adjmethod <- args$adjmethod
    
    if(mf.overlap & any(mf.points)){
    #D1A has below version. Check with him? 
    #if(mf.overlap){
      # get the index of the last good point in MF
      mf.idx = do.call(trigger.fxn, list(which(mf.points)))
      # get the MF value of the last googd point
      mf.tmp = MF[mf.idx]
      delta.idx = seq(from=mf.idx, by=delta.by, length.out=args$delta_sample)
      # NOT SURE WHAT THIS IS DOING IN RESETTING 'delta.idx'
      # MAYBE FEWER THAN SPECIFIED NUMBER OF VALUES TO USE ARE AVAILABLE?
      if(any(delta.idx < 1)){
        #print(trigger.fxn)
        print("Overlap too small for delta_sample upper; resizing")
        delta.idx <- delta.idx[delta.idx >= 1]
      }
      #CEW 2018-01-09: need corresponding fix for the lower tail
      if(any(delta.idx > length(MF))){
        print("Overlap too small for delta_sample lower; resizing")
        delta.idx <- delta.idx[delta.idx <= length(MF)]
      }
      ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ## COMPUTES THE DELTA (I.E. ADDITIVE TERM FOR ADJUSTMENT) AS AN AVERAGE
      #print(paste0("Adjusting tail with ", adjmethod))
      ## check arg for delta or mult
      if (args$adjmethod == 'add') {
        #### It's THIS easy for additive!  The rest is for ratio!
        offset <- mean(DS[delta.idx] - MF[delta.idx])
      } else if (args$adjmethod == 'mult'){
        ## We will create ratio adjustment. But first!
        ## Check for division by zero or very small number, replace with "largest seen" ratio
        # get the indices of the points "inside the tails"
        if (args$tail=='upper') {
          inside.tail <- seq(min(which(mf.points)),(min(delta.idx)-1))
        } else if (args$tail=='lower') {
          inside.tail <- seq((max(delta.idx)+1), max(which(mf.points)))
        }
        #CEW added 12-12-16; needed to remove negative/0 subscripts
        if (any(inside.tail < 1)){
          inside.tail <- inside.tail[inside.tail > 0]
          if (length(inside.tail) == 0){
            stop("Error in tail_cutoff: At least one inside index should be greater than 0!")
          }
        }
        #CEW 2018-01-09: Looking at the 
        # FIND LARGEST RATIO INSIDE TAILS
        mult.ifzero <- max(DS[inside.tail]/MF[inside.tail], na.rm=TRUE)
        # CALCULATE ADJUSTMENT RATIO
        mult.vec <- DS[delta.idx]/MF[delta.idx]
        #CEW added 12-12-16; mult.vec can, in fact, be NA sometimes
        #mult.vec <- mult.vec[!is.na(mult.vec)]
        #mult.ifzero <- mult.ifzero[!is.na(mult.ifzero)]
        if (length(mult.vec) == 0){
          if (length(mult.ifzero) > 0){
            mult.vec <- mult.ifzero
          }else{
            print('Some calculated multiples were NA; returning 1')
            mult.vec <- 1
            mult.ifzero <- 1
          }
        }
        if (is.na(mult.vec[1])){
          mult.vec <- 9999
        }
        if (any(mult.vec > mult.ifzero)) {
          print("Some calculated multipliers were too large, replacing with max inside tails.")
          mult.vec[mult.vec > mult.ifzero] <- mult.ifzero
        }
        offset <- mean(mult.vec)
      } else {
        stop("something is wrong with adjmethod supplied")
      }
      cond = do.call(adjust.cond, list(MF[mf.rank], mf.tmp))
      if (args$tail =='upper'){
        cond.val <- 2
      }else{
        cond.val <- 1
      }
    } else {
      # UNABLE TO PERFORM TAIL ADJUSTMENT -- INSTEAD USING SIMPLE BIAS CORRECTION
      # THIS ARISES WHEN THE DIFFERENCE IN MEANS BETWEENGCMh & GCMf IS SO LARGE THAT
      # THE DISTRIBUTIONS DON'T OVERLAP -- OCCURS SOMETIMES OVER THE ATLANTIC
      # WHERE THE VARIANCE IS ESPECIALLY SMALL
      
      #We are either in the non-overlapping case or the goalpost case, 
      #(goalpost = all MF values are either < min(MH) or > max(MH))
      #and in either case the adjustment will be the same
      if (args$adjmethod == 'add') {
        #offset = mean(OH) - mean(MH)
        offset = offset = ( sum(OH) / args$oh_ss ) - ( sum(MH) / args$mh_ss ) 
      } else { # alternative correction in "mult" land
        #offset = mean(OH)/mean(MH)
        offset = (sum(OH) / args$oh_ss ) / ( sum(MH) / args$mh_ss) 
      }
#       print(mean(OH)  - mean(MH))
#       print((sum(OH) / args$oh_ss)  - (sum(MH) / args$mh_ss) )
#       stop('examine this out')
      if (mf.overlap){
        print("Goalpost case")
        cond = do.call(adjust.cond, list(MF[mf.rank], do.call(trigger.fxn, list(MH))))
        cond.val <- 6
      }else{
        print("Non-overlapping case")
        cond <- rep(1, length(MF))
        #This ordering is the reverse of what is present from the tail adjust,
        #but results in fewer changed values from the prior encoding
        if (args$tail == 'upper'){
          cond.val <- 4
        }else{
          cond.val <- 5
        }
        cond <- rep(1, length(MF))
      }
    }
    #return(list(cond=mf.cond, offset=offset))
  }else{
    print("No adjust")
    cond=rep(FALSE, length(MF))
    offset=0
    cond.val <- 0
    #return(list(cond=cond, offset=offset))
  }
return(list(cond = cond * cond.val, offset = offset))
}

