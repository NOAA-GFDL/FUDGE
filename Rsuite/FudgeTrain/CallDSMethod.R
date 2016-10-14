#CallDSMethod.R
#' CallDSMethod
#' Calls a downscaling method specified as ds.method and returns the result of training it 
#' upon train.predict and train.target, and then running the relvant equation upon
#' esd.gen. No cross-validation is used for these methods. 
#' 
#' @param ds.method: a string representation of the downscaling data to be used. 
#' @param train.predict: A list of vectors of train.predictor data. For any downscaling run, 
#' there can be multiple predictor datasets - both in a multivariate sense and a multiple point
#' sense.
#' @param train.target: a vector of train.target data. For any downscaling run, there should 
#' only ever be one target dataset of one variable length.
#' @param esd.gen: A list of lists of vectors of esd generation data. For any downscaling run, 
#' if we attempt to train the data and apply it as separate steps, it should be possible to 
#' apply that training to several possible realizations - each of which is effectively a 
#' train.predict data structure. 
#' @param args = NULL: a list of arguments to be passed to the downscaling function.
#' Defaults to NULL (no arguments)
#' @param ds.var: The target variable. Can be used to key off of some methods. 
#' @examples 
#' @references \url{link to the FUDGE API documentation} 
#' TODO: Find a better name for general.bias.corrector
#' TODO: Modify methods other than simple.lm for this kind of dataset manipulation. 
#' TODO: add explicit rather than implicit multivariate support
#' TODO: Find better way to initialize the output storage vectors
#' TODO: Seriously, THERE HAS GOT TO BE A LESS COMPLEX WAY
#' 


callDSMethod <- function(method, OH, MH, MF, args, 
                         missvals="distributional", 
                         minsample=1, missval.args=list(), 
                         train_qcmask=F){
  #Remove missing values
#   missval.out <- remove.FUDGE.missvals(OH, MH, MF, 
#                                        minsample, missval.args, 
#                                        missval.method='distributional', 
#                                        return_qcmask = train_qcmask)
#   OH <- missval.out$OH
#   MH <- missval.out$MH
#   MF <- missval.out$MF
  #don't bother assigning status for now
  #Call downscaling function
  ds.out <- switch(method, 
                   'lm'      = callMultiLM(OH, MH, MF, args), 
                   'lmstep'  = callStep.lm(OH, MH, MF, args), 
                   'CDFt'    = callCDFt(OH, MH, MF, args, ds.method="CDFt", train_qcmask), 
                   'CDFtmod' = callCDFt(OH, MH, MF, args, ds.method="CDFtmod", train_qcmask),
                   'PARM'    = callPARM(OH, MH, MF, args),
                   'BCQM'    = callQM(OH, MH, MF, args, type='BCQM'),
                   'EDQM'    = callQM(OH, MH, MF, args, type='BCQM'),
                   'EDQM'    = callQM(OH, MH, MF, args, type='BCQM'),
                   'DeltaSD' = callDeltaSD(OH, MH, MF, args),
                   ReturnDownscaleError(method)
  )
  #Restore missing values to original place now takes place within method
  #as a h-release expidency shortcut, combine ds.out and missval qc masks
  if (train_qcmask){
    ds.out$qc <- ds.out$qc * missval.out$qcmask # 1 and NA; NAs propagate
  }
  return(list(ds=ds.out$ds, qc=ds.out$qc))
}

# CallDSMethod <- function(ds.method, train.predict, train.target, esd.gen, args=NULL, ds.var='irrelevant', 
#                          att.table=NA, missval.handling='distributional', min.present=1, train_qc_mask=FALSE){
#   #   save(file='/home/cew/Code/testing/sample_callds_input.R', list=c('train.predict', 'train.target', 'esd.gen', 'att.table'))
#   #   stop('comment the line above this')
#   rip.list <- lapply(att.table$fut.pred, '[[', 'rip')
#   rips <- unique(rip.list)
#   esd.index <- list()
#   if('MF'%in%ds.names){
#     for(rip in rips){
#       #Insert check for least number of present vals in here; store var name for later b/c 
#       #that check still has potential to be very useful
#       esd.index[[rip]] <- esd.gen[rip.list==rip][[1]]
#     }
#   }else if('OH' %in% ds.names){ #if downscaling with Delta or CFQM
#     for(rip in rips){
#       esd.index[[rip]] <- rep(train.target, length.out=length(esd.gen[rip.list==rip][[1]]))
#     }
#   }
#   if(missval.handling=='synchronous'){
#     #This should be pre-initialized, even if it's likely to be small
#     ds.input <- assess.missvals(train.target, train.predict, esd.gen, type=missval.handling, 
#                                 ds.method, args, min.present, att.table=att.table, qcmask=train_qc_mask)
#     if(((ds.input$status > 0 && 'MF'%in%train.names) || ds.input$status==130) && ds.input$status !=120){
#       #If the number of missing values has been altered by the fxn, get a new addition index
#       for(rip in rips){
#         esd.index[[rip]] <- ds.input$MF[rip.list==rip][[1]]
#       }
#     }
#     if(ds.input$status < 200 && sum(lapply(ds.input$MF, length)==0)==0){ 
#       #Use 200 to refer to fatal errors; everything else should be recoverable
#       out <- switch(ds.method, 
#                     "lm"=callMulti.lm(ds.input$MH, ds.input$OH, ds.input$MF, args, ds.var, att.table),
#                     "lmstep" = callStep.lm(ds.input$MH, ds.input$OH, ds.input$MF, args, ds.var, att.table),
#                     "PARM" = parm.main(ds.input$MH, ds.input$OH, ds.input$MF, args, ds.var, att.table),
#                     ReturnDownscaleError(ds.method))
#       #out.tmp <- apply(mapply('add.back.in', out, esd.index), 2, as.list) #TODO: this needs a BIG check to make sure
#       out.tmp <- list()
#       for(o in 1:length(out)){
#         out.name <- names(out)[o]
#         out.tmp[[out.name]] <- out[[o]]
#       }
#       #names(out.tmp) <- names(out)                           #that the order of rips doesn't invalidate this
#     }else{
#       #The qcmask return code goes in here, eventually
#       message(paste('Warning in CallDSMethod: insufficent data to downscale at this point and time window', 
#                     'when using a present val threshold for esd.gen of', 1)) #This code is now used ONLY for ds methods
#       if(length(esd.index)==1){
#         out.tmp <- list(rep(NA, length(esd.index[[1]]))) #It should be possible to generalize this, but it isn't working
#       }else{
#         out.tmp <- apply(mapply('rep', NA, sapply(esd.index, length), simplify=TRUE), 2, as.list)
#       }
#       names(out.tmp) <- paste0(ds.var, ".", rips)
#     }
#     if(train_qc_mask){
#       out.tmp$qc.mask <- ds.input$qcmask
#       names(out.tmp$qc.mask) <- paste0(ds.var, ".", rips)
#     }
#     return(out.tmp)  
#   }else{
#     #For all other cases:
#     out.tmp <- list()
#     if(!train.ds.split){
#       for(rip in rips){
#         esd.gen.rip.index <- lapply(att.table$fut.pred, '[[', 'rip')==rip
#         esd.gen.rip <- esd.gen[esd.gen.rip.index]
#         targ.var <- att.table$hist.targ[[1]]$var
#         out.var <- paste0(targ.var, ".", rip)
#         rip.att.table <- list('hist.pred' = att.table$hist.pred, 
#                               'hist.targ' = att.table$hist.targ, 
#                               'fut.pred' = att.table$fut.pred[esd.gen.rip.index])
#         ds.input <- assess.missvals(train.target, train.predict, esd.gen.rip, type=missval.handling, 
#                                     ds.method, args, min.present, att.table = rip.att.table, qcmask=train_qc_mask)
#         if((ds.input$status > 0 && ('MF'%in%train.names && 'MF'%in%ds.names))){
#           esd.index[[rip]] <- ds.input$MF[[1]]
#         }
#         if(ds.input$status < 200 && sum(lapply(ds.input$MF, length)==0)==0){ 
#           #Use 200 to refer to fatal errors; everything else should be recoverable
#           out <- switch(ds.method, 
#                         'CDFt' = callCDFt(ds.input$MH, ds.input$OH, ds.input$MF, args, ds.method="gfdl_CDFt", ds.var, rip.att.table),
#                         'simple.bias.correct' = callSimple.bias.correct(ds.input$MH, ds.input$OH, ds.input$MF, args),
#                         "BCQM" = callBCQMv2(ds.input$OH, ds.input$MH, ds.input$MF, args, ds.var, rip.att.table), 
#                         "EDQM" = callEDQMv2(ds.input$OH, ds.input$MH, ds.input$MF, args, ds.var, rip.att.table), 
#                         "CFQM" = callCFQMv3(ds.input$OH, ds.input$MH, ds.input$MF, args, ds.var, rip.att.table), 
#                         "DeltaSD" = callDeltaSD(ds.input$OH, ds.input$MH, ds.input$MF, args, ds.var, rip.att.table, ds.input$status), #esd.gen,
#                         #"lm"=callMulti.lm(ds.input$MH, ds.input$OH, ds.input$MF, args, ds.var, rip.att.table),
#                         #"lmstep" = callStep.lm(ds.input$MH, ds.input$OH, ds.input$MF, args, ds.var, rip.att.table),
#                         'CDFtmod' = callCDFt(ds.input$MH, ds.input$OH, ds.input$MF, args, ds.method="CDFtmod", ds.var, rip.att.table),
#                         #"PARM" = parm.main(ds.input$MH, ds.input$OH, ds.input$MF, args, ds.var, att.table),
#                         ReturnDownscaleError(ds.method))
#           out.tmp[[out.var]] <- add.back.in(out[[1]], esd.index[[rip]])
#           attributes(out.tmp[[out.var]]) <- attributes(out[[1]])
#           if(ds.method%in%c('CDFt', "CDFtmod", 'DeltaSD')){
#             for(i in 1:length(out)){
#               if(!is.null(attr(out[[i]], 'status'))){
#                 ds.input$qcmask[[out.var]] <- rep(NA, length(ds.input$qcmask[[1]])) #this is where this particular 
#               }
#             }
#             names(ds.input$qcmask) <- names(out)
#           }
#         }else{
#           #The qcmask return code goes in here, eventually
#           message(paste('Warning in CallDSMethod: insufficent data to downscale at this point and time window', 
#                         'when using a present val threshold for esd.gen of', 1)) #This code is now used ONLY for ds methods
#           out.tmp[[out.var]] <- rep(NA, length(esd.index[[1]])) #It should be possible to generalize this, but it isn't working
#         }
#         if(train_qc_mask){
#           out.tmp$qc.mask[[out.var]] <- ds.input$qcmask[[length(ds.input$qcmask)]]
#         }
#       }
#     }else{
#       #If it's posisble to split the training and downscaling into separate steps within the ds function
#       ds.input <- assess.missvals(train.target, train.predict, esd.gen, type=missval.handling, 
#                                   ds.method, args, min.present, att.table=att.table, qcmask=train_qc_mask)
#       if(((ds.input$status > 0 && 'MF'%in%train.names) || ds.input$status==130) && ds.input$status !=120){
#         #If the number of missing values has been altered by the fxn, get a new addition index
#         for(rip in rips){
#           esd.index[[rip]] <- ds.input$MF[rip.list==rip][[1]]
#         }
#       }
#       if(ds.input$status < 200 && sum(lapply(ds.input$MF, length)==0)==0){ 
#         #Use 200 to refer to fatal errors; everything else should be recoverable
#         out <- switch(ds.method, 
#                       "lm"=callMulti.lm(ds.input$MH, ds.input$OH, ds.input$MF, args, ds.var, att.table),
#                       "lmstep" = callStep.lm(ds.input$MH, ds.input$OH, ds.input$MF, args, ds.var, att.table),
#                       "PARM" = parm.main(ds.input$MH, ds.input$OH, ds.input$MF, args, ds.var, att.table),
#                       ReturnDownscaleError(ds.method))
#         #out.tmp <- apply(mapply('add.back.in', out, esd.index), 2, as.list) #TODO: this needs a BIG check to make sure 
#         #names(out.tmp) <- names(out)                           #that the order of rips doesn't invalidate this
#         for(o in 1:length(out)){
#           out.name <- names(out)[o]
#           out.tmp[[out.name]] <- out[[o]]
#           attributes(out.tmp[[out.name]]) <- attributes(out[[o]])
#         }
#       }else{
#         #The qcmask return code goes in here, eventually
#         message(paste('Warning in CallDSMethod: insufficent data to downscale at this point and time window', 
#                       'when using a present val threshold for esd.gen of', 1)) #This code is now used ONLY for ds methods
#         if(length(esd.index)==1){
#           out.tmp <- list(rep(NA, length(esd.index[[1]]))) #It should be possible to generalize this, but it isn't working
#         }else{
#           out.tmp <- apply(mapply('rep', NA, sapply(esd.index, length), simplify=TRUE), 2, as.list)
#         }
#         names(out.tmp) <- paste0(ds.var, ".", rips)
#       }
#       if(train_qc_mask){
#         out.tmp$qc.mask <- ds.input$qcmask
#         names(out.tmp$qc.mask) <- paste0(ds.var, ".", rips)
#       }
#       return(out.tmp)  
#     }
#     print('attributes of output')
#     print(lapply(out.tmp, 'attributes'))
#     return(out.tmp)
#   }
# }

ReturnDownscaleError <- function(ds.method){
  #Returns an error and stops the entire function if the DS method used is not supported.
  stop(paste("Downscale Method Error: the method", ds.method, "is not supported for FUDGE at this time."))
}

callSimple.bias.correct <- function(pred, targ, new, args){
  #'Performs a simple bias correction adjustment to a 
  #'single variable series, applying the mean difference 
  #'between the predictor and target datasets in the 
  #'historical period to the esd.gen dataset
  #'to generate downscaled data. 
  #'It takes no arguments. 
  out.var.name <- names(new)
  
  bias <- mean(unlist(pred)-targ)
  new.targ <- unlist(new)-bias
  out.list <- list()
  out.list[[out.var.name]] <- new.targ
  return(out.list)
}

callDeltaSD <- function(LH,CH,CF,args,ds.var='null', att_table=NULL, status=status){
  #'@author carlos.gaitan@noaa.gov
    #'@description The script uses the Delta Method to downscale coarse res. climate variables  
    #over a single variable
    #'@param LH: Local Historical (a.k.a. observations)
    #'@param CH: Coarse Historical (a.k.a. GCM historical)
    #'@param CF: Coarse Future (a.k.a GCM future)
    #'@param args: A list containing three arguments: 
    #'  @param deltatype, one of 'mean' or 'median', which will be used to determine 
    #'  what single value will be used for the difference between the CH and CF
    #'  @param deltaop, one of 'ratio' or 'add', which will be used to determine 
    #'  the mthod for calculating the delta
    #'  @param ignore0.Delta, a value of 'on' or 'off' that controls whether to use all days for
    #'  calculating the delta ('off'), or use only those days for which the delta was greater
    #'  than zero ('on')
    #'Uses the difference (ratio or subtraction) between CF and CH means or medians to calculate
    #' a delta that is applied to the LH (observational) data
    #'@return SDF: Downscaled Future (Local)
    ########################################
    #Note: preferred behavior is to truncate vectors
    #rather than randomly sampling iff too short.
    
    out.var.name <- names(CF)
    
    #Unlist data from its input form
    CH <- unlist(CH)
    # Obtain options
    if(!is.null(args$deltatype)){
      deltatype <- args$deltatype
    }else{
      stop(paste("DeltaSD Downscaling Error: deltatype not found in args"))
    }
    if(!is.null(args$deltaop)){
      deltaop <- args$deltaop
    }else{
      stop(paste("DeltaSD Downscaling Error: deltaop not found in args"))
    }
    if(!is.null(args$ignore0.Delta)){
      ignore0.Delta <- ifelse(args$ignore0.Delta=='on', TRUE, FALSE)
    }else{
      stop(paste("DeltaSD Downscaling Error: ignore0.Delta not found in args"))
    }
    #Prepare to loop by RIP
    out.list <- list()
    rip.list <- lapply(att_table$fut.pred, '[[', 'rip')
    for(rip in rip.list){
      out.status <- 0
      #Select the future predictor vars that you are interested in
      out.var.name <- paste0(ds.var, ".", rip)
      #Unlist the ensemble members
      rip.index <- (rip.list==rip)
      CF.data <- CF[rip.index]
      CF.data <- unlist(CF.data)
      #Decide how many iterations of the delta method to perform
      #based on the relative lengths of the historical
      #and future data (future should be >= historical) #Switch 1-28 from <  
      if(length(LH) >= length(CF.data)){
        #If the future and historical periods are unequal, truncate the vectors
        #That...raises an interesting question: should the CH vector be truncated as well?
        #Technically, it doesn't need to be for the code to work...
        if(ignore0.Delta){
          SDF <- LH[1:length(CF.data)]
          out.temp <- delta.downscale(LH[1:length(CF.data)], CH, CF.data, deltatype, deltaop, ignore0.Delta, status=status)
          if(!is.null(attr(out.temp, 'status'))){
            out.status <- out.status + 1
          }
          #SDF[SDF!=0] <- out.temp
          SDF[1:length(CF.data)] <- 0
          comp.indices <- which(LH[1:length(CF.data)]!=0)
          SDF[comp.indices] <- out.temp
        }else{
          SDF <- delta.downscale(LH[1:length(CF.data)], CH, CF.data, deltatype, deltaop,status=status)
        }
      }else{
        #Otherwise, if the vectors are uneven then calculate n+1 deltas, 
        #where n=length(CF.data)/length(LH)
        write.len <- 1
        out.len <- length(CF.data)
        in.len <- length(LH)
        SDF <- rep(NA, out.len)
        #vector comparisons take a long time relative to other things
        if(ignore0.Delta){
          comp.indices <- which(LH!=0)
        }
        while(write.len <= out.len){
          #print('looping on write.len')
          #delta.downscale removes NAs in the output vector
          if((write.len + in.len) <= length(CF.data)){
            tempvec <- delta.downscale(LH, CH, CF.data[write.len:(write.len + in.len)], 
                                       deltatype, deltaop, ignore0.Delta,status=status)
            if(!is.null(attr(tempvec, 'status'))){
              out.status <- out.status + 1
            }
          }else{
            len.diff <- length(CF.data) - write.len + 1
            #This needs an elese case for the case where there is *more* LH data
            #than CF data at a given point.
            in.len <- len.diff
            tempvec <- delta.downscale(LH[1:len.diff], CH[1:len.diff], 
                                       CF.data[seq(from=write.len, to=length(CF.data))], 
                                       deltatype, deltaop, ignore0.Delta,status=status)
            if(!is.null(attr(tempvec, 'status'))){
              out.status <- out.status + 1
            }
          }
          if(ignore0.Delta){
            SDF[seq(from=write.len, to=(write.len + in.len - 1))] <- 0 #Set all vals to 0 first
            sd.write.indices <- (write.len-1) + comp.indices
            #Remove any that might lead to a longer write index
            sd.write.indices <- sd.write.indices[sd.write.indices <= out.len] 
            SDF[sd.write.indices] <- tempvec
          }else{
            if(length(tempvec) > 0){
              SDF[seq(from=write.len, to=(write.len +length(tempvec)-1))] <- tempvec # length(tempvec)
            }else{
              print(summary(LH))
              print(summary(CH))
              print(summary(CF.data[write.len:(write.len + in.len)]))
              stop(paste("Error in DeltaSD: you should not have reached this point!"))
            }
          }
          write.len <- write.len + in.len
        }
      }
      #Adjusted 11-4 to account for presence of missing value vectors
      #being used to determine how many repetitions are needed for a delta
      SDF <- SDF[!is.na(SDF)]
      num.na <- (sum(is.na(SDF)))
      #       if(num.na > 0){
      #         print(length(CF.data))
      #         print(length(LH))
      #         print(summary(LH))
      #         print(summary(SDF))
      #         print(num.na)
      #         stop("Error in DeltaSD: NAs not in out being introduced from somewhere")
      #       }
      out.list[[out.var.name]] <- SDF
      if(out.status > 0){
        attr(out.list[[out.var.name]], 'status') <- out.status
      }
    }
    return(out.list)
}

delta.downscale <- function(delta.targ, delta.hist, delta.fut, deltatype, deltaop, 
                            ignore0.Delta=FALSE, status=0){
  #Calculates a delta after removing NAs and applies it to a target vector.
  #Helper method for callDeltaSD, but might be used elsewhere.
  if(status > 0){
    if(status==120){ #Check remove.delta.missvals for this value
      message('returning OH data without delta')
      return(delta.targ[1:length(delta.fut)])
    }
  }  
  #Make sure that there are no NA values in the current vector
  delta.fut.orig.len <- length(delta.fut)
  delta.fut <- delta.fut[!is.na(delta.fut)]
  delta.targ <- delta.targ[!is.na(delta.targ)]
  delta.hist.temp <- delta.hist[!is.na(delta.hist)]
  if(ignore0.Delta){
    #     delta.fut.orig.len <- length(delta.fut)
    #     delta.fut <- delta.fut[!is.na(delta.fut) & delta.fut!=0]
    #     delta.targ <- delta.targ[!is.na(delta.targ) & delta.targ!=0]
    #     delta.hist.temp <- delta.hist[!is.na(delta.hist) & delta.hist!=0]
    #delta.fut.orig.len <- length(delta.fut)
    delta.fut <- delta.fut[delta.fut!=0]
    delta.targ <- delta.targ[delta.targ!=0]
    delta.hist.temp <- delta.hist.temp[delta.hist.temp!=0]
  }
  #   }else{
  #     delta.fut.orig.len <- length(delta.fut)
  #     delta.fut <- delta.fut[!is.na(delta.fut)]
  #     delta.targ <- delta.targ[!is.na(delta.targ)]
  #     delta.hist.temp <- delta.hist[!is.na(delta.hist)]
  #   }
  if(length(delta.hist)==0){
    if(deltaop=='ratio'){
      #Return the OH data
      message(paste("Insufficent historical predictor (MH) data when used with the ratio deltatype;", 
                    "returning target values without the delta"))
      out <- delta.targ
      attr(out, 'status') <- 1
      return(out)
      #When the options are enabled properly, flag this and return a qc mask
    }else{
      #message("Adding ignore0.Delta correction for MH data: zero returned as MH value for delta calculation")
      delta.hist <- 0
    }
  }else{
    delta.hist <- delta.hist.temp
  }
  if(length(delta.fut)==0){
    #No need to check for all missing values on this; the effect is the same in either case
    #message("Adding ignore0.Delta correction for the MF data: zero returned as MF for delta calculation")
    delta.fut <- 0
  }
  #The na.rms are important in this step - it's possible, if there are more LH than CH samples in the case
  #where the future time period is longer than the historical time period, for missing values to get passed
  #into the CH dataset. 
  if(deltaop=='add'){
    #Downscale by difference delta
    delta<-do.call(deltatype, list(delta.fut, na.rm=TRUE))-do.call(deltatype, list(delta.hist, na.rm=TRUE))
    out <- delta.targ + delta #CHANGED FROM delta.fut
  }else if(deltaop=='ratio'){
    #Downscale by percentage delta (never negative, but occasionally NaN)
    delta<-do.call(deltatype, list(delta.fut,na.rm=TRUE))/do.call(deltatype, list(delta.hist,na.rm=TRUE))
    #Loud warning message for divide-by-0 case
    if(is.nan(delta)||is.infinite(delta)|| is.na(delta)){
      message(paste("Warning in delta.downscale: Calculated delta with deltaop", deltaop, 
                    "is either NaN or Inf and cannot be processed normally.",
                    "Returning target data values without delta adjustment applied."))
      #Add a value flagging operation at this point
      out <- delta.targ
      attr(out, 'status') <- 1
      #       print(summary(delta.fut))
      #       print(summary(delta.hist))
      #       print(median(delta.fut))
      #       print(median(delta.hist))
      #       stop('examine the output that led to this')
      return(out)
    }
    out <- delta.targ*delta 
  }else{
    stop(paste("delta.downscale Downscaling Error: deltaop", deltaop, "is not one of 'ratio' or 'add'"))
  }
  return(out)
}

callNoDSMethod <- function(pred, targ, new, args, ds.var, att.table){
  out.list <- list()
  rip.list <- lapply(att_table$fut.pred, '[[', 'rip')
  rips <- unique(rip.list)
  for (rip in rips){
    out.var <- paste0(ds.var, rip)
    rip.index <- (rip.list==rips)
    out.list[[out.var]] <- new[[rip.index]][[1]]
  }
  return(out.list)
}
