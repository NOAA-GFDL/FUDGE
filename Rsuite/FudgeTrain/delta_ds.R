#delta_ds.R
#Main and helper functions for downscaling
#with DeltaSD

callDeltaSD <- function(LH,CH,CF,args, train_qcmask, status=0){
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
    #'  than zero ('on'). At this point it's a legacy setting; the main ignore0 setting in 
    #'  the precipiation options sets 0 to missing 
    #'Uses the difference (ratio or subtraction) between CF and CH means or medians to calculate
    #' a delta that is applied to the LH (observational) data
    #'@return SDF: Downscaled Future (Local)
    ########################################
    #Note: preferred behavior is to truncate vectors
    #rather than randomly sampling iff too short.
    
    #Use this rather than unlist - unlist removes some dimension data
    LH = LH[[1]]
    CH = CH[[1]]
    CF = CF[[1]]
    out.data <- array(NA, dim=dim(CF))
    #Remove missing values 
    LH <- LH[!is.na(LH)]
    CH <- CH[!is.na(CH)]
    nrips = dim(out.data)[4]
    #Are we returning a mask of those points where delta beahved as expected?
    if (train_qcmask){
      qc.data <- array(1, dim=dim(CF))
    }else{
      qc.data <- NULL
    }
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
    for(n in 1:nrips){
      out.status <- 0
      CF.data <- CF[,,,n,,]
      CF.idx <- !is.na(CF.data)
      CF.data <- CF.data[CF.idx]
      #Decide how many iterations of the delta method to perform
      #based on the relative lengths of the historical
      #and future data (future should be >= historical) #Switch 1-28 from <  
      if(length(LH) >= length(CF.data)){
        #If the future and historical periods are unequal, truncate the vectors
        #That...raises an interesting question: should the CH vector be truncated as well?
        #Technically, it doesn't need to be for the code to work...
             save(file="/home/cew/Code/h_testing//ds_debug/save/new_delta_train.R", 
                  list=c('LH', 'CF.data', 'CH', 'deltatype', 'deltaop', 'ignore0.Delta', 'status'))
        if(ignore0.Delta){
          SDF <- LH[1:length(CF.data)]
          out.temp <- delta.downscale(LH[1:length(CF.data)], CH, CF.data, deltatype, deltaop, ignore0.Delta, status=status)
          if(!is.null(attr(out.temp, 'status'))){
            out.status <- out.status + 1
          }
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
      #Adjusted again 10-14-16 since there is now an assumption that your 
      #ds methods keep track of missing values in the predictor dataset
      ########################
      #SDF <- SDF[!is.na(SDF)]
      #num.na <- (sum(is.na(SDF)))
      #######################
      print(length(out.data[,,,n,,]))
      print(length(SDF))
      print(length(CF.data))
      out <- rep(NA, length(CF.idx))
      out[CF.idx] <- SDF
      out.data[,,,n,,] <- out
      if(out.status > 0){
        #As of 10-14-16, replacing status setting with qcmask
        if (train_qcmask){
          #the below used to store the status return; for now, to keep 
          #consistency with the other, more commonly-used training mask
          #methods, we're replacing that with NA
          qc.data[,,,n,,] <- NA 
        }
      }
    }
    save(file="/home/cew/Code/h_testing//ds_debug/save/new_delta_ds.R", 
         list=c('SDF', 'CF.idx', 'out.data', 'out'))
    stop('examine output')
    return(list(ds=out.data, qc=qc.data))
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
    delta.fut <- delta.fut[delta.fut!=0]
    delta.targ <- delta.targ[delta.targ!=0]
    delta.hist.temp <- delta.hist.temp[delta.hist.temp!=0]
  }
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
      return(out)
    }
    out <- delta.targ*delta 
  }else{
    stop(paste("delta.downscale Downscaling Error: deltaop", deltaop, "is not one of 'ratio' or 'add'"))
  }
  return(out)
}