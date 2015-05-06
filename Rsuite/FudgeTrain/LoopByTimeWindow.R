#LoopByTimeWindow.R
#Written by Carolyn Whitlock, August 2014
#'Calls temporal masking functions, given a set of time data
#'and a mask for each function.
#'Then, cross-validates the masked data (if applicable) 
#'and merges the downscaled results it gets back
#'into a single time series
#'
#'####Arguments related to downscaling data:
#'@param train.predictor: A 1-D array of data that will be used as the historic predictor dataset for downscaling
#'@param train.target: A 1-D array of data that will be used as the target dataset for downscaling. Should be of 
#'the same dimensions as the future predictor dataset. 
#'@param esd.gen: A 1-D array of data that will be used as the future predictor dataset for downscaling. 
#'
#'####Arguments related to the downscaling process:
#'@param kfold: the k-fold cross-validation. Note that *ONLY* if k=0 will downscaling
#'equations be called on the esd.gen dataset(s). Currently, only k=0 is supported in the current version of FUDGE.
#'@param downscale.fxn: A string referring to the downscaling function to be called.
#'@param downscale.args: A list of the arguments to the downscaling function. 
#'
#'####Arguments related to the masking function:
#'@param mask.struct: a list of masks returned by CreateTimeWindowList, used to mask out days for downscaling by time
#'window. Will have either 3 or 4 masks depending upon whether or not the masks used in the future predictor series
#'overlap each other.
#'
#'###Arguments related to section 3 and section 5 adjustment
#'@param s3.adjust, s3.instructions: Whether or not to adjust the data before downscaling, and the instructions for 
#'adjustment before downscaling. Defaults to FALSE, and 'na' if no instructions are included. See the section 3 
#'documentation for more details.
#'@param s5.adjust, s5.instructions: Whether or not to adjust the data/create a QC mask after downscaling, and the 
#'instructions for the post-downscaling adjustment. Defaults to FALSE, and 'na' if no instructions are included. See
#'the section 5 documentation for more details.
#'
#'@param create.qc.mask: Whether a second dataset containing a qc mask is returned from the function. Defaults
#'to FALSE.
#'
#'@return A single dataset containing as many time levels as the esd.gen dataset containing all downscaled data.
#'####
#' @references \url{link to the FUDGE API documentation}

LoopByTimeWindow <- function(train.predictor=NULL, train.target=NULL, esd.gen, mask.struct, 
                             create.ds.out=TRUE, downscale.fxn=NULL, downscale.args = NULL, kfold=0, kfold.mask=NULL, 
                             graph=FALSE, masklines=FALSE, 
                             ds.orig=NULL, ds.var='tasmax',
                             #s5.adjust=FALSE, s5.method=s5.method, s5.args = s5.args,
                             s3.instructions='na', s3.adjust=FALSE,
                             s5.instructions='na', s5.adjust=FALSE,
                             create.qc.mask=FALSE, create.adjust.out=FALSE)
{
  #May be advisable to hold fewer masks in memory. Can move some of the looping code to compensate.
  #At the present time, it might make more sense to call the more complicted fxns from elsewhere.
  
  if(mask.struct[[1]]!='na'){
    #If there are masks included:
    mask.data.by.time.window <- TRUE
    num.masks <- length(names(mask.struct[[3]]$masks))
  }else{
    #If there are no masks (so whole time series is used)
    mask.data.by.time.window <- FALSE
    num.masks <- 1
  }
  downscale.length <- length(esd.gen)
  if(create.ds.out){
    downscale.vec <- rep(NA, downscale.length)
  }else{
    downscale.vec <- NULL
  }
  if(create.qc.mask){
    qc.mask <- rep(NA, downscale.length)
  }else{
    qc.mask <- NULL
  }
  ##Create checkvector to test collision of kfold validation masks
  checkvector <- rep(0, downscale.length)
  #And finally, in order to see internal activity, add the graph options
  if(graph){
    #     mask.cols = colorRampPalette(c("red", "gray90", "blue"))(num.masks) #Try ivory next time you run it
    #     fit.cols = colorRampPalette(c("red", "gray90", "blue"))(num.masks*kfold)
    mask.cols = rainbow(num.masks)
    fit.cols = rainbow(num.masks*kfold)
    plot(seq(1:length(train.target)), train.target, type = "l", lwd = 3, main=paste("Mask and lines of best fit for time windowing"))
  }
  
  for (window in 1:num.masks){
    if(mask.data.by.time.window){
      window.predict <- ApplyTemporalMask(train.predictor, mask.struct[[1]]$masks[[window]])
      window.target <- ApplyTemporalMask(train.target, mask.struct[[2]]$masks[[window]])
      window.gen <- ApplyTemporalMask(esd.gen, mask.struct[[3]]$masks[[window]])
    }else{
      window.predict <- train.predictor
      window.target <- train.target
      window.gen <- esd.gen
    }
    if(!is.null(ds.orig)){
      window.orig <- ApplyTemporalMask(ds.orig, mask.struct[[3]]$masks[[window]])
    }else{
      window.orig <- NA
    }
    
    #If no cross-validation is being performed:
    for(kmask in 1:length(kfold.mask)){
      if (kfold > 1){
        message("Warning: Kfold cross-validation not supported at this time")
        kfold.predict <- ApplyTemporalMask(window.predict, kfold.masks[[1]]$masks[[kmask]])
        kfold.target <- ApplyTemporalMask(window.target, kfold.masks[[2]]$masks[[kmask]])
        kfold.gen <- ApplyTemporalMask(window.gen, kfold.masks[[3]]$masks[[kmask]])
        if(!is.null(ds.orig)){
          kfold.orig <- ApplyTemporalMask(window.orig, kfold.masks[[3]]$masks[[kmask]])
        }else{
          kfold.orig <- NA
        }
        #Apply kfold mask to time-trimming mask, if applicable
        if(length(mask.struct) > 3){
          use.time.trim.mask <- TRUE
          kfold.timemask <- ApplyTemporalMask(mask.struct[[4]]$masks[[window]], kfold.masks[[3]]$masks[[kmask]])
        }else{
          use.time.trim.mask <- FALSE
        }
      }else{
        #TODO: Ask someone about how looping over a sinle element slows the code (OR DOES IT?)
        kfold.predict <- window.predict
        kfold.target <- window.target
        kfold.gen <- window.gen
        kfold.orig <- window.orig
        if(length(mask.struct) > 3){
          use.time.trim.mask <- TRUE
          kfold.timemask <- mask.struct[[4]]$masks[[window]]
        }else{
          use.time.trim.mask <- FALSE
        }
        
      }
      #If there is enough data available in the window to perform downscaling
      if (sum(!is.na(kfold.predict))!=0 && sum(!is.na(kfold.target))!=0 && sum(!is.na(kfold.gen))!=0){
        #If no time-trimming mask is used
        #        if(length(mask.struct) <= 3){
        #Adjust the values of the downscaled ouptut, if applicable
        if(s3.adjust){
          temp.out <- callS3Adjustment(s3.instructions=s3.list, 
                                       hist.pred = kfold.predict, 
                                       hist.targ = kfold.target, 
                                       fut.pred = kfold.gen,  
                                       s5.instructions=s5.list)
          s5.instructions <- temp.out$s5.list
          kfold.target <- temp.out$input$hist.targ
          kfold.predict <- temp.out$input$hist.pred
          kfold.gen <- temp.out$input$fut.pred
          remove(temp.output)
        }
        #perform downscaling on the series and merge into new vector
        if(create.ds.out){
          #TODO CEW: Should this looping structure be more nested? The assignment to downscale.vec might not be nessecary
          temp.out <- CallDSMethod(ds.method = downscale.fxn,
                                   train.predict = kfold.predict[!is.na(kfold.predict)], 
                                   train.target = kfold.target[!is.na(kfold.target)], 
                                   esd.gen = kfold.gen[!is.na(kfold.gen)], 
                                   args=downscale.args, 
                                   ds.var=ds.var)
          #Assign downscaled output to vector
          if(use.time.trim.mask){
#             downscale.vec[!is.na(kfold.timemask)] <- temp.out[!is.na(kfold.timemask)]
            temp.assign <- esd.gen[[1]]
            temp.assign[!is.na(mask.struct[[3]]$masks[[window]])] <- temp.out
            downscale.vec[!is.na(kfold.timemask)] <- temp.assign[!is.na(kfold.timemask)]
          }else{
            downscale.vec[!is.na(kfold.gen)] <- temp.out
          }
        }
        #And adjust the downscaled output, if applicable
        if(s5.adjust){
          if(is.na(kfold.orig)){
            #If there is ds data being passed in from outside, it gets checked
            data <- temp.out
          }else{
            #otherwise, use the ds values from the run you have just completed
            #data <- kfold.orig[!is.na(kfold.orig)]
            data <- temp.out[!is.na(temp.out)]
          }
          temp.out <- callS5Adjustment(s5.instructions=s5.instructions,
                                       #s5.method=s5.method,s5.args=s5.args,
                                       data = data, 
                                       hist.pred = kfold.predict[!is.na(kfold.predict)], 
                                       hist.targ = kfold.target[!is.na(kfold.target)], 
                                       fut.pred = kfold.gen[!is.na(kfold.gen)])
          #create.qc.mask=create.qc.mask, create.adjust.out=create.adjust.out)
          if(!is.null(temp.out$qc.mask)){
            if(use.time.trim.mask){
              temp.assign <- esd.gen[[1]]
              temp.assign[!is.na(mask.struct[[3]]$masks[[window]])] <- temp.out
              downscale.vec[!is.na(kfold.timemask)] <- temp.assign[!is.na(kfold.timemask)]
            }else{
              qc.mask[!is.na(kfold.gen)] <- temp.out$qc.mask #A NULL assignment might cause problems here. Second if?
            }
          }else{
            #Try not doing anything
          }
          #If there is a time-trimming mask, use it here
          #Assign downscaled output to vector
          if(use.time.trim.mask){
            temp.assign <- esd.gen[[1]]
            temp.assign[!is.na(mask.struct[[3]]$masks[[window]])] <- temp.out
            downscale.vec[!is.na(kfold.timemask)] <- temp.assign[!is.na(kfold.timemask)]
          }else{
            downscale.vec[!is.na(kfold.gen)] <- temp.out$ds.out
          }
        }

        if(graph){
          if(masklines){
            abline(v=which(!is.na(window.gen))[1])      #Option for plotting start of masks as | lines
          }
          points(seq(1:length(window.gen))[!is.na(window.gen)], downscale.vec[!is.na(window.gen)], 
                 pch = (window-1), lwd = 1, col=mask.cols[window]) #ty = window, lwd = 4,
        }
        #Otherwise, you don't need to do anything because that loop should be full of NAs
      }else{
        print(paste("Too many NAs in loop", (window*length(kfold.mask))+kmask, "of", num.masks*length(kfold.mask), "; passing loop without downscaling"))
      }
    }
  }
  #Exit loop
  return(list('downscaled'=downscale.vec, 'qc.mask'=qc.mask)) #'postproc.out'=postproc.out))
}

#Converts NAs to 0, and all non-NA values to 1
#and returns the result in a 1-D form
convert.NAs<-function(dataset){
  dataset2<-dataset
  dataset2[is.na(dataset)]<-0
  dataset2[!is.na(dataset)]<-1
  return(as.vector(dataset2))
}
