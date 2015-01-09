#LoopByTimeWindow.R
#Written by Carolyn Whitlock, August 2014
#'Calls temporal masking functions, given a set of time data
#'and a mask for each function.
#'Then, calls CrossValidate for each masked window
#'and merges the downscaled results it gets back
#'into a single time series
#'
#'####Arguments related to downscaling data:
#'@param train.predictor
#'@param train.target
#'@param esd.gen: a single file or a list of files pointing to the data to which masks should be applied. Currently, 
#'only a single esd.gen vector is supported.
#'
#'####Arguments related to the CrossValidate call:
#'@param kfold: the k-fold cross-validation. Note that *ONLY* if k=0 will downscaling
#'equations be called on the esd.gen dataset(s).
#'@param downscale.fxn: A string referring to the downscaling function to be called.
#'@param downscale.args: A list of the arguments to the downscaling function. 
#'
#'####Arguments related to the masking function:
#'@param masklist: a list of pathnames pointing to the files which contain masks to apply. If none are provided, 
#'defaults to running all datasets without temporal masks of any sort.
#'Currently, the code assumes that all maskfiles will have the same number of masks within them
#'to apply; this assumption needs to be talked about at some point.
#'
#'@return A single timeseries containing all downscaled data.
#'####
#' @examples
#' sample_t_predict <- seq(1:365)
#' sample_t_target <- sin(sample_t_predict*0.05)
#' sample_esd_gen <- seq(1:365)
#' mask_list <- list("/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_pm2weeks_clim_noleap.nc", 
#'                   "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_pm2weeks_clim_noleap.nc", 
#'                   "/net3/kd/PROJECTS/DOWNSCALING/DATA/WORK_IN_PROGRESS/GFDL-HIRAM-C360/masks/time_masks/maskdays_bymonth_clim_noleap.nc")
#'                   d_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
#' esd.gen = sample_esd_gen, kfold = 0, downscale.fxn = "ESD.Train.totally.fake", 
#' downscale.args=NULL,
#' masklist = mask_list)
#' lines(seq(1:365), d_data, col="cyan")
#' d2_data <- DownscaleByTimeWindow(train.predictor = sample_t_predict, train.target = sample_t_target, 
#'                                  esd.gen = sample_esd_gen, kfold = 2, downscale.fxn = "ESD.Train.totally.fake", 
#'                                  downscale.args=NULL,
#'                                  masklist = alt_mask_list)
#' lines(seq(1:365), d2_data, col="magenta")
#' @references \url{link to the FUDGE API documentation}
#' TODO: Check on assumption that all maskfiles will have the same number of masks
#' RESOLVED: They may not in later versions, but it's a valid 1^5 assumption.
#' TODO: Do train.predictor and train.target need to be able to accept lists?
#' RESOLVED: Yes they do, but not for 1^5.
#' TODO: Check on how to determine which masks go with which data. Actually, check with MJ
#' on how the time pruning might work.
#' TODO: Figure out how the sourcing/wrappers are going to work, as well as the args input.
#' TODO: Also, is this the place to start doing simple checks to avoid calling the fxn if all points
#' are NA? Or do we do that twice: once for lat/lon, and then again for the time masking?
#' MORE INFO NEEDED: At the moment, the checks take place in the CrossValidate function. Calls elsewhere
#' migth make sense; it's something to discuss once the driver script is working.
#' TODO: will it always be a valid assumption to assume that the downscaling fxn returns on value
#' for every value passed into it?
#' TODO: Currently will not accept lists as an argument for esd.gen and its cousisns, but that's solvable
#' later. 

LoopByTimeWindow <- function(train.predictor=NULL, train.target=NULL, esd.gen, mask.struct, 
                             create.ds.out=TRUE, downscale.fxn=NULL, downscale.args = NULL, kfold=0, kfold.mask=NULL, 
                             graph=FALSE, masklines=FALSE, 
                             ds.orig=NULL, ds.var='tasmax',
                             #s5.adjust=FALSE, s5.method=s5.method, s5.args = s5.args, 
                             s5.instructions='na', s5.adjust=FALSE,
                             create.qc.mask=create.qc.mask, create.adjust.out=create.adjust.out)
{
  #May be advisable to hold fewer masks in memory. Can move some of the looping code to compensate.
  #At the present time, it might make more sense to call the more complicted fxns from elsewhere.
  #source("../../FudgePreDS/ApplyTemporalMask.R")
  #source("MaskMerge.R")
  #source("CrossValidate.R")
  if(mask.struct[[1]]!='na'){
    #If there are masks included:
    print("why are you here?")
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
  #   if(create.postproc){
  #     postproc.out <- rep(NA, downscale.length)
  #   }else{
  #     postproc.out <- NULL
  #   }
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
    #if (window%%10==0 || window==1){
    #  message(paste("starting on window", window, "of", num.masks))
    #}
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
        kfold.predict <- ApplyTemporalMask(window.predict, kfold.masks[[1]]$masks[[kmask]])
        kfold.target <- ApplyTemporalMask(window.target, kfold.masks[[2]]$masks[[kmask]])
        kfold.gen <- ApplyTemporalMask(window.gen, kfold.masks[[3]]$masks[[kmask]])
        if(!is.null(ds.orig)){
          kfold.orig <- ApplyTemporalMask(window.orig, kfold.masks[[3]]$masks[[kmask]])
        }else{
          kfold.orig <- NA
        }
      }else{
        #TODO: Ask someone about how looping over a sinle element slows the code (OR DOES IT?)
        kfold.predict <- window.predict
        kfold.target <- window.target
        kfold.gen <- window.gen
        kfold.orig <- window.orig
      }
      #If there is enough data available in the window to perform downscaling
      if (sum(!is.na(kfold.predict))!=0 && sum(!is.na(kfold.target))!=0 && sum(!is.na(kfold.gen))!=0){
        if(length(mask.struct) <= 3){
          #perform downscaling on the series and merge into new vector
          if(create.ds.out){
            #TODO CEW: Should this looping structure be more nested? The assignment to downscale.vec might not be nessecary
            #print(summary(kfold.predict))
            #print(summary(kfold.target))
            #print(summary(kfold.gen))
            temp.out <- CallDSMethod(ds.method = downscale.fxn,
                                     train.predict = kfold.predict[!is.na(kfold.predict)], 
                                     train.target = kfold.target[!is.na(kfold.target)], 
                                     esd.gen = kfold.gen[!is.na(kfold.gen)], 
                                     args=downscale.args, 
                                     ds.var=ds.var)
            downscale.vec[!is.na(kfold.gen)] <- temp.out
            #print(summary(as.vector(downscale.vec)))
          }
          if(s5.adjust){
            if(is.na(kfold.orig)){
              #If there is ds data being passed in from outside, it gets checked
              data <- temp.out
            }else{
              #otherwise, use the ds values from the run you have just completed
              data <- kfold.orig[!is.na(kfold.orig)]
            }
            #print("results before adjust section")
            #print(summary(data), digits=6)
            temp.out <- callS5Adjustment(s5.instructions=s5.instructions,
                                         #s5.method=s5.method,s5.args=s5.args,
                                         data = data, 
                                         hist.pred = kfold.predict[!is.na(kfold.predict)], 
                                         hist.targ = kfold.target[!is.na(kfold.target)], 
                                         fut.pred = kfold.gen[!is.na(kfold.gen)])
            #create.qc.mask=create.qc.mask, create.adjust.out=create.adjust.out)
            if(!is.null(temp.out$qc.mask)){
              qc.mask[!is.na(kfold.gen)] <- temp.out$qc.mask #A NULL assignment might cause problems here. Second if?
            }else{
              #Try not doing anything
            }
            downscale.vec[!is.na(kfold.gen)] <- temp.out$ds.out
            #print("results after adjust section")
            #print(summary(temp.out$ds.out), digits=6)
          }
          
          #           if(create.postproc){
          #             postproc.out[!is.na(kfold.gen)] <- CallPostProcMethod(data=kfold.gen[!is.na(kfold.gen)], 
          #                                                                   mask=kfold.target[!is.na(kfold.target)], 
          #                                                                   mask.data=kfold.predict[!is.na(kfold.predict)],
          #                                                                   pp.method=postproc.method, args=postproc.args)
          #             print(summary(postproc.out))
          #             print(length(postproc.out))
          #             print(length(!is.na(postproc.out)))
          #           }
        }else{
          #If there is a 4th pruning mask (currently only supported by 1 scenario), apply that afterwards
          #TODO: Test this bit, the masks are going to be significantly more complicated
          time.trim.mask <- mask.struct[[4]]$masks[[window]]
          temp.out <- window.gen
          temp.out[!is.na(temp.out)] <- CallDSMethod(ds.method = downscale.fxn,
                                                     train.predict = window.predict[!is.na(window.predict)], 
                                                     train.target = window.target[!is.na(window.target)], 
                                                     esd.gen = window.gen[!is.na(window.gen)], 
                                                     args=downscale.args)
          temp.out2<-ApplyTemporalMask(temp.out, time.trim.mask)
          if(create.qc.mask){
            qc.mask[!is.na(time.trim.mask)] <- QCDSValues(data = temp.out.2, qc.test=qc.test,
                                                          hist.pred = window.predict[!is.na(window.predict)], 
                                                          hist.targ = window.target[!is.na(window.target)], 
                                                          fut.pred = window.gen[!is.na(window.gen)])
          }
          downscale.vec[!is.na(time.trim.mask)]<-temp.out2[!is.na(temp.out2)]
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
  #However, if cross-validation is being performed
  #   } else { 
  #     stop(paste("Cross validation not supported in FUDGE at this time; please run with k < 2"))
  #     #Remember to duplicate most of the structure from above; you're just adding a few new checks
  #   }
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
