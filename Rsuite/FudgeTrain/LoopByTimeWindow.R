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
#' TODO: will it always be a valid assumption to assume that the downscaling fxn returns one value
#' for every value passed into it?
#' RESOLVED: For this stage, yes - you want oen value for every value of

LoopByTimeWindow <- function(train.predictor=NULL, train.target=NULL, esd.gen, mask.struct,
                             create.ds.out=TRUE, downscale.fxn=NULL, downscale.args = NULL, kfold=0, kfold.mask=NULL,
                             att.table=NA,
                             graph=FALSE, masklines=FALSE,
                             ds.orig=NULL, ds.var='tasmax',
                             s3.instructions='na', s3.adjust=FALSE,
                             s5.instructions='na', s5.adjust=FALSE,
                             train_qcmask=FALSE,
                             create.qc.mask=FALSE,
                             min.present=1,
                             verbose=TRUE, test=TRUE)
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
    #     mask.struct <- list(list(masks=rep(1, length(train.predictor[[1]]))),
    #                         list(masks=rep(1, length(train.target))),
    #                         list(masks=rep(1, length(esd.gen[[1]]))))
    mask.struct <- list(list(masks=list(rep(1, length(train.predictor[[1]])))),
                        list(masks=list(rep(1, length(train.target)))),
                        list(masks=list(rep(1, length(esd.gen[[1]])))))
  }
  downscale.length <- length(esd.gen[[1]]) #Will always have at least one entry
  rip.list <- lapply(att.table$fut.pred, '[[', 'rip')
  rips <- unique(rip.list)
  downscale.varlist <- paste0(att.table$hist.targ[[1]]$var, ".", rips) #Should only ever have single value in hist.targ
  if(create.ds.out){
    downscale.vec  <- rep(NA, downscale.length)
    downscale.list <- list()
    for(name in downscale.varlist){
      downscale.list[[name]] <- downscale.vec
    }
  }else{
    downscale.list <- NULL
  }
  if(create.qc.mask){
    qc.mask <- rep(NA, downscale.length) #Check this bit - shouldn't cause errors, but might later
    qc.masklist <- list()
    for(name in downscale.varlist){
      qc.masklist[[name]] <- qc.mask
    }
  }else{
    qc.masklist <- NULL
  }
  #And finally, in order to see internal activity, add the graph options
  if(graph){
    #     mask.cols = colorRampPalette(c("red", "gray90", "blue"))(num.masks) #Try ivory next time you run it
    #     fit.cols = colorRampPalette(c("red", "gray90", "blue"))(num.masks*kfold)
    mask.cols = rainbow(num.masks)
    fit.cols = rainbow(num.masks*kfold)
    plot(seq(1:length(train.target)), train.target, type = "l", lwd = 3, main=paste("Mask and lines of best fit for time windowing"))
  }

  #for (window in 1:num.masks){
    for (window in 7:7){
    print(paste('on window', window, 'of', num.masks))
    if(mask.data.by.time.window){
      #This should not produce any errors - at present, the only dataset that shoudl have missvals that are *not*
      #due to a mask would be the observational dataset, and that dataset should not have to deal with the awkward pooling of the
      #       if('OH'%in%ds.names){
      #         #...is it enough just to mask the masks in this step?
      #         window.target <- trim.time.mask(data=train.target, mask=downscale.args$delta.mask)
      # #         window.predict <- lapply(train.predictor, 'trim.time.mask', mask=downscale.args$delta.mask)
      #         mask.struct[[2]]$masks[[window]] <- trim.time.mask(data = mask.struct[[2]]$masks[[window]],
      #                                                            mask=downscale.args$delta.mask)
      # #         mask.struct[[1]]$masks[[window]] <- trim.time.mask(data = mask.struct[[1]]$masks[[window]],
      # #                                                            mask=downscale.args$delta.mask)
      #         window.target  <- trim.time.mask(data=window.target,    mask=mask.struct[[2]]$masks[[window]])
      #       }else{
      #         window.target  <- trim.time.mask(data=train.target,    mask=mask.struct[[2]]$masks[[window]])
      #         #window.predict <- lapply(train.predictor, trim.time.mask, mask=mask.struct[[1]]$masks[[window]])
      #       }
      window.target  <- trim.time.mask(data=train.target,    mask=mask.struct[[2]]$masks[[window]])
      window.predict <- lapply(train.predictor, trim.time.mask, mask=mask.struct[[1]]$masks[[window]])
      window.gen     <- lapply(esd.gen        , trim.time.mask, mask=mask.struct[[3]]$masks[[window]])
    }else{
      window.predict <- train.predictor
      window.target  <- train.target
      window.gen     <- esd.gen
    }
    if(!is.null(ds.orig)){
      #TODO: edit this for compliance with the rest?
      window.orig <- ApplyTemporalMask(ds.orig, mask.struct[[3]]$masks[[window]])
    }else{
      window.orig <- NA
    }

    #If no cross-validation is being performed:
    for(kmask in 1:length(kfold.mask)){
      if (kfold > 1){
        #kfold.predict <- trim.time.mask(data=window.predict, mask=mask.struct[[1]]$masks[[kmask]])
        kfold.target  <- trim.time.mask(data=window.target,  mask=mask.struct[[2]]$masks[[kmask]])
        #kfold.gen     <- trim.time.mask(data=window.gen,     mask=mask.struct[[3]]$masks[[kmask]])
        kfold.predict <- lapply(window.target, 'trim.time.mask',  mask=mask.struct[[1]]$masks[[kmask]])
        kfold.gen     <- lapply(window.gen, 'trim.time.mask',  mask=mask.struct[[1]]$masks[[kmask]])
        if(!is.null(ds.orig)){
          kfold.orig <- ApplyTemporalMask(window.orig, kfold.masks[[3]]$masks[[kmask]])
        }else{
          kfold.orig <- NA
        }
        #Apply kfold mask to time-trimming mask, if applicable
        if(length(mask.struct) > 3){
          use.time.trim.mask <- TRUE
          ttrim.mask <- apply.any.mask(data=mask.struct[[4]]$masks[[window]], mask=kfold.masks[[3]]$masks[[kmask]], dim='temporal')
          ds.ttrim.mask <- trim.time.mask(data=mask.struct[[3]]$masks[[window]], mask=mask.struct[[4]]$masks[[window]])
        }else{
          use.time.trim.mask <- FALSE
        }
      }else{
        kfold.predict <- window.predict
        kfold.target <- window.target
        kfold.gen <- window.gen
        kfold.orig <- window.orig
        if(mask.data.by.time.window){
          if(length(mask.struct) > 3){
            use.time.trim.mask <- TRUE
            ttrim.mask <- mask.struct[[3]]$masks[[window]]
            ds.ttrim.mask <- mask.struct[[4]]$masks[[window]]
          }else{
            use.time.trim.mask <- FALSE
            ttrim.mask <- mask.struct[[3]]$masks[[window]]
          }
        }else{
          use.time.trim.mask <- FALSE
          ttrim.mask <- rep(1, length(kfold.gen[[1]]))
        }
      }
      #If there is enough data available in the window to perform downscaling
      #      if (sum(!is.na(kfold.predict))!=0 && sum(!is.na(kfold.target))!=0 && sum(!is.na(kfold.gen))!=0){
      #Adjust the values of the downscaled ouptut, if applicable
      if(s3.adjust){
        if(test){
          #Add the save in here
          save(file="/home/cew/Code/testing/transform_tests/rdata/save1.R", list=c('kfold.target'))
        }
        temp.out <- callS3Adjustment(s3.instructions=s3.instructions,
                                     hist.pred = kfold.predict,
                                     hist.targ = kfold.target,
                                     fut.pred = kfold.gen,
                                     att.list=att.table,
                                     s5.instructions=s5.instructions)
        s5.instructions.window <- temp.out$s5.list
        #           (print(summary(kfold.target - temp.out$input$hist.targ)))
        kfold.target <- temp.out$input$hist.targ
        kfold.predict <- temp.out$input$hist.pred
        kfold.gen <- temp.out$input$fut.pred
        remove(temp.out)
        if(test){
          #Add the save in here
          save(file="/home/cew/Code/testing/transform_tests/rdata/save2.R", list=c('kfold.target'))
        }
      }else{
        s5.instructions.window <- s5.instructions
      }
      #If a method that uses the OH data is used, perform a final trimming step
      #at this point to make sure that the OH and MF data start with the
      #same number of leap years
      if('OH'%in%ds.names){
        #         print(summary(mask.struct[[2]]))
        #         print(length(mask.struct[[2]]$masks[[window]]))
        #         print(summary(delta.mask))
        #         print(length(delta.mask))
        tmp.ds.time.mask <- apply.any.mask(data = mask.struct[[2]]$masks[[window]],
                                           mask=delta.mask,
                                           dim='temporal')
        tmp.ds.time.mask <- trim.time.mask(data=tmp.ds.time.mask,
                                           mask= mask.struct[[2]]$masks[[window]])
        #         print(length(tmp.ds.time.mask))
        #         print(length(kfold.target))
        kfold.target.final <- trim.time.mask(data=kfold.target, mask=tmp.ds.time.mask)
        #         print(which(is.na(tmp.ds.time.mask))[1:600])
        #         print(length(kfold.target.final))
        #         stop('examine this output')
      }else{
        kfold.target.final <- kfold.target
      }
      #perform downscaling on the series and merge into new vector
      if(create.ds.out){
        temp.out <- CallDSMethod(ds.method = downscale.fxn,
                                 train.predict = kfold.predict,
                                 train.target = kfold.target.final,
                                 esd.gen = kfold.gen,
                                 att.table=att.table,
                                 args=downscale.args,
                                 missval.handling=missval.handling, #Global set by SetDSMethodInfo()
                                 ds.var=ds.var,
                                 min.present=min.present, train_qcmask)
        tout.names <- names(temp.out)[!names(temp.out)%in%c('qc.mask')]
        #tout.names <- names(temp.out)[!names(temp.out)%in%c('qc.mask')]
        #print(tout.names)
        for(var.name in tout.names){
          if(use.time.trim.mask){
            temp.assign <- ttrim.mask
            temp.assign[!is.na(temp.assign)] <- temp.out[[var.name]]
            downscale.list[[var.name]][!is.na(ds.ttrim.mask)] <- as.numeric(temp.assign)[!is.na(ds.ttrim.mask)]#[!is.na(ttrim.mask)]
            if(train_qcmask){
              temp.assign[!is.na(temp.assign)] <- temp.out$qc.mask[[var.name]]
              #temp.out$qc.mask[[var.name]] <- trim.time.mask(temp.out$qc.mask[[var.name]],mask=ds.ttrim.mask)
              temp.out$qc.mask[[var.name]][!is.na(ds.ttrim.mask)] <- as.numeric(temp.assign)[!is.na(ds.ttrim.mask)]
            }
          }else{
            downscale.list[[var.name]][!is.na(ttrim.mask)] <- as.numeric(temp.out[[var.name]])#[!is.na(ttrim.mask)]
            #stop('examine output')
            if(train_qcmask){
              qc.masklist[[var.name]][!is.na(ttrim.mask)] <- as.numeric(temp.out$qc.mask[[var.name]])
            }
          }
          attributes(downscale.list[[var.name]]) <- attributes(temp.out[[var.name]])
        }#end for var in tout.names
      }#end if create.ds.out
      #And adjust the downscaled output, if applicable
      if(length(s5.instructions.window) > 0){
        if(verbose){message('calling section 5 adjustment')}
        if(train_qcmask){
          if(any(sapply(temp.out$qc.mask, function(x){sum(is.na(x))})!=0)){
            train.resize.sample=T
          }
          train.resize.sample <- F
        }else{
          train.resize.sample <- FALSE
        }
        if(test){
          save(file="/home/cew/Code/testing/transform_tests/rdata/save3.R", list=c('data'))
        }
        if(!train.resize.sample){
          data <- temp.out
          temp.out <- callS5Adjustment(s5.instructions = s5.instructions.window,
                                       #s5.method=s5.method,s5.args=s5.args,
                                       data = data[names(data)!='qc.mask'],
                                       data.atts= att.table,
                                       hist.pred = kfold.predict, #lapply(kfold.predict, remove.missvals),
                                       hist.targ = kfold.target, #kfold.target[!is.na(kfold.target)],
                                       fut.pred = kfold.gen) #lapply(kfold.gen, remove.missvals))
          if(test){
            #Add the save in here
            save(file="/home/cew/Code/testing/transform_tests/rdata/save4.R", list=c('temp.out'))
          }
          #for (new.ds.var in names(temp.out)){
          if(create.qc.mask && !(train_qcmask)){
            #if (verbose) {message('adding qc mask')}
            for(var.name in names(qc.masklist)){
              #intersection of time windowing mask and MF dataset
              if(use.time.trim.mask){
                temp.assign <- ttrim.mask
                temp.assign[!is.na(temp.assign)] <- temp.out$qc.mask[[var.name]]
                qc.masklist[[var.name]][!is.na(ds.ttrim.mask)] <- as.numeric(temp.assign)[!is.na(ds.ttrim.mask)]#[!is.na(ttrim.mask)]
              }else{
                qc.masklist[[var.name]][!is.na(ttrim.mask)] <- as.numeric(temp.out$qc.mask[[var.name]])#[!is.na(ttrim.mask)]
              }
            }
          }
          for(var.name in names(temp.out$ds.out)){
            if(use.time.trim.mask){
              temp.assign <- ttrim.mask
              temp.assign[!is.na(temp.assign)] <- temp.out$ds.out[[var.name]]
              downscale.list[[var.name]][!is.na(ds.ttrim.mask)] <- as.numeric(temp.assign)[!is.na(ds.ttrim.mask)]#[!is.na(ttrim.mask)]
            }else{
              downscale.list[[var.name]][!is.na(ttrim.mask)] <- as.numeric(temp.out$ds.out[[var.name]])#[!is.na(ttrim.mask)]
            }
          }
        }
      }
      if(graph){
        if(masklines){
          abline(v=which(!is.na(window.gen))[1])      #Option for plotting start of masks as | lines
        }
        points(seq(1:length(window.gen))[!is.na(window.gen)], downscale.list[!is.na(window.gen)],
               pch = (window-1), lwd = 1, col=mask.cols[window]) #ty = window, lwd = 4,
      }
      #Otherwise, you don't need to do anything because that loop should be full of NAs
      #       }else{
      #         print(paste("Too many NAs in loop", (window*length(kfold.mask))+kmask, "of", num.masks*length(kfold.mask), "; passing loop without downscaling"))
      #       }
    }
  }
  #stop('examine output')
  #print(summary(kfold.target))
  #print(lapply(downscale.list, summary))
  #print(summary(downscale.list$pr.r1i1p1))
  #However, if cross-validation is being performed
  #   } else {
  #     stop(paste("Cross validation not supported in FUDGE at this time; please run with k < 2"))
  #     #Remember to duplicate most of the structure from above; you're just adding a few new checks
  #   }
  #Exit loop
  #Make sure that dimensions are correct before leaving function
  if(!is.null(downscale.list)){
    lapply(downscale.list, 'assign.dim', c(1, 1, length(downscale.list[[1]])))
  }
  #Make sure that the qcmask consists of ones and NAs, not ones and zeroes
  if(train_qcmask){
    for(n in names(qc.masklist)){
      qc.masklist[[n]][qc.masklist[[n]]==0] <- NA
    }
  }
  #...also? This has the potential to cause issues when we start working with datasets that
  #happen to have missing values of their own - and might use other kinds of missing data.
  return(list('downscaled'=downscale.list, 'qc.mask'=qc.masklist)) #'postproc.out'=postproc.out))
}

#Converts NAs to 0, and all non-NA values to 1
#and returns the result in a 1-D form
convert.NAs<-function(dataset){
  dataset2<-dataset
  dataset2[is.na(dataset)]<-0
  dataset2[!is.na(dataset)]<-1
  return(as.vector(dataset2))
}

#Helper function for setting dims with lapply
assign.dim <- function(data, dimensions){
  dim(data) <- dimensions
}
