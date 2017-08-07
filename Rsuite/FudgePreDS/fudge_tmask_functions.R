#fudge_tmask_functions.R
#Functions relating to how FUDGE deals with time masks.
#Covers mask application, qc functions and convenience wrappers for
#handling long qc checks.

get.fudge.timemask.list <- function(){
  #'Wrapper funciton for the time masking operations; removed to a separate
  #'function for readability in the main driver.
  
  time_mask_overlap = F
  apply_tmasks = T
  nmasks <- 1
  #tmask.list.names <- c("train.pred", "train.targ", "fut.pred")
  tmask.list.names <- c("MH", "OH", "MF")
  tmask.filenames <- list("OH" = target.time.window, "MH" = hist.time.window, "MF" = fut.time.window)
  if (all(tmask.filenames=='na')){
    tmask.filenames <- list()
  }
  #Either all data will use a time-windowing mask, or none will use it. 
  if (ds.ref$train_and_use_same){ 
    #Future data used in downscaling will be underneath the fut.time tag
    if(fut.time.trim.mask=='na'){
      #If there is no time trimming mask:
      print(paste("time trimming mask", fut.time.trim.mask))
      if(target.time.window!='na'){
        #If there are masks included (this should be the most common use case)
        message('Creating list of time windows')
        tmask.list <- CreateTimeWindowList(hist.train.mask = hist.time.window, hist.targ.mask = target.time.window, 
                                           esd.gen.mask = fut.time.window, k=k.fold, 
                                           method=ds.method, 
                                           train.and.use.same = ds.ref$train_and_use_same)
        names(tmask.list) <- tmask.list.names
        nmasks <- length(tmask.list$OH$masks)
      }else{
        #Otherwise, if there were no masks included at all
        message("no time windows included; moving on to next step")
        tmask.list <- list("na")
        apply_tmasks = F
      }
    }else{
      #If there is a time trimming mask, and there are masks of all kinds
      print(paste("time trimming mask", fut.time.trim.mask))
      tmask.list <- CreateTimeWindowList(hist.train.mask = hist.time.window, hist.targ.mask = target.time.window, 
                                         esd.gen.mask = fut.time.window, k=k.fold, method=ds.method, 
                                         time.prune.mask = fut.time.trim.mask, 
                                         train.and.use.same = ds.ref$train_and_use_same)
      names(tmask.list) <- c(tmask.list.names, "trim") #time.trim.mask
      tmask.filenames <- c(tmask.filenames, list('trim' = fut.time.trim.mask))
      nmasks <- length(tmask.list$OH$masks)
      if (grepl("_olap", tmask.filenames$MF, fixed=T)){
        time_mask_overlap <- T
      }
    }
  }else{ #Once esd.gen implemented, should be most common use case
    #Data used in downscaling (as opposed to training ) will be underneath the esdgen tag
    if(fut.time.trim.mask=='na'){
      if(target.time.window!='na'){
        tmask.list <- CreateTimeWindowList(hist.train.mask = hist.time.window, hist.targ.mask = target.time.window, 
                                           esd.gen.mask = fut.time.window, k=kfold, method=ds.method, 
                                           train.and.use.same = ds.ref$train_and_use_same)
        names(tmask.list) <- tmask.list.names
        nmasks <- length(tmask.list$OH$masks)
      }else{
        #Otherwise, if there were no masks included at all
        message("no time windows included; moving on to next step")
        tmask.list <- list("na")
        apply_tmasks = F
      }
    }else{
      print(paste("time trimming mask", fut.time.trim.mask))
      tmask.list <- CreateTimeWindowList(hist.train.mask = hist.time.window, hist.targ.mask = target.time.window, 
                                         esd.gen.mask = fut.time.window, k=k.fold, method=ds.method, 
                                         time.prune.mask = fut.time.trim.mask, 
                                         train.and.use.same = ds.ref$train_and_use_same)
      names(tmask.list) <- c(tmask.list.names, "trim")
      tmask.filenames <- c(tmask.filenames, list('trim' = fut.time.trim.mask))
      nmasks <- length(tmask.list$OH$masks)
      time_mask_overlap <- T
    }
  }
  ###Do OH-downscaling specific mask-reading
  if (ds.ref$downscale_oh){
    #Create global reference vectors for where leapdays are in the input
    #historical and future datasets
    tmask.list$hist.leapmask <- get.leapday.mask(paste0(target.file.start.year_1, "-", 
                                                  target.file.end.year_1), 
                                           OH$metadata[[1]]$calendar)
    tmask.list$fut.leapmask <- get.leapday.mask(paste0(fut.file.start.year_1, "-", 
                                                 fut.file.end.year_1), 
                                          MF$metadata[[1]]$calendar)
    tmask.list$hist.year.div <- get.year.divisions(target.file.start.year_1, 
                                         target.file.end.year_1, 
                                         OH$metadata[[1]]$calendar)
    tmask.list$fut.year.div <- get.year.divisions(fut.file.start.year_1, 
                                        fut.file.end.year_1, 
                                        MF$metadata[[1]]$calendar)
    tmask.list$nyears_hist <- length(unique(cut(1:oh.dim[6], breaks=tmask.list$hist.year.div)))
    tmask.list$nyears_fut <- length(unique(cut(1:mf.dim[6], breaks=tmask.list$fut.year.div)))
  }
  ###Set booleans for handy access
  print(paste("Applying overlapping time window masks:", time_mask_overlap))
  tmask.list$time_mask_overlap <- time_mask_overlap
  tmask.list$apply_tmasks <- apply_tmasks
  tmask.list$nmasks <- nmasks
  tmask.list$filenames <- tmask.filenames
  #print(str(tmask.list))
  return(tmask.list)
}


apply.fudge.tmask <- function(data, mask){
  #Fast and dirty adaptation of apply.any.mask
  #for use with 6-dimensional output
  #print('applying tmask')
  data <- aperm(data, c(6,1,2,3,4,5))
  data.dim <- dim(data)
  result <- rep(mask, length.out=length(data)) * as.vector(data)
  dim(result) <- data.dim
  return(aperm(result, c(2,3,4,5,6,1)))
}

