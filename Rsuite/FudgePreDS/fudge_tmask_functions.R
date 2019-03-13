#fudge_tmask_functions.R
#Functions relating to how FUDGE deals with time masks.
#Covers mask application, qc functions and convenience wrappers for
#handling long qc checks.

get.fudge.timemask.list <- function(aux.ref, ds.ref, TMPDIR){
  #'Wrapper funciton for the time masking operations; removed to a separate
  #'function for readability in the main driver.
  
  time_mask_overlap = F
  apply_tmasks = T
  nmasks <- 1
  #tmask.list.names <- c("train.pred", "train.targ", "fut.pred")
  tmask.list.names <- c("OH", "MH","MF")
  tmask.filenames <- list()
  for (ftype in tmask.list.names){
    #Note: we only get one tmask per dataset type now, though we support
    #one per dataset type / var combo in the input data struct.
    tmask.filenames[[ftype]] <- aux.ref[[ftype]][[1]]$tmask
  }
  if (all(tmask.filenames=='na')){
    tmask.filenames <- list()
    apply_tmasks = F
  }else{
    for (ftype in tmask.list.names){
      tmask.filenames[[ftype]] <- file.path(TMPDIR, tmask.filenames[[ftype]])
    }
  }
  #Check for whether there is an indexing mask used by the ds data
  #(formerly known as the time-trimming mask)
  is_indexed_with_tmask <- aux.ref$DS$indexing_tmask != 'na'
  #Either all data will use a time-windowing mask, or none will use it. 
  if (ds.ref$train_and_use_same){ 
    #Future data used in downscaling will be underneath the fut.time tag
    if(!is_indexed_with_tmask){
      if(apply_tmasks){
        tmask.list <- CreateTimeWindowList(hist.train.mask = tmask.filenames$MH, 
                                           hist.targ.mask = tmask.filenames$OH, 
                                           esd.gen.mask = tmask.filenames$MF, k=aux.ref$kfold, 
                                           method=ds.ref$method, 
                                           train.and.use.same = ds.ref$train_and_use_same)
        names(tmask.list) <- tmask.list.names
        nmasks <- length(tmask.list$OH$masks)
      }else{
        message("no time windows included; moving on to next step")
        tmask.list <- list("na")
      }
    }else{
      #If there is a time trimming mask, and there are masks of all kinds
      tmask.list <- CreateTimeWindowList(hist.train.mask = tmask.filenames$MH, 
                                         hist.targ.mask = tmask.filenames$OH, 
                                         esd.gen.mask = tmask.filenames$MF, k=aux.ref$kfold, 
                                         method=ds.ref$method, 
                                         time.prune.mask = aux.ref$DS$indexing_tmask,
                                         train.and.use.same = ds.ref$train_and_use_same)
      names(tmask.list) <- c(tmask.list.names, "trim") #time.trim.mask
      tmask.filenames <- c(tmask.filenames, list('trim' = aux.ref$DS$indexing_tmask))
      nmasks <- length(tmask.list$OH$masks)
      if (check_mask_olap(tmask.list$MF$masks)){
        time_mask_overlap <- T
      }
    }
  }else{ 
    if(!is_indexed_with_tmask){
      if(apply_tmasks){
        tmask.list <- CreateTimeWindowList(hist.train.mask = tmask.filenames$MH, 
                                           hist.targ.mask = tmask.filenames$OH, 
                                           esd.gen.mask = tmask.filenames$MF, k=aux.ref$kfold, 
                                           method=ds.ref$method, 
                                           train.and.use.same = ds.ref$train_and_use_same)
        names(tmask.list) <- tmask.list.names
        nmasks <- length(tmask.list$OH$masks)
      }else{
        #Otherwise, if there were no masks included at all
        message("no time windows included; moving on to next step")
        tmask.list <- list("na")
      }
    }else{
      tmask.list <- CreateTimeWindowList(hist.train.mask = tmask.filenames$MH, 
                                         hist.targ.mask = tmask.filenames$OH, 
                                         esd.gen.mask = tmask.filenames$MF, k=aux.ref$kfold, 
                                         method=ds.ref$method, 
                                         time.prune.mask = aux.ref$DS$indexing_tmask,
                                         train.and.use.same = ds.ref$train_and_use_same)
      names(tmask.list) <- c(tmask.list.names, "trim")
      tmask.filenames <- c(tmask.filenames, list('trim' = aux.ref$DS$indexing_tmask))
      nmasks <- length(tmask.list$OH$masks)
      time_mask_overlap <- T
    }
  }
  #Do explicit checks on the members of masks
  masknames <- names(tmask.list$OH$masks)
  for (el in names(tmask.list)){
    if (length(tmask.list[[el]]$masks) != nmasks){
      stop(paste("Error in get.fudge.timemask.list: the", el, "dataset", 
                 "had", length(tmask.list[[el]]$masks), "masks, not", nmasks, "!"))
    }
    el_masknames <- names(tmask.list[[el]]$masks)
    if (any(el_masknames != masknames)){
      stop(paste("Error in get.fudge.timemask.list: the names of masks in OH and", 
                 el, "datasets are not identical!"))
    }
  }
  
  ###Do OH-downscaling specific mask-reading
  if (ds.ref$downscale_oh){
    #Create global reference vectors for where leapdays are in the input
    #historical and future datasets
    has_calendar = TRUE
    if (!apply_tmasks){
      warning(paste("Warning in get.fudge.tmask.list: you should use a tmask if", 
                    "working with delta and/or CFQM unless you are working with", 
                    "synthetic data!"))
      print(MF$metadata[[1]]$dim$time$calendar)
      tmask.list$OH$masks[[1]] <- rep(1, length(OH$metadata[[1]]$dim$time$vals))
      if (MF$metadata[[1]]$dim$time$calendar != "none"){      
        fut.tseries <- get.PCICt.time.series(MF$metadata[[1]]$dim$time$units, 
                                             MF$metadata[[1]]$dim$time$vals, 
                                             MF$metadata[[1]]$dim$time$calendar)
        hist.tseries <- get.PCICt.time.series(OH$metadata[[1]]$dim$time$units, 
                                              OH$metadata[[1]]$dim$time$vals, 
                                              OH$metadata[[1]]$dim$time$calendar)
        tmask.list$hist.leapmask <- make.leapday.mask(hist.tseries)
        tmask.list$fut.leapmask <- make.leapday.mask(fut.tseries)
        OH_years <- get_tmask_years(OH$metadata[[1]]$filename)
        MF_years <- get_tmask_years(MF$metadata[[1]]$filename)
      }else{
        #This case is used for the synthetic data - no leapdays to be found.
        has_calendar = FALSE
        tmask.list$hist.leapmask <- rep(1, length(OH$metadata[[1]]$dim$time$vals))
        tmask.list$fut.leapmask <- rep(1, length(MF$metadata[[1]]$dim$time$vals))
      }
    }else{
      print("getting the leapday masks")
      OH_years <- get_tmask_years(tmask.filenames$OH)
      MF_years <- get_tmask_years(tmask.filenames$MF)
      tmask.list$hist.leapmask <- get.leapday.mask(OH_years$yearstring,
                                                   OH$metadata[[1]]$dim$time$calendar, 
                                                   origin=OH$metadata[[1]]$dim$time$units,
                                                   time.data=OH$metadata[[1]]$dim$time$vals)
      tmask.list$fut.leapmask <- get.leapday.mask(MF_years$yearstring,
                                                  MF$metadata[[1]]$dim$time$calendar, 
                                                  origin = MF$metadata[[1]]$dim$time$units,
                                                  time.data=MF$metadata[[1]]$dim$time$vals)
    }
    if (has_calendar){
      print("getting year divisions")
      tmask.list$hist.year.div <- get.year.divisions(OH_years$start, 
                                                     OH_years$end, 
                                                     OH$metadata[[1]]$dim$time$calendar)
      tmask.list$fut.year.div <- get.year.divisions(MF_years$start, 
                                                    MF_years$end, 
                                                    MF$metadata[[1]]$dim$time$calendar)
      tmask.list$nyears_hist <- length(unique(cut(1:oh.dim[6], breaks=tmask.list$hist.year.div)))
      tmask.list$nyears_fut <- length(unique(cut(1:mf.dim[6], breaks=tmask.list$fut.year.div)))
#       print(tmask.list$nyears_hist)
#       print(tmask.list$nyears_fut)
#       stop('examine this output')
    }else{
      tmask.list$hist.year.div <- length(tmask.list$hist.leapmask)
      tmask.list$fut.year.div <- length(tmask.list$fut.leapmask)
      tmask.list$nyears_hist <- 1
      tmask.list$nyears_fut <- 1
    }
    #     print(tmask.list$hist.year.div)
    #     print(tmask.list$fut.year.div)
    #     print(tmask.list$nyears_hist)
    #     print(tmask.list$nyears_fut)
    #     stop('examine this output. Does it make sense?')
  }
  ###Set booleans for handy access
  print(paste("Applying overlapping time window masks:", time_mask_overlap))
  tmask.list$time_mask_overlap <- time_mask_overlap
  tmask.list$apply_tmasks <- apply_tmasks
  tmask.list$nmasks <- nmasks
  tmask.list$filenames <- tmask.filenames
  tmask.list$calendar <- MF$metadata[[1]]$dim$time$calendar
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

check_mask_olap <- function(data.list){
  #'Checks the masks for overlapping along the time axis
  out <- rep(0, length(data.list[[1]]))
  for (n in 1:length(data.list)){
    data.list[[n]][is.na(data.list[[n]])] <- 0
    out <- out + data.list[[n]]
  } 
  return(any(out > 1))
}

get_tmask_years <- function(tmask_file){
  #Gets the year ranges that the data uses
  tmask_nc <- nc_open(tmask_file)
  tmask_time <- ncvar_get(tmask_nc, 'time')
  tmask_cal <- ncatt_get(tmask_nc, 'time', 'calendar')$value
  tmask_units <- ncatt_get(tmask_nc, 'time', 'units')$value
  nc_close(tmask_nc)
  trange <- get.PCICt.time.range(tmask_units, tmask_time, tmask_cal)
  yrs <- format(trange, '%Y%m%d')
  out <- list()
  out$start <- yrs[1]
  out$end <- yrs[2]
  out$yearstring <- paste0(yrs[1], "-", yrs[2])
  return(out)
}

parse_tmask_years <- function(tmask_file){
  #Builds the strings that govern tmask start and end dates from
  #an esd-standard tmask name. Technically, also works with filenames. 
  tmask_name <- basename(tmask_file)
  tmask_split <- unlist(strsplit(tmask_file, '_'))
  tstring <- tmask_split[sapply(tmask_split, function(x){grepl('-', x, fixed=T)})]
  if (length(tstring) > 1){
    #Then tstring is the one that is only 8 + 1 + 8 chars long
    tstring <- tstring[sapply(tstring, function(x){nchar(x) == 17})]
  }
  #This will either have only 8 char. with a - in the end or have a .nc at end
  out <- list()
  out$start <- substr(tstring, 1, 8)# first 8
  out$end <- substr(tstring, 10, 17) #last 8 after the dash
  out$yearstring <- tstring #paste0(out$start, '-', out$end)
  return(out)
}

