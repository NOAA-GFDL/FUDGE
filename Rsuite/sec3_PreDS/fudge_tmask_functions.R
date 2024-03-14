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
  if (all(tmask.filenames %in% c('na', 'none'))){
    tmask.filenames <- list()
    apply_tmasks = F
  }else{
    for (ftype in tmask.list.names){
      tmask.filenames[[ftype]] <- file.path(TMPDIR, tmask.filenames[[ftype]])
    }
  }
  #Check for whether there is an indexing mask used by the ds data
  #(formerly known as the time-trimming mask)
  is_indexed_with_tmask <- !(aux.ref$DS$indexing_tmask %in%  c('na', 'none'))
  # #Either all data will use a time-windowing mask, or none will use it. 
  # if (ds.ref$train_and_use_same){ 
  #   #Future data used in downscaling will be underneath the fut.time tag
  #   if(!is_indexed_with_tmask){
  #     if(apply_tmasks){
  #       tmask.list <- CreateTimeWindowList(hist.train.mask = tmask.filenames$MH, 
  #                                          hist.targ.mask = tmask.filenames$OH, 
  #                                          esd.gen.mask = tmask.filenames$MF, k=aux.ref$kfold, 
  #                                          method=ds.ref$method, 
  #                                          train.and.use.same = ds.ref$train_and_use_same)
  #       names(tmask.list) <- tmask.list.names
  #       nmasks <- length(tmask.list$OH$masks)
  #     }else{
  #       message("no time windows included; moving on to next step")
  #       tmask.list <- list("na")
  #     }
  #   }else{
  #     #If there is a time trimming mask, and there are masks of all kinds
  #     tmask.list <- CreateTimeWindowList(hist.train.mask = tmask.filenames$MH, 
  #                                        hist.targ.mask = tmask.filenames$OH, 
  #                                        esd.gen.mask = tmask.filenames$MF, k=aux.ref$kfold, 
  #                                        method=ds.ref$method, 
  #                                        time.prune.mask = aux.ref$DS$indexing_tmask,
  #                                        train.and.use.same = ds.ref$train_and_use_same)
  #     names(tmask.list) <- c(tmask.list.names, "trim") #time.trim.mask
  #     tmask.filenames <- c(tmask.filenames, list('trim' = aux.ref$DS$indexing_tmask))
  #     nmasks <- length(tmask.list$OH$masks)
  #     if (check_mask_olap(tmask.list$MF$masks)){
  #       time_mask_overlap <- T
  #     }
  #   }
  # }else{ 
  tus <- ds.ref$train_and_use_same
  tus <- FALSE
    if(!is_indexed_with_tmask){
      if(apply_tmasks){
        tmask.list <- CreateTimeWindowList(hist.train.mask = tmask.filenames$OH, 
                                           hist.targ.mask = tmask.filenames$MH, 
                                           esd.gen.mask = tmask.filenames$MF, k=aux.ref$kfold, 
                                           method=ds.ref$method, 
                                           train.and.use.same = tus)
        names(tmask.list) <- tmask.list.names
        nmasks <- length(tmask.list$OH$masks)
      }else{
        #Otherwise, if there were no masks included at all
        message("no time windows included; moving on to next step")
        tmask.list <- list("na")
      }
    }else{
      tmask.list <- CreateTimeWindowList(hist.train.mask = tmask.filenames$OH, 
                                         hist.targ.mask = tmask.filenames$MH, 
                                         esd.gen.mask = tmask.filenames$MF, k=aux.ref$kfold, 
                                         method=ds.ref$method, 
                                         time.prune.mask = aux.ref$DS$indexing_tmask,
                                         train.and.use.same = tus)
      names(tmask.list) <- c(tmask.list.names, "trim")
      tmask.filenames <- c(tmask.filenames, list('trim' = aux.ref$DS$indexing_tmask))
      nmasks <- length(tmask.list$OH$masks)
      time_mask_overlap <- T
    }
#  }
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
    fut_calendar <- MF$metadata[[1]]$dim$time$calendar
    hist_calendar <- OH$metadata[[1]]$dim$time$calendar
    if (fut_calendar != 'none'){
      fut.tseries <- get.PCICt.time.series(MF$metadata[[1]]$dim$time$units, 
                                           MF$metadata[[1]]$dim$time$vals, 
                                           MF$metadata[[1]]$dim$time$calendar)
      hist.tseries <- get.PCICt.time.series(MH$metadata[[1]]$dim$time$units, 
                                            MH$metadata[[1]]$dim$time$vals, 
                                            MH$metadata[[1]]$dim$time$calendar)
    }else{
      fut.tseries <- MF$metadata[[1]]$dim$time$vals
      hist.tseries <- MH$metadata[[1]]$dim$time$vals
    }
    resample_method = 'cf_resample'
    if (resample_method == 'cf_resample'){
      #Added November 2019 - resamples based off fo four-year leapday cycles
      # Current structure of return is a separate list for each ensemble,
      # which accounts for year cycling
      tmask.list$cf_resample <- CF_resample(hist.tseries, fut.tseries, 
                                            hist_calendar, fut_calendar)
    }else{
      # Resample based off of adding/removing leapdays - used in i,j,k
      nyears_hist = length(unique(format(hist.tseries, "%Y")))
      nyears_fut = length(unique(format(fut.tseries, "%Y")))
      if (hist.cal %in% cal360.strings()){
        hist.year.div = which(format(hist.tseries, '%m-%d') == "12-30")
        fut.year.div = which(format(fut.tseries, '%m-%d') == "12-30")
      }else if (hist.cal == 'none'){
        hist.year.div = length(hist.tseries)
        fut.year.div = length(fut.tseries)
      }else{
        # Note to self: add/remove leapday indexing may not be accurate when 
        # working with years that don't start on Jan. first. Keep in mind for 
        # debugging.
        hist.year.div <- which(format(hist.tseries, "%m-%d") == '12-31')
        fut.year.div <- which(format(hist.tseries, "%m-%d") == '12-31')
      }
      #start_year = (1 + (n-1) * oh_offset)
      tmask.list$cf_resample <- get.OH.time.index(nyears_hist, nyear_fut, 
                                                  hist.year.div, fut.year.div, 
                                                  start_year = 1)
    }
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
  #'Rewriting the above with lessons from the section 8 tools - 
  #'we get slightly better performance with a broadcast rather than an aperm
  data.dim <- dim(data)
  if (length(data) == length(mask)){
    result <- as.vector(data) * as.vector(mask)
    dim(result) <- data.dim
  }else{
    result <- tmask.repeat(mask, data.dim) * data
  }
  return(result)
}

tmask.repeat <- function(tmask, dims){
  tmask.array <- sapply(tmask, rep, times=prod(dims[-length(dims)]))
  dim(tmask.array) <- dims
  return(tmask.array)
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

