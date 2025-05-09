#!/bin/Rscript
#make_time_subset.R

make_time_subset <- function(test.nc, start_date, end_date){
  #'This makes a time subset that is roughly compatible with the
  #'existing FudgeIO_reboot subset capabilities
  #This assumes that you already have the nc file open
  dim_vals <- test.nc$dim$time$vals
  if ('time_bnds' %in% names(test.nc$var)){
    bnds_vals <- ncvar_get(test.nc, 'time_bnds')
  }else{
    #DIY bnds (from S8Utils)
    bnds_vals <- build.bnds(dim_vals)
  }
  time.units <- test.nc$dim$time$units
  time.calendar <- test.nc$dim$time$calendar
  #time.subset is located in trim_time_axis.R
  subset = time.subset(start_date, end_date, 
                       dim_vals, bnds_vals, 
                       time.calendar, time.units)
  return(list('time'=c(subset$subset[[1]], 
                       subset$subset[[2]] + subset$subset[[1]])))
}

build_time_subset <- function(file1, start_date='none', end_date='none'){
  #'Helpful wrapper function for make_time_subset. Takes some of the editing 
  #'burden away from individual section 8 tools.
  test.nc <- nc_open(file1)
  if (start_date == 'none'){
    #start date is first date in file
    start_date  = format(get.PCICt.time.series(time.data = test.nc$dim$time$vals[1], 
                                               origin=test.nc$dim$time$units, 
                                               calendar=test.nc$dim$time$calendar), 
                         '%Y-%m-%d')
  }else{
    start_date = paste0(start_date, ":12")
  }
  if (end_date == 'none'){
    #end date is last date in file
    start_date  = format(get.PCICt.time.series(time.data = test.nc$dim$time$vals[length(test.nc$dim$time$vals)], 
                                               origin=test.nc$dim$time$units, 
                                               calendar=test.nc$dim$time$calendar), 
                         '%Y-%m-%d')
  }else{
    end_date = paste0(end_date, ":12")
  }
  time_subset = make_time_subset(test.nc, start_date, end_date)
  nc_close(test.nc)
  return(time_subset)
}

make_time_subset_string <- function(time.dim){
  #Makes the string showing the date range
  time_start <- get.PCICt.time.series(time.dim$units, time.dim$vals[1], time.dim$calendar)
  time_end <- get.PCICt.time.series(time.dim$units, 
                                    time.dim$vals[length(time.dim$vals)], 
                                    time.dim$calendar)
  subset_string <- paste("from", format(time_start, "%Y-%m-%d"), 'to', format(time_end, "%Y-%m-%d"))
  return(subset_string)
}