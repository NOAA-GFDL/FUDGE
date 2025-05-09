read.time.data <- function(input, calendar='julian', has_subset=F, subset=list()){
  #'Reads in data from a file, 
  #'sorting out the variables that use a time dimension (except for time_bnds)
  #'from the variables that do not.
  #'Also does the calendar checks, b/c it's best to get that out of the way soon.
  read_nc <- nc_open(input)
  struct <- list(time_dim = list(), 
                 dims = list(), 
                 time_vars = list(), 
                 vars = list())
  #Get the time dimension
  if (!('time') %in% names(read_nc$dim)){
    stop(paste("Error in reading file", input, "; no dim named time found!"))
  }
  struct$time_dim['time'] <- list(read_nc$dim$time)
  struct$time_dim$time$prec <- ncatt_get(read_nc, 'time', 'prec')
  #If doesn't have att, make double
  if (! struct$time_dim$time$prec$hasatt ){
    struct$time_dim$time$prec <- 'double'
  }else{
    struct$time_dim$time$prec <- struct$time_dim$time$prec$value
  }
  get.cal.strings()
  #Check on calendar
  if (calendar %in% julian.strings){
    if (struct$time_dim$time$calendar %in% julian.strings){
      stop(paste("Error in ConvertCalendar: both", struct$time_dim$time$calendar, 
                 "and", calendar, "are julian!"))
    }
  }else if (calendar %in% noleap.strings){
    if (struct$time_dim$time$calendar %in% noleap.strings){
      stop(paste("Error in ConvertCalendar: both", struct$time_dim$time$calendar, 
                 "and", calendar, "are noleap!"))
    }    
  }else if (calendar %in% cal360.strings){
    stop(paste("Error in ConvertCalendar: 360-day calendar", 
               calendar, "not yet supported!"))
  }else{
    stop(paste("Error in ConvertCalendar: command-line input calendar", 
               calendar, "not recognized! Please consult developer!"))
  }
  #Get the other dimensions that aren't time
  not_time_select <- names(read_nc$dim)[!(names(read_nc$dim) == 'time')]
  
  for (dname in not_time_select){
    if (!dname %in% names(subset)){
      struct$dims[[dname]] <- read_nc$dim[[dname]]
    }else{
      struct$dims[dname] <- read_nc$dim[dname]
      s_start <- subset[[dname]][1]
      s_end <- (subset[[dname]][1] + subset[[dname]][2]) - 1
      sub_vals <- struct$dims[[dname]]$vals[s_start:s_end]
      struct$dims[[dname]]$vals <- sub_vals
      struct$dims[[dname]]$len <- as.integer(subset[[dname]][2])
    }
  }
  ## original case - assumed no subsets
  #struct$dims <- read_nc$dim[not_time_select]
  #Iterate through the vars; if they use a time dim, store them
  var_names <- names(read_nc$var)
  struct$has_time_bnds <- F
  for (v in var_names){
    #Check for whether dim uses time
    #    print(unlist(lapply(read_nc$var[[v]]$dim, function(x){x$name})))
    uses_time_dim <- 'time' %in% unlist(lapply(read_nc$var[[v]]$dim, function(x){x$name}))
    if (uses_time_dim){
      if (v != 'time_bnds'){
        if (has_subset){
          struct$time_vars[[v]] <- clone.fudge.var.object(v, read_nc, subset=subset)
          #struct$time_vars[[v]] <- obtain.fudge.var(v, read_nc, subset=subset)
        }else{
          struct$time_vars[[v]] <- clone.fudge.var.object(v, read_nc)
        }
      }else{
        struct$has_time_bnds <- T
        struct$time_bnds_atts <- ncatt_get(read_nc, 'time_bnds')
      }
    }else{
      if (has_subset){
        struct$vars[[v]] <- clone.fudge.metavar.object(v, read_nc, subset=subset)
      }else{
        struct$vars[[v]] <- clone.fudge.metavar.object(v, read_nc)
      }
    }
  }
  #separately, and add to head of list when list is initialized
  nc_close(read_nc)
  return(struct)
}
