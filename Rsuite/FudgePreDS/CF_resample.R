# FOLLOWING 9 CASES exist, but only 5, 8, and 9 right now. NO mix and match.

# CASES
#
# 1. noleap/gregorian
#   - repeat noleap
#   - index to non-leap greg days
#   - repeat 02-28's to 02-29's
#
# 2. gregorian/noleap
#   - remove hist.leapdays
#   - straight repeat
#
# 3. 360/gregorian
#   - not sure yet
#
# 4. gregorian/360
#   - not sure yet
#
# 5. gregorian/gregorian
#   - find leapdays in historical time series (ts) and future ts
#   - remove enough days from beginning of historical ts so leapdays line up
#   - repeat while length of new ts is shorter than length of future ts
#
# 6. noleap/360
#   - not sure yet
#
# 7. 360/noleap
#   - not sure yet
#
# 8. 360/360
#   straight repeat
#
# 9. noleap/noleap
#   straight repeat

## PROBLEM CASE?
# hist_first_year <- 1979
# hist_last_year <- 2005
#
# fut_first_year <- 2007
# fut_last_year <- 2099

# ensemble limits? 1 to 12 ?


CF_resample <- function(hist_dates, fut_dates, hist_caltype, fut_caltype) {
	if (hist_caltype != fut_caltype){
		warning(paste("hist_caltype:", hist_caltype, "and fut_caltype:", fut_caltype))
		stop("Historical calendar type must match future calendar type!")
	}
  nrips = 1
  cf_fut_masks = list()
  get.cal.strings()
  hist_hascal = (hist_caltype != "") || (hist_caltype %in% none.strings)
  fut_hascal = (fut_caltype != "") || (fut_caltype %in% none.strings)
  for (n in 1:nrips){
    if (hist_hascal & fut_hascal){
      # Note: as of 02-2020, doing gregorian resample for all calendar
      # types, to faciliate intercomparison of data
      cf_fut_idx <- gregorian_resample(hist_dates, fut_dates)
    } else {
      cf_fut_idx <- simple_resample(hist_dates, fut_dates)
    }
    cf_fut_masks[[n]] <- cf_fut_idx
  }
    
	return(cf_fut_masks)
}

	# CASE 5: gregorian/gregorian
gregorian_resample <- function(hist_dates, fut_dates) {
	# length of original hist_dates vector
	hist_day_count <- length(hist_dates)
	fut_day_count <- length(fut_dates)
	message("Resampling historical obs to future time series length:")
	message(ifelse(hist_day_count < fut_day_count, "FUTURE longer", "HISTORICAL longer"))
	min_idx = min(hist_day_count, fut_day_count)
	# logical vectors indicating positions of leapdays in historical and future time periods
	# 02-2020: can't search for Feb. 29th b/c we want to do this for 365-day 
	# and 360-day data too
	leapyears <- c(1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 
	               1940, 1944, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 
	               1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 
	               2020, 2024, 2028, 2032, 2036, 2040, 2044, 2048, 2052, 2056, 
	               2060, 2064, 2068, 2072, 2076, 2080, 2084, 2088, 2092, 2096)
	hist_leapdays <- format(hist_dates,"%m-%d") == "02-28" & (as.numeric(format(hist_dates,"%Y")) %in% leapyears)
	fut_leapdays <- format(fut_dates,"%m-%d") == "02-28" & (as.numeric(format(fut_dates,"%Y")) %in% leapyears)
	  
	# duplicate the future vector into a temp that can be altered in the while loop
	tmp_fut_leapdays <- fut_leapdays
	# initialize the index vector that will map historical dates to future dates
	cf_fut_idx <- c()
	# keep adding pieces of the hist index to the cf_fut_idx until it is at least the length of future dates
	while(length(cf_fut_idx) < fut_day_count) {
		# which day is the first leapday in the historical?
		hist_first_leap <- min(which(hist_leapdays))
	  # which day is the first leapday in the future that we haven't matched yet?
	  fut_first_leap <-  min(which(tmp_fut_leapdays))
	  # how many days do we need to remove to line them up?
	  leap_offset <- hist_first_leap - fut_first_leap
		# a way to handle case when first fut leap year occurs before first hist leap year
		if (leap_offset < 0) {
		  hist_first_leap <- which(hist_leapdays)[[2]]
		  leap_offset <- hist_first_leap - fut_first_leap
		}
	  # generate index positions from there to the end of the historical dates
	  cf_fut_idx_tmp <- ((leap_offset+1):hist_day_count) #[1:min_idx]
	  # add those index positions to the new index vector
	  cf_fut_idx <- c(cf_fut_idx, cf_fut_idx_tmp)
	  # now that those dates have been matched, remove them from the future days still needing matches
	  tmp_fut_leapdays <- tmp_fut_leapdays[-c(1:length(cf_fut_idx_tmp))]
	}
	return(cf_fut_idx[1:fut_day_count])
}

# CASES 8 & 9
simple_resample <- function(hist_dates, fut_dates) {
		# length of original hist_dates vector
		hist_day_count <- length(hist_dates)
		fut_day_count <- length(fut_dates)
		message("Resampling historical obs to future time series length:")
		message(ifelse(hist_day_count < fut_day_count, "FUTURE longer", "HISTORICAL longer"))
		cf_fut_idx <- rep(1:hist_day_count, length.out=fut_day_count)
		return(cf_fut_idx)
}


test_fun <- function(hist_caltype = "gregorian",
                     fut_caltype = "gregorian",
                     hist_first_year = 1979, hist_last_year = 2005,
                     fut_first_year = 2041, fut_last_year = 2099) {
  ##############################################################
  ## Including simulated time series to "unit test" this idea ##
  ##############################################################
  
  hist_last <- ifelse(hist_caltype=="360_day",
                      paste0(hist_last_year,"-12-30"),
                      paste0(hist_last_year,"-12-31"))
  fut_last <- ifelse(fut_caltype=="360_day",
                     paste0(fut_last_year,"-12-30"),
                     paste0(fut_last_year,"-12-31"))
  
  hist_dates <- seq(as.PCICt(paste0(hist_first_year,"-01-01"), cal=hist_caltype),
                    as.PCICt(hist_last, cal=hist_caltype),
                    by="day")
  fut_dates <- seq(as.PCICt(paste0(fut_first_year,"-01-01"), cal=fut_caltype),
                   as.PCICt(fut_last, cal=fut_caltype),
                   by="day")
  
  CF_resample(hist_dates, fut_dates, hist_caltype, fut_caltype)
}



