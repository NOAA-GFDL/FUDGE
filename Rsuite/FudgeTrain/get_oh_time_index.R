
get.OH.time.index <- function(nyears_hist, nyears_fut, hist.year.div, fut.year.div, 
                              start_year=1){
  #'Generates fut_time_len samples from hist_time_len, repeating hist samples 
  #'if fut timeseries is longer, and subsampling hist samples if fut time len is 
  #'shorter.
  #' Used by: delta downscaling, CFQM downscaling, the pr masking functions when
  #' OH is used to downscale
  if (nyears_hist > nyears_fut){
    if (start_year > 1){
      start_idx <- hist.year.div[start_year - 1] + 1
    }else{
      start_idx <- 1
    }
    tmp.ndays <- start_idx:hist.year.div[nyears_fut + start_year - 1]
    max.ndays <- start_idx:hist.year.div[length(hist.year.div)]
    if (hist.year.div[start_year + nyears_fut - 1] <= hist.year.div[length(hist.year.div)]){
  #  if (start_idx + hist.year.div[start_year] <= hist.year.div[length(hist.year.div)]){
   #if (length(tmp.ndays) >= max.ndays){
      #Net next nyears samples
      OH.idx <- start_idx:hist.year.div[nyears_fut + start_year - 1]
      #start_idx <- start_idx + hist.year.div[nyears_fut + start_year]
    }else{
      #Get last n samples from end of OH; cycle around to beginning
      year_diff = nyears_hist - start_year
      OH.idx <- c(start_idx:hist.year.div[length(hist.year.div)], 
                  hist.year.div[1]:hist.year.div[year_diff - 1])
      #start_idx <- OH.idx[length(OH.idx)] + 1
    }
    start_year <- ((start_year  + nyears_fut) %% nyears_hist) #I'm not sure the +1 that used to be here is needed
  }else{
    #If you are repeating OH samples to make up MF
    #....this should be based off of YEARS, not simple indices
    #And you are going to be working on these as a function of the 
    hist.max <- hist.year.div[length(hist.year.div)]
    start_idx <- fut.year.div[start_year]
    start.vec <- start_idx:hist.max
    num.rep <- floor(nyears_fut/nyears_hist)
    num.remain <- nyears_fut %% nyears_hist
    idx.remain <- hist.year.div[num.remain]
    OH.idx <- c(rep(1:hist.max, num.rep), 
                1:idx.remain)
  }
  return(list(index=OH.idx, end=start_year))
}
