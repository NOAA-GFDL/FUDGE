
# get.OH.time.index <- function(nyears_hist, nyears_fut, hist.year.div, fut.year.div, 
#                               start_year=1){
#   #'Generates fut_time_len samples from hist_time_len, repeating hist samples 
#   #'if fut timeseries is longer, and subsampling hist samples if fut time len is 
#   #'shorter.
#   #' Used by: delta downscaling, CFQM downscaling, the pr masking functions when
#   #' OH is used to downscae
# #   print('num years in hist and fut')
# #   print(nyears_hist)
# #   print(nyears_fut)
#   if (nyears_hist > nyears_fut){
#     if (start_year > 1){
#       start_idx <- hist.year.div[start_year - 1] + 1
#     }else{
#       start_idx <- 1
#     }
# #     print(start_idx)
# #     print(nyears_fut)
# #     print(start_year)
# #     print(hist.year.div)
# #     print((nyears_fut + start_year - 1) %% nyears_hist)
#     tmp.ndays <- start_idx:hist.year.div[ (nyears_fut + start_year - 1) %% nyears_hist]
#     max.ndays <- start_idx:hist.year.div[length(hist.year.div)]
#     if (hist.year.div[start_year + nyears_fut - 1] <= hist.year.div[length(hist.year.div)]){
#       #Net next nyears samples
#       OH.idx <- start_idx:hist.year.div[nyears_fut + start_year - 1]
#     }else{
#       #Get last n samples from end of OH; cycle around to beginning
#       year_diff = nyears_hist - start_year
#       OH.idx <- c(start_idx:hist.year.div[length(hist.year.div)], 
#                   hist.year.div[1]:hist.year.div[year_diff - 1])
#     }
#     start_year <- ((start_year  + nyears_fut) %% nyears_hist) #I'm not sure the +1 that used to be here is needed
#   }else{
#     #If you are repeating OH samples to make up MF, go by the number of
#     #years present
#     hist.max <- hist.year.div[length(hist.year.div)]
#     start_idx <- fut.year.div[start_year]
#     start.vec <- start_idx:hist.max
#     num.rep <- floor(nyears_fut/nyears_hist)
#     num.remain <- nyears_fut %% nyears_hist
#     if (num.remain == 0){
#       OH.idx <- c(rep(1:hist.max, num.rep))
#     }else{
#       idx.remain <- hist.year.div[num.remain]
#       OH.idx <- c(rep(1:hist.max, num.rep), 
#                   1:idx.remain)
#     }
#   }
#   return(list(index=OH.idx, end=start_year))
# }

get.OH.time.index <- function(nyears_hist, nyears_fut, hist.year.div, fut.year.div, 
                              start_year=1){
  #'Generates fut_time_len samples from hist_time_len, repeating hist samples 
  #'if fut timeseries is longer, and subsampling hist samples if fut time len is 
  #'shorter.
  #' Used by: delta downscaling, CFQM downscaling, the pr masking functions when
  #' OH is used to downscale
  #Initialize oh_year_div()
  #oh_year_div <- c()
  if (nyears_hist > nyears_fut){
    if (start_year > 1){
      start_idx <- hist.year.div[start_year - 1] + 1
    }else{
      start_idx <- 1
    }
    if (hist.year.div[start_year + nyears_fut - 1] <= hist.year.div[length(hist.year.div)]){
      #Net next nyears samples
      OH.idx <- start_idx:hist.year.div[nyears_fut + start_year - 1]
      oh_year_div <- hist.year.div[1:(nyears_fut + start_year - 1)]
    }else{
      #Get last n samples from end of OH; cycle around to beginning
      year_diff = nyears_hist - start_year
      OH.idx <- c(start_idx:hist.year.div[length(hist.year.div)], 
                  hist.year.div[1]:hist.year.div[year_diff - 1])
      oh_year_div <- hist.year.div[1:(nyears_fut + start_year - 1)]
    }
    start_year <- ((start_year  + nyears_fut) %% nyears_hist)
  }else if ( !(hist.year.div[1] == 0 & start_year %in% c(0,1) ) ){
    #If you are repeating OH samples to make up MF
    #....this should be based off of YEARS, not simple indices
    #And you are going to be working on these as a function of the 
    hist.max <- hist.year.div[length(hist.year.div)]
    start_idx <- fut.year.div[start_year]
    start.vec <- start_idx:hist.max
    num.rep <- floor(nyears_fut/nyears_hist)
    num.remain <- nyears_fut %% nyears_hist
    if (num.remain == 0){
      OH.idx <- c(rep(1:hist.max, num.rep))
      oh_year_div <- as.vector(t(sapply(hist.year.div, 
                                      function(x){sapply(1:(num.rep+1), 
                                                         function(y){x +(y-1) * hist.max}
                                      )}
      )))
      oh_year_div <- oh_year_div[1:length(fut.year.div)]
    }else{
      idx.remain <- hist.year.div[num.remain]
      OH.idx <- c(rep(1:hist.max, num.rep), 
                  1:idx.remain)
      oh_year_div <- as.vector(t(sapply(hist.year.div, 
                                        function(x){sapply(1:(num.rep+1), 
                                                           function(y){x +(y-1) * hist.max}
                                        )})))
      oh_year_div <- oh_year_div[1:length(fut.year.div)]
    }
  }else{
    #This is a case for synthetic data only - assumed that your year divs are the same
    OH.idx = 1:fut.year.div
    start_year = 1
    oh_year_div = 0
  }  
  return(list(index=OH.idx, end=start_year, year_div=oh_year_div))
}
