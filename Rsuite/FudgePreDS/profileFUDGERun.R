profileFUDGERun <- function(){
  #Prins basic profiling information for the 
  #minifile run, along with statistics about the 
  #number of points downscaled over
  message("All timings are reported in seconds")
  message("All memory usage statistics are reported in Mb")
  message(paste0("profile_npts_OH:", sum(sapply(OH$data, 'length'))))
  message(paste0("profile_npts_MH:", sum(sapply(MH$data, 'length'))))
  message(paste0("profile_npts_MF:", sum(sapply(MF$data, 'length'))))  
  message(paste0("profile_npts_DS:", ds_tally))
  message(paste0("profile_n_twindows:", tmasks$nmasks))

  runtime <- proc.time()[3] - fudge.start.time
  message(paste0("profile_timetorun:", round(runtime, digits=3)))
  message(paste0("profile_timeincalc:", round(ds.elapsed, digits=3)))
  message(paste0("profile_timenotincalc:", round(runtime - ds.elapsed, digits=3)))
  gcmem <- gc()
  #message(paste("profile_ncells:", gcmem[1,6]))
  #message(paste("profile_vcells:", gcmem[2,6])
  message(paste0("profile_maxmem:", sum(gcmem[,6])))
  message(paste0("profile_ppnode:", Sys.info()['nodename']))
  out <- system2('free', '-m', stdout=T)
  memlist <- unlist(strsplit(out[2], " +"))
  swaplist <- unlist(strsplit(out[4], " +"))
  message(paste0("profile_memavail:", memlist[2]))
  message(paste0("profile_swapavail:", swaplist[4]))
}

report_gc_memuse_fudge <- function(simplify=F){
  prof_out <- gc()
  memhi <- sum(prof_out[,6])
  if (simplify){
  if(memhi >= 1000){
    #Memory in Gb
    return(paste(round(memhi/1000, digits=2), "Gb"))
  }else{
    #Memory in Mb
    return(paste(round(memhi, digits=2), "Mb"))
  }
  }else{
    return(paste(round(memhi, digits=2)))
  }
}
