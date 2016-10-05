profileFUDGERun <- function(){
  #Prins basic profiling information for the 
  #minifile run, along with statistics about the 
  #number of points downscaled over
  message(paste0("profile_npts_OH:", sum(sapply(OH$data, 'length'))))
  message(paste0("profile_npts_MH:", sum(sapply(MH$data, 'length'))))
  message(paste0("profile_npts_MF:", sum(sapply(MF$data, 'length'))))  
  #message(paste0("profile_npts_OH:", sum(sapply(list.target$clim.in, 'length'))))
  #message(paste0("profile_npts_MH:", sum(sapply(list.hist$clim.in, 'length'))))
  #message(paste0("profile_npts_MF:", sum(sapply(list.fut$clim.in, 'length'))))  

  message(paste0("profile_timetorun:", proc.time()[3] - fudge.start.time))
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
  #message(paste("profile_memavail:"))
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
