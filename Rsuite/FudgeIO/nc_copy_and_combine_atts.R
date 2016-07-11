nc.copy.and.combine.atts <- function(f.src.array, v.src.all,
                                     f.dest, v.dest, exception.list = NULL, 
                                     rename.mapping = NULL, definemode = FALSE, 
                                     verbose=FALSE) {
  #'Modfied from R netcdf4.helpers function nc.copy.atts
  #'Carolyn Whitlock, November 2015
  att.struct <- lapply(f.src.array, ncdf4::ncatt_get, v.src.all)
  att.all <- unlist(lapply(att.struct, 'names'))
  att.all <- att.all[!(att.all %in% exception.list)]
  att.union <- count(att.all)
  att.intersect <- att.union$x[att.union$freq > 1]
  att.union <- att.union$x
  if (length(att.union) > 0) {
    if (!definemode) 
      ncdf4::nc_redef(f.dest)
    clone.fun <- function(x, x.intersect) {
      if(verbose){message(x)}
      if (!(x %in% x.intersect)) {
        att.str <- sapply(att.struct, '[[', x)
        if(length(att.str) > 1){
          att.str <- unlist(att.str)
        }
      }else{
        if(verbose){message('in intersect case; reducing')}
        att.select <- unlist(sapply(att.struct, '[[', x))
        att.str <- paste(unique(att.select), sep=";")
      }
      if(verbose)(print(att.str))
      ncdf4::ncatt_put(f.dest, v.dest, x, att.str, 
                       definemode = TRUE)
    }
    lapply(as.vector(att.union), clone.fun, att.intersect)
    if (!definemode) 
      ncdf4::nc_enddef(f.dest)
  }
}
