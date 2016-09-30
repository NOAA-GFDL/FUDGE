#fudge_subset_functions.R
#Functions related to subsetting and looping over data - 
#getting multiple points, padding datasets, and determining
#which coordinates to loop over within the function.

pad.fudge.var.dim <- function(data, pad.dim, pad.by=1, 
                              pad.type='NA', pad.x.loc='start'){
  #Pads a 6-dimensional dataset with more values along one of 
  #x or y depending on args. The pad specification changes
  #how the missing values will be added/padded.
  #Called from the fudge dataset builder.
  #Strong assumption that the 6-dimensional structure is enforced
  #pad.dim can be one of 'x' or 'y'
  new.dim <- dim(data)
  print(new.dim)
  print(pad.by)
  if (pad.dim=='x'){
    x.len <- new.dim[1]
    new.dim[1] <- x.len + pad.by
    if(pad.x.loc=='start'){
      pad.seq = pad.by:x.len
    }else if(pad.x.loc=='end'){
      pad.seq = 1:x.len
    }
  }else if (pad.dim=='y'){
    #y dim always gets iterated over twice
    y.len <- new.dim[2]
    new.dim[2] <- new.dim[2] + (pad.by - 1) # no longer * 2 
  }
  out.data <- array(NA, dim=new.dim)
  if (pad.dim=='x'){
    out.data[pad.seq,,,,,] <- data
  }else if (pad.dim=='y'){
    print(pad.by)
    pad.start <- (pad.by-1)/2 #start counting from here
    #print((((pad.by-1)/2):y.len)+1)
    #print(length((((pad.by-1)/2):y.len)+1))
    #print((((pad.by-1)/2):y.len))
    #print(length((((pad.by-1)/2):y.len)))
    out.data[,(1:114) + (pad.start-1),,,,] <- data
  }
  #stop('examine output')
  #if we ever get to this point, pad.type can be set to the 
  #last index in the dimension that it is padding as well
  remove(data)
  gc()
  return(out.data)
}

get.FUDGE.subset <- function(by, idx, idx_max){
  #Gets a subset of indices suitable for using in the IO functions
  #by: how many points you will be taking in a single slice
  #idx: where you are starting counting from
  #idx_max: max allowable index. 
  #allow_small_subset: whether to make smaller subsets
  #  when idx=1 or idx=idx_max
  ##Now returns extra arguments, to determine what steps need to be made
  ##to pad the input datsets.
  delt <- floor(by/2) #for by=1, this is 0
  start = idx - delt
  end = idx + delt
  #Track whether or not you need to add end-of-grid checks elsewhere
  #Only one of teh olap conditions should ever be true at once.
  olap = F
  olap_by = 0
  olap_loc = 'none'
  if ((start)  < 1){
    olap = T
    olap_by = start * (-1)
    olap_loc = 'start'
    start = 1
  }
  if (end > idx_max){
    if(olap == T){
      stop(paste("Error in get.fudge.subset: subset overlapped start and end", 
                 "of index simultaneously! This should not be possible."))
    }
    olap = T
    olap_by = end - idx_max
    olap_loc = 'end'
    end <- idx_max
  }
  return(list(subset=c(start, end), 
              olap=olap, 
              olap_by=olap_by, 
              olap_loc=olap_loc))
}

get.loop.indices <- function(x.len, y.len, by, by.min=1, 
                             allow_partial=F){
  #Get.loop.indices deals with two cases: 
  #' 1) You are looping by exactly as many points as there are 
  #' in the given dimension. In this case, don't try to loop; 
  #' there is a single start and end index.
  #' 2) You are looping by fewer points than there are in the
  #' given dimension (i.e. looping by 3 in 114 lat indices); in 
  #' that case, the center of your 'by' moves by one each time, leaving
  #' smaller sample sizes at the end of your distribution.
  #' 9-21: Code has been modified; we now pad datasets that have insifficient
  #' indices in x or y with missing values to fill the dataset. Need to return
  #' fewer looping coords as a result.
  if (allow_partial){
    delt <- (by-1)/2
  }else{
    delt <- by
  }
  if (x.len==by){
    x.start <- 1
    x.end <- by
  }else{
    out <- loop.seq(x.len, delt)
    x.start <- out$start; x.end <- out$end
  }
  if(y.len==by){
    y.start <- 1
    y.end <- by
  }else{
    out <- loop.seq(y.len, delt)
    y.start <- out$start; y.end <- out$end
  }
  return(list(x.start = x.start, x.end = x.end, 
              y.start  = y.start, y.end=y.end))
}

loop.seq <- function(len, delt, allow_partial=F){
  #Creates a sequence of start and end values to loop over 
  #delt = (by-1)/2
  ###These two lines get used if we ever turn on downscaling
  ###over incomplete datasets again
  if (allow_partial){
    base.seq <- 1:(len-delt)
    start.seq <- c(rep(1, delt),   base.seq)
    end.seq <- c(base.seq+delt, rep(len, delt))
  }else{
    base.seq <- 1:(len - (delt-1) )
    start.seq <- base.seq
    end.seq <- base.seq + (delt-1)
  }
  return(list(start=start.seq, end=end.seq))
}


get.xy.slice <- function(data, coords, i, j){
  #Given a subset of x and y coords and a list of data
  #either covering those coords (the most common case)
  #or of x and y dimensions 1 (i.e. ENSO, same for all x and y)
  #subsets the data for use in the downscaling steps.
  x1 <- coords$x.start[i]
  x2 <- coords$x.end[i]
  y1 <- coords$y.start[j]
  y2 <- coords$y.end[j]
  #Function for most common case
  subset.xy <- function(el, x1, x2, y1, y2, n){
    if (any( (dim(el)[1:2]) < c(x2,y2) )){
      #Subsetting is not posisble; don't bother on short dims
      message(paste("Warning in subset.xy: var", n, 
                    "had too few indices to subset on x and y;", 
                    "returning var unsubset"))
      return(el)
    }else{
      #Subsetting is posisble (the most common case)
      return(el[x1:x2, y1:y2, ,,, , drop=F])
    }
  }
  return(lapply(data, subset.xy,
                x1, x2, y1, y2, n=names(data)))
}
