limited_tail_cutoff <- function(MH, OH, MF, DS, args){
  #'Section 5 adjustment method for adjusting a specific # of points on the
  #'tail of the distribution. 
  #'
  #Edited 9-30-2016 to add two new arguments: 
  #trigger, which specifies an alternative trigger for the tail adjust
  #len_tail, which specifies how many points the trigger affects
  #Edited  2018-04-27 to remove CDFt-specific checks; other edits took place
  #in January of the same year
  if (args$tail == 'lower'){
    trigger.fxn <- 'min'
    select.cond <- '<='
    adjust.cond <- '<'
    delta.by <- 1
  }else if (args$tail == 'upper'){
    trigger.fxn <- 'max'
    select.cond <- '>='
    adjust.cond <- '>'
    delta.by <- (-1)
  }
  delta_sample = args$delta_sample
  if (args$trigger != 'all'){
    stop(paste("Error in limited_tail_cutoff: as of fudgetag",
               "icing-testing-20180219, arguments for trigger other than 'all' are not", 
               "supported in the cdft limited tail cutoff!"))
  }
  if (sum(is.na(DS) != sum(is.na(MF)))){
    print(sum(is.na(DS)))
    print(sum(is.na(MF)))
    stop('missing values should be the same count')
  }
  if (length(DS) != length(MF)){
    print(length(DS))
    print(length(MF))
    stop('length should be the same length')
  }
  ds.rank <- rank(DS, ties='first')  #used to get sorted output in same order as unsorted ds
  DS = sort(DS)
  ds.order = order(DS)
  MF = MF[ds.order]
  mf.points <- args$tail_len:length(DS)-args$tail_len
  
  
  apply_adjust = T
  #Check minsamplesize based on tail length
  minsample <- max(c((2*args$tail_len + 1), (args$tail_len + args$delta_sample + 1)))
  if (length(DS) < minsample){
    print('Not enough points for required tail length; no tail adjustment')
    apply_adjust <- F
  }
  if (length(DS) > 1 && apply_adjust){
    #Need at least 2 points to adjust
      if (args$tail=='lower'){
        ds.cond <- c(rep(T, args$tail_len), rep(F, length(DS)-args$tail_len))
        ds.cond <- ds.cond[ds.rank]
        ds.idx <- args$tail_len + 1
        args$low_cutoff <- DS[ds.idx]
      }else if (args$tail=='upper'){
        ds.cond <- c(rep(F, length(DS)-args$tail_len), rep(T, args$tail_len))
        ds.cond <- ds.cond[ds.rank]
        ds.idx <- length(DS) - args$tail_len
        args$high_cutoff <- DS[ds.idx]
      }
  }else{
    print("Insufficient data to adjust")
    apply_adjust <- F
  }
  # Now, determine from more complex trigger whether still applies
  if (apply_adjust){
    delta.idx = seq(from=ds.idx, by=delta.by, length.out=delta_sample)
    if (args$adjmethod == 'add'){
      offset = mean(DS[delta.idx] - MF[delta.idx])
    } else if (args$adjmethod == 'mult') {
      if (args$tail=='upper') {
        inside.tail <- seq(min(delta.idx), min(mf.points))
      } else if (args$tail=='lower') {
        inside.tail <- seq(max(delta.idx),  max(mf.points))
      }
      if (any(inside.tail < 1)){
        inside.tail <- inside.tail[inside.tail > 0]
        if (length(inside.tail) == 0){
          stop("Error in limited_tail_cutoff: At least one inside index should be greater than 0!")
        }
      }
      mult.ifzero <- max(DS[inside.tail]/MF[inside.tail], na.rm=TRUE)
      mult.vec <- DS[delta.idx]/MF[delta.idx]
      if (any(mult.vec > mult.ifzero)) {
        print("Some calculated multipliers were too large, replacing with max inside tails.")
        mult.vec[mult.vec > mult.ifzero] <- mult.ifzero
      }
      offset <- mean(mult.vec)
    } else {
      stop("Something has gone terribly wrong with supplied ADJMETHOD!")
    }
    return(list(cond=ds.cond, offset=offset))
  }else{
    print('no adjust')
    cond = rep(FALSE, length(DS))
    offset = 0
    return(list(cond=cond, offset=offset))
  }
}

find_delta_adj <- function(select.cond, DS, MF, cutoff, args){
  #Finds the points that need adjustment on the high tail using the 
  #method that the cdftmod tail adjust originally used.
  #Assume that the sort is going to be the same for both datasetes
  
  ds.indices <- which(do.call(select.cond, list(DS, cutoff)))
  if (args$tail=='upper'){
    ds.idx <- ds.indices[1] - 1
  }else if (args$tail=='lower'){
    ds.idx <- ds.indices[length(ds.indices)] + 1
  }
  return(ds.idx)
}

