limited_tail_cutoff <- function(MH, OH, MF, DS, args){
  #'Section 5 adjustment method for adjusting a specific # of points on the
  #'tail of the distribution. 
  #'
  #Edited 9-30-2016 to add two new arguments: 
  #trigger, which specifies an alternative trigger for the tail adjust
  #len_tail, which specifies how many points the trigger affects
  #Edited  2018-04-27 to remove CDFt-specific checks; other edits took place
  #in January of the same year
  trigger.fxn = switch(args$tail,
                       lower='min',
                       upper='max')
  select.cond = switch(args$tail,
                       lower = '<=',
                       upper = '>=')
  adjust.cond = switch(args$tail,
                       lower='<',
                       upper='>')
  delta_sample = args$delta_sample #lastNpts
  delta.by = switch(args$tail,
                    lower=(1),
                    upper=(-1))
  cutoff = switch(args$tail,
                  lower=args$low_cutoff,
                  upper=args$high_cutoff)
  #Initialize and sort data
  if (args$trigger != 'all'){
    stop(paste("Error in limited_tail_cutoff: as of fudgetag",
               "icing-testing-20180219, arguments for trigger other than 'all' are not", 
               "supported in the cdft limited tail cutoff!"))
  }
  ds.cond = do.call(select.cond, list(DS, cutoff))
  ds.order = order(DS) #used to sort from lowest to highest
  ds.rank <- rank(DS, ties='first')  #used to get sorted output in same order as unsorted ds
  #note: earlier versions did not include ties argument, lead to some changed answers
  DS = sort(DS)
  MF = MF[ds.order]
  apply_adjust = T
  #Determine if any points will need adjustment
  minsample <- max(c((2*args$tail_len + 1), (args$tail_len + args$lastNpts + 1)))
  if (length(DS) < minsample){
    print('Not enough points for required tail length; no tail adjustment')
    apply_adjust <- F
  }
  if (length(DS) > 1 && apply_adjust){
    #Need at least 2 points to adjust
    if (args$trigger=='all'){
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
    }
  }else{
    print("Insufficient data to adjust")
    apply_adjust <- F
  }
  # Now, determine from more complex trigger whether still applies
  if (apply_adjust){
    delta.idx = seq(from=ds.idx, by=delta.by, length.out=delta_sample)
    if (args$adjmethod == 'add'){
      offset = mean(DS[delta.idx] - MF[delta.idx]) #previously MF - DS
    } else if (args$adjmethod == 'mult') {
      ## Check for division by zero or very small number, replace with "largest seen" ratio
      opp.tail.cut = switch(args$tail,
                            upper=args$low_cutoff,
                            lower=args$high_cutoff)
      opp.trigger.fxn = switch(args$tail,
                               upper='min',
                               lower='max')
      opp.cond = switch(args$tail,
                        upper = '<=',
                        lower = '>=')
      if (do.call(opp.cond, list(do.call(opp.trigger.fxn, list(DS)), opp.tail.cut)) && !is.na(opp.tail.cut)) {
        opp.indices <- which(do.call(opp.cond, list(DS, opp.tail.cut)))
        if (args$tail=='upper'){
          opp.idx <- opp.indices[length(opp.indices)] + 1
        }else if (args$tail=='lower'){
          opp.idx <- opp.indices[1] - 1
        }
        inside.start <- ds.idx + delta.by*delta_sample
        inside.tails <- seq(inside.start, opp.idx)
        mult.ifzero <- max(DS[inside.tails]/MF[inside.tails], na.rm=TRUE)
        mult.vec <- DS[delta.idx]/MF[delta.idx]
        ##Adding missval removal 11-1-2016 - 
        ##not sure why NaN are present in the first place, but they 
        ##are being generated now
        mult.vec <- mult.vec[!is.na(mult.vec)]
        if (any(mult.vec > mult.ifzero)) {
          print("Some calculated multipliers were too large, replacing with max inside tails.")
          mult.vec[mult.vec > mult.ifzero] <- mult.ifzero
        }
        offset <- mean(mult.vec)
      } else {
        if (args$tail=='upper'){
          opp.idx <- 1
        }else if (args$tail=='lower'){
          opp.idx <- length(DS)
        }
      }
      inside.start <- ds.idx + delta.by*delta_sample
      inside.tails <- seq(inside.start, opp.idx)
      mult.ifzero <- max(DS[inside.tails]/MF[inside.tails], na.rm=TRUE)
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

