cdft_tail_cutoff <- function(MH, OH, MF, DS, args){
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
  print(paste("cutoff point:", cutoff))
  #Scooping out the OhGh conditional
  if (do.call(select.cond, list(do.call(trigger.fxn, list(DS)), cutoff)) && !is.na(cutoff)){
    #Assume that the sort is going to be the same for both datasetes
    ds.cond = do.call(select.cond, list(DS, cutoff))
    ds.order = order(DS)
    DS = sort(DS)
    MF = MF[ds.order]
    #print(which(do.call(select.cond, list(DS, cutoff))))
    ds.indices <- which(do.call(select.cond, list(DS, cutoff)))
    if (args$tail=='upper'){
      ds.idx <- ds.indices[1] - 1
    }else if (args$tail=='lower'){
      ds.idx <- ds.indices[length(ds.indices)] + 1
    }
    #     print(ds.idx)
    #get delta
    delta.idx = seq(from=ds.idx, by=delta.by, length.out=delta_sample)
    #     print(delta.idx)
    
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
      } else {
        if (args$tail=='upper'){
          opp.idx <- 1
        }else if (args$tail=='lower'){
          opp.idx <- length(DS)
        }
      }
      inside.start <- ds.idx + delta.by*delta_sample
      inside.tails <- seq(inside.start, opp.idx)
      #print(delta.idx)
      #print(inside.tails)
      #CEW added 12-12-16; needed to remove negative/0 subscripts
      if (any(inside.tails < 1)){
        inside.tails <- inside.tails[inside.tails > 0]
        if (length(inside.tails) == 0){
          stop("Error in cdft_tail_cutoff: At least one inside index should be greater than 0!")
        }
      }
      mult.ifzero <- max(DS[inside.tails]/MF[inside.tails], na.rm=TRUE)
      mult.vec <- DS[delta.idx]/MF[delta.idx]
      #CEW added 12-12-16; mult.vec can, in fact, be NA sometimes
      print(mult.ifzero)
      print(mult.vec)
      if (length(mult.vec) == 0){
        if (length(mult.ifzero) > 0){
        mult.vec <- mult.ifzero
        }else{
          print('Some calculated multiples were NA; returning 1')
          mult.vec <- 1
          mult.ifzero <- 1
        }
      }
      if (is.na(mult.vec)){
        mult.vec <- 9999
      }
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
