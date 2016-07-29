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
    print(ds.idx)
    #get delta
    delta.idx = seq(from=ds.idx, by=delta.by, length.out=delta_sample)
    print(delta.idx)
    delta = mean(DS[delta.idx] - MF[delta.idx]) #previously MF - DS
    return(list(cond=ds.cond, offset=delta))
  }else{
    print('no adjust')
    cond=rep(FALSE, length(DS))
    offset=0
    return(list(cond=cond, offset=offset))
  }
}