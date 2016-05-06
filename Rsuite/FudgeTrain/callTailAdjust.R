callTailAdjust <- function(test, input, adjusted.output){
  #'Calls a BCSD-style adjustment on quantile-mapped code
  #'based off of the limits to the adjustment of the 
  #'MH and MF datasets
  rips <- unique(lapply(input$data.atts$fut.pred, "[[", 'rip'))
  targ.var <- names(input$data.atts$hist.targ)
  arg.names <- names(test$qc_args)
  if('ignore0'%in%arg.names){
    #Hasty adjustment to set missing values to 0 in bcsd correction
    ignore0 <- (test$qc_args$ignore0=='on')
  }else{
    ignore0 <-  FALSE
  }
  if(!('lastNpts')%in%arg.names){
    delta_sample <- 1
  }else if(test$qc_args$lastNpts==0){
    delta_sample = 1
  }else{
    delta_sample = test$qc_args$lastNpts
  }
  triggertypes <- c('GfGh', 'OhGh')
  if(!('triggertype')%in%arg.names){
    stop(paste("Error in callTailAdjust: triggertype must be specified!"))
  }else if(!test$qc_args$triggertype%in%triggertypes){
    stop(paste("Error in callTailAdjust: triggertype must be one of", 
               paste(triggertypes, collapse=",")))
  }else{
    triggertype = test$qc_args$triggertype
  }
  for(rip in rips){
    ds.var <- paste0(targ.var, ".", rip)
    fut.pred <- ds.var
    hist.pred <- names(input$hist.pred)[lapply(input$data.atts$hist.pred, 
                                               "[[", 'var')==targ.var]
    ds.correct <- tail.correct(model.hist = input$hist.pred[[hist.pred]], 
                               obs.hist = input$hist.targ, 
                               model.fut = input$fut.pred[[fut.pred]], 
                               obs.fut = adjusted.output$ds.out[[ds.var]], 
                               args = list(delta_sample = delta_sample, 
                                           triggertype = triggertype))
    if(test$qc.mask=='on'){
      qc.tmp <- ifelse(mapply(both.na, ds.correct, as.numeric(adjusted.output$ds.out[[ds.var]])), 
                       1, 0)
      non.missing <- which(qc.tmp==0)
      ds.tmp <- ifelse(ds.correct==as.numeric(adjusted.output$ds.out[[ds.var]]),
                       1, NA)
      qc.tmp[non.missing] <- ds.tmp[non.missing]
      adjusted.output$qc.mask[[ds.var]] <- qc.tmp
    }
    if(test$adjust.out=='on'){
      adjusted.output$ds.out[[ds.var]] <- as.numeric(ds.correct)
    }
  }
  return(list('input' = input, 'adjusted.output' = adjusted.output))
}

tail.correct <-  function(model.hist, obs.hist,  model.fut, obs.fut, args){
  #Fast wrapper for the more complicated tail_cutoff function
  hitail = tail_cutoff(model.hist, obs.hist, model.fut, obs.fut, c(args, tail='upper'))
  lotail = tail_cutoff(model.hist, obs.hist, model.fut, obs.fut, c(args, tail='lower'))
  cond.data = switch(args$triggertype, 
         'GfGh' = model.fut, 
         'OhGh' = obs.fut)
  obs.fut[hitail$cond] = cond.data[hitail$cond] - hitail$offset
  obs.fut[lotail$cond] = cond.data[lotail$cond] - lotail$offset
  return(obs.fut)
}

tail_cutoff <- function(MH, OH, MF, DS, args){
  #'Function for switching from one tail adjustment to another
  #'Currently accepts the arguments 'GfGh' and 'OhGh'
  select.cond = switch(args$tail, 
                       lower = '>=', 
                       upper = '<=')
  adjust.cond = switch(args$tail, 
                       lower='<',
                       upper='>')
  if(args$triggertype=='GfGh'){
    mf.rank = rank(MF)
    MF  = sort(MF)
    DS = sort(DS)
    #mf.idx = tail(which(MF <= max(MH)), 1)
    mf.idx = tail(which(do.call(select.cond, list(MF, max(MH)))), 1)
    mf.tmp = MF[mf.idx]
    ##get delta
    delta.idx = (mf.idx-delta_sample-1):mf.idx
    delta = mean(MF[delta.idx] - DS[delta.idx])
    ##get selection
    mf.cond = do.call(adjust.cond, list(MF[mf.rank], mf.tmp))
    #DS[MF > mf.tmp] = MF[MF > mf.tmp] - delta
    return(list(cond=mf.cond, offset=delta))
  }else if(args$triggertype=='OhMh'){
    if (max(OH) <= max(MH)){
      oh.prob = ecdf(x=OH)(max(MH))
      #Assume that the sort is going to be the same for both datasetes
      ds.val  = quantile(DS, probs=OH)
      mf.val = quantile(MF, probs=OH)
      ds.cond <- do.call(adjust.cond, list(DS, ds.val))
      #Calculate delta
      MF = sort(MF)
      DS = sort(DS)
      #mf.idx = tail(which(MF <= mf.val),1)
      mf.idx = tail(which(do.call(select.cond, list(MF, mf.val))), 1)
      delta.idx = (mf.idx-delta_sample-1):mf.idx
      delta = mean(MF[delta.idx] - DS[delta.idx])
      #DS[DS > ds.val] = DS[DS > ds.val] - delta
      return(list(cond=ds.cond, offset=delta))
    }else{
      print('no adjust')
      cond=rep(FALSE, length(DS))
      offset=0
      return(list(cond=cond, offset=offset))
    }
  }
}