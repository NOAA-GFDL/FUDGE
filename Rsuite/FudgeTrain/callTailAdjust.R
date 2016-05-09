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
                               ds.fut = adjusted.output$ds.out[[ds.var]], 
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

tail.correct <-  function(model.hist, obs.hist,  model.fut, ds.fut, args){
  #Fast wrapper for the more complicated tail_cutoff function
  hitail = tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='upper'))
  lotail = tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='lower'))
  cond.data = switch(args$triggertype, 
         'GfGh' = model.fut, 
         'OhGh' = ds.fut)
  ds.fut[hitail$cond] = cond.data[hitail$cond] - hitail$offset
  ds.fut[lotail$cond] = cond.data[lotail$cond] - lotail$offset
  return(ds.fut)
}

tail_cutoff <- function(MH, OH, MF, DS, args){
  #'Function for switching from one tail adjustment to another
  #'Currently accepts the arguments 'GfGh' and 'OhGh'
  trigger.fxn = switch(args$tail, 
                         lower='min',
                         upper='max')
  select.cond = switch(args$tail, 
                       lower = '>=', 
                       upper = '<=')
  adjust.cond = switch(args$tail, 
                       lower='<',
                       upper='>')
  delta_sample = args$delta_sample
  delta.by = switch(args$tail, 
                    lower=(-1), 
                    upper=(1))
  if(args$triggertype=='GfGh'){
    mf.rank = rank(MF)
    MF  = sort(MF)
    DS = sort(DS)
    #mf.idx = tail(which(MF <= max(MH)), 1)
    mf.idx = tail(which(do.call(select.cond, list(MF, do.call(trigger.fxn, list(MH))))), 1)
    if(length(mf.idx)==1){
      mf.tmp = MF[mf.idx]
      ##get delta
      #delta.idx = (mf.idx-delta_sample):mf.idx
      #print(mf.idx)
      #print(summary(MF))
      #print(summary(MH))
      #print(which(do.call(select.cond, list(MF, do.call(trigger.fxn, MH)))))
      delta.idx = seq(from=mf.idx, by=delta.by, length.out=delta_sample)
      print(delta.idx)
      delta = mean(MF[delta.idx] - DS[delta.idx])
      ##get selection
      mf.cond = do.call(adjust.cond, list(MF[mf.rank], mf.tmp))
      print("condition sum:")
      print(sum(mf.cond))
      #stop('examine output')
      #DS[MF > mf.tmp] = MF[MF > mf.tmp] - delta
      return(list(cond=mf.cond, offset=delta))
    }else{
      print("No adjust")
      cond=rep(FALSE, length(DS))
      offset=0
      return(list(cond=cond, offset=offset))
    }
  }else if(args$triggertype=='OhGh'){
    if (do.call(trigger.fxn, OH) <= do.call(trigger.fxn, MH)){
      oh.prob = ecdf(x=OH)(do.call(trigger.fxn, MH))
      #Assume that the sort is going to be the same for both datasetes
      ds.val  = quantile(DS, probs=oh.prob)
      mf.val = quantile(MF, probs=oh.prob)
      ds.cond <- do.call(adjust.cond, list(DS, ds.val))
      #Calculate delta
      MF = sort(MF)
      DS = sort(DS)
      #mf.idx = tail(which(MF <= mf.val),1)
      mf.idx = tail(which(do.call(select.cond, list(MF, mf.val))), 1)
      #delta.idx = (mf.idx-delta_sample):mf.idx
      delta.idx = seq(from=mf.idx, by=delta.by, length.out=delta_sample)
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