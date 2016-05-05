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
  if(!('sample_size')%in%arg.names){
    sample_size <- 1
  }else if(test$qc_args$sample_size==0){
    sample_size = 1
  }else{
    sample_size = test$qc_args$sample_size
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
                               args = test$qc_args)
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
  cond.data = switch(args$method, 
         'GfGh' = model.fut, 
         'OhGh' = obs.fut)
  obs.fut[hitail$cond] = cond.data[hitail$cond] - hitail$offset
  obs.fut[lotail$cond] = cond.data[lotail$cond] - lotail$offset
  #stop('examine output')
  return(obs.fut)
}

tail_cutoff <- function(MH, OH, MF, DS, args){
  #'Function for switching from one tail adjustment to another
  #'Currently accepts the arguments 'BCQM' and 'CDFt'
  sample.fxn <- function(data, tail, sample_size){
    sample_size = sample_size - 1
    s.start <- switch(tail, 
                      'upper'= length(data) - sample_size, 
                      'lower' = 1)
    return(mean(sort(data)[s.start:(s.start+sample_size)], 
                na.rm=T))
  }
  if(args$method=='GfGh'){
    mh.thold <- sample.fxn(MH, args$tail, 1)
    mh.sample <- sample.fxn(MH, args$tail, args$sample_size)
    oh.sample <- sample.fxn(OH, args$tail, args$sample_size)  
    tail.comp = switch(args$tail, 
                       upper=">", 
                       lower="<")
    bcsd.delta <- mh.sample - oh.sample
    mf.cond <- do.call(tail.comp, list(MF, mh.thold))
    return(list(cond=mf.cond, offset=bcsd.delta))
  }else if(args$method=='OhGh'){
    mh.sample <- sample.fxn(MH, args$tail, args$sample_size)
    oh.sample <- sample.fxn(OH, args$tail, args$sample_size)
    mh.thold <- sample.fxn(MH, args$tail, 1)
    tail.comp = switch(args$tail, 
                       upper=">", 
                       lower="<")
    if(do.call(tail.comp, list(oh.sample, mh.sample))){
      oh.prob <- ecdf(x=OH)(mh.thold)
      #Map from quantile space to downscaled space
      ds.condval <- quantile(DS, probs=oh.prob)
      mf.diffval <- quantile(MF, probs=oh.prob)
      cdft.diff <- mf.diffval - ds.condval
      ds.cond <- do.call(tail.comp, list(DS, ds.condval))
      return(list(cond=ds.cond, offset=cdft.diff))
    }else{
      print('no adjust')
      cond=rep(FALSE, length(DS))
      offset=0
      return(list(cond=cond, offset=offset))
    }
  }else{
    stop("Error in tail_cutoff: method should be one of BCQM or CDFt!") 
  }
}