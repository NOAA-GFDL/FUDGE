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
      ds.correct$mask[ds.correct$mask==0] <- NA
      adjusted.output$qc.mask[[ds.var]] <- ds.correct$mask
      
#       qc.tmp <- ifelse(mapply(both.na, ds.correct, as.numeric(adjusted.output$ds.out[[ds.var]])), 
#                        1, 0)
#       non.missing <- which(qc.tmp==0)
#       #Getting what look and act like errors in epsilon
#       #when comparing n=1 with n > 1; 
#       #adding an epsilon check to see if that solves the issue.
#       eps = .Machine['double.eps'][[1]]*2
#       #abs(values_new1 - values2[slope_rank==1]) <= eps
#       ds.tmp <- ifelse(
#         abs(ds.correct - as.numeric(adjusted.output$ds.out[[ds.var]]))<=eps,
#                        1, NA)
#       qc.tmp[non.missing] <- ds.tmp[non.missing]
#       print('Total number of points different:')
#       print(sum((qc.tmp)==1, na.rm=T))
#       adjusted.output$qc.mask[[ds.var]] <- qc.tmp
    }
    if(test$adjust.out=='on'){
      adjusted.output$ds.out[[ds.var]] <- as.numeric(ds.correct$ds)
    }
  }
  return(list('input' = input, 'adjusted.output' = adjusted.output))
}

tail.correct <-  function(model.hist, obs.hist,  model.fut, ds.fut, args){
  #Fast wrapper for the more complicated tail_cutoff function
  cond.data = switch(args$triggertype, 
                     'GfGh' = model.fut, 
                     'OhGh' = ds.fut)
  cond.missvals <- !is.na(cond.data)
  ds.orig <- ds.fut
  model.hist <- remove.missvals(model.hist)
  obs.hist <- remove.missvals(obs.hist)
  model.fut <- remove.missvals(model.fut)
  ds.fut <- remove.missvals(ds.fut)
  #Calculate high and low tails
  print('calling upper')
  hitail = tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='upper'))
  print('calling lower')
  lotail = tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='lower'))
  #Add missing values of condition back into main
  cond.tmp <- cond.missvals
  cond.tmp[cond.tmp] <- hitail$cond * 2
  hitail$cond <- cond.tmp
  cond.tmp <- cond.missvals
  cond.tmp[cond.tmp] <- (lotail$cond)
  lotail$cond <- cond.tmp
#  ds.orig <- ds.fut
#   save(file="/home/cew/Code/testing/test_out.R", list=c('ds.fut', 'hitail', 'cond.data', 'ds.orig', 
#                                                         'model.hist', 'obs.hist', 'model.fut'))
  ds.orig[hitail$cond] = cond.data[hitail$cond] + hitail$offset
  ds.orig[lotail$cond] = cond.data[lotail$cond] + lotail$offset
  #print(sum(cond.tmp))
  #print('Total number of points different at this point:')
  #print(which((ds.fut-ds.orig)==0))
#   xval = length(model.fut)
#   yrange <- range(c(ds.fut, ds.orig, model.fut,model.hist))
#   plot(x=1:length(model.hist), sort(model.hist), col='green')
#   lines(x=1:length(model.hist), sort(obs.hist), col='black')
#   plot(x=1:xval, y=sort(model.fut), col='red', lwd=2, main="MF, DS and DS with tail adjust", ylim=yrange)
#   abline(h=max(model.hist), col='green')
#   abline(h=max(obs.hist), col='black')
#   lines(x=1:xval, y=sort(ds.fut), col='black', lwd=2)
#   lines(x=1:xval, y=sort(remove.missvals(ds.orig)), col="blue",lwd=2, lty=2)
#   for(i in seq(0, 300, 30)){
#     abline(v=i)
#   }
#   legend('bottomright',legend=c('MF','DS','DS /w tailadj'), col=c('red', 'black', 'blue'), lty=c(1,1,2)) 
#   
  # mask = (hitail$cond|lotail$cond)
  tmp.mask <- hitail$cond + lotail$cond
  tmp.mask[tmp.mask > 4] = 4
  #print(tmp.mask)
#   print(hitail$offset)
#   print(lotail$offset)
#   print(sum(tmp.mask))
#   print(sum(ds.fut==ds.orig))
stop("examine output")
  return(list(ds = ds.orig, mask = (hitail$cond + lotail$cond)))
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
                    lower=(1), 
                    upper=(-1))
  if(args$triggertype=='GfGh'){
    mf.rank = rank(MF)
    MF  = sort(MF)
    DS = sort(DS)
    #mf.idx = tail(which(MF <= max(MH)), 1)
    mf.points = do.call(select.cond, list(MF, do.call(trigger.fxn, list(MH))))
#     trigger.cond = do.call(adjust.cond, list(do.call(trigger.fxn, list(MF)), 
#                                              do.call(trigger.fxn, list(MH))))
    mf.overlap <- max(min(MF),min(MH)) <= min(max(MF),max(MH))
    #print(trigger.cond)
    if(any(mf.points==TRUE, na.rm=T)){
        mf.idx = do.call(trigger.fxn, list(which(mf.points)))
        #Check on behavior when Max(MH) == max(MF)
        #do.call(trigger.fxn, list(MH))
        mf.tmp = MF[mf.idx]
        ##get delta
        if(mf.overlap){
          delta.idx = seq(from=mf.idx, by=delta.by, length.out=delta_sample)
          if(any(delta.idx < 1)){
            print(trigger.fxn)
            print("Overlap too small of delta_sample; resizing")
            delta.idx <- delta.idx[delta.idx >= 1]
          }
          delta = mean(DS[delta.idx] - MF[delta.idx])
          #print(paste("Delta:", delta))
          ##get selection
          mf.cond = do.call(adjust.cond, list(MF[mf.rank], mf.tmp))
        }else{
          print("Selecting mean(OH) - mean(MH)")
          delta = mean(OH) - mean(MH)
          print(mf.tmp)
          print(adjust.cond)
          mf.cond <- do.call(select.cond, list(MF[mf.rank], mf.tmp))
          mf.cond <- mf.cond * 4
        }
      #DS[MF > mf.tmp] = MF[MF > mf.tmp] - delta
      return(list(cond=mf.cond, offset=delta))
    }else{
      print("No adjust")
      cond=rep(FALSE, length(MF))
      offset=0
      return(list(cond=cond, offset=offset))
    }
  }else if(args$triggertype=='OhGh'){
    if (do.call(adjust.cond, list(do.call(trigger.fxn, list(OH)), do.call(trigger.fxn, list(MH))))){
      oh.prob = ecdf(x=OH)(do.call(trigger.fxn, list(MH)))
      #Assume that the sort is going to be the same for both datasetes
      ds.val  = quantile(DS, probs=oh.prob)
      mf.val = quantile(MF, probs=oh.prob)
      #Calculate delta
      MF = sort(MF)
      ds.rank = rank(DS)
      DS = sort(DS)
      ds.cond <- do.call(adjust.cond, list(DS[ds.rank], ds.val))
      #mf.idx = tail(which(MF <= mf.val),1)
      #mf.idx = tail(which(do.call(select.cond, list(MF, mf.val))), 1)
      mf.points = do.call(select.cond, list(MF, mf.val))
      mf.idx = do.call(trigger.fxn, list(which(mf.points)))
      #get delta
      delta.idx = seq(from=mf.idx, by=delta.by, length.out=delta_sample)
      delta = mean(MF[delta.idx] - DS[delta.idx])
      #DS[DS < ds.val] = DS[DS < ds.val] - delta
      return(list(cond=ds.cond, offset=delta))
    }else{
      print('no adjust')
      cond=rep(FALSE, length(DS))
      offset=0
      return(list(cond=cond, offset=offset))
    }
  }
}