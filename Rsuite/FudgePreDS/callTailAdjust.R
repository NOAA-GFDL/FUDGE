callTailAdjust <- function(test, input, adjusted.output){
  #'Calls a BCSD-style adjustment on quantile-mapped code
  #'based off of the limits to the adjustment of the 
  #'MH and MF datasets
  targ.var <- names(adjusted.output$hist.targ)
  arg.names <- names(test$qc_args)
  nrips <- dim(adjusted.output$ds.out)[4]
  if (adjusted.output$qc.mask=='on'){
    adjusted.output$qc.mask <- list(array(NA, dim=dim(adjusted.output$ds.out)))
  }
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
  triggertypes <- c('GfGh', 'cdft') # 'OhGh'
  if(!('triggertype')%in%arg.names){
    stop(paste("Error in callTailAdjust: triggertype must be specified!"))
  }else if(!test$qc_args$triggertype%in%triggertypes){
    stop(paste("Error in callTailAdjust: triggertype must be one of", 
               paste(triggertypes, collapse=",")))
  }else{
    triggertype = test$qc_args$triggertype
  }
    arg_list = list(delta_sample = delta_sample, 
                    triggertype = triggertype)
  MH <- adjusted.output$hist.pred[[targ.var]]
  OH <- adjusted.output$hist.targ[[targ.var]]
  for(n in 1:nrips){
    MF.slice <- input$fut.pred[[targ.var]][,,,n,,]
    DS.slice <- adjusted.output$ds.out[,,,n,,]
    #Check for attributes
    if(triggertype=='cdft'){
      if(!is.null(attr(adjusted.output$ds.out[[ds.var]], 'high_cutoff'))){
        high_cutoff=attr(adjusted.output$ds.out[[ds.var]], 'high_cutoff')
        low_cutoff = attr(adjusted.output$ds.out[[ds.var]], 'low_cutoff')
        arg_list = c(arg_list, 
                     list(high_cutoff=high_cutoff, low_cutoff=low_cutoff))
        print(arg_list)
        stop('examine output')
      }else{
        stop(paste("Error in callTailAdjust: triggertype of cdft", 
                   "specified, but high_cutoff attribute not set in ds data!"))
      }
    }
    ds.correct <- tail.correct(model.hist = MH, 
                               obs.hist = OH, 
                               model.fut = MF.slice, 
                               ds.fut = adjusted.output$ds.out[[ds.var]], 
                               args = arg_list)
    if(test$qc.mask=='on'){
      ds.correct$mask[ds.correct$mask==0] <- NA
      adjusted.output$qc.mask[,,,n,,] <- ds.correct$mask
    }
    if(test$adjust.out=='on'){
      adjusted.output$ds.out[,,,n,,] <- as.numeric(ds.correct$ds)
    }
  }
  print(summary(adjusted.output))
  return(list('input' = input, 'adjusted.output' = adjusted.output))
}

tail.correct <-  function(model.hist, obs.hist,  model.fut, ds.fut, args){
  #Fast wrapper for the more complicated tail_cutoff function
  cond.data = switch(args$triggertype, 
                     'GfGh' = model.fut, 
                     'OhGh' = ds.fut, 
                     'cdft' = model.fut)
  cond.missvals <- !is.na(cond.data)
  ds.orig <- ds.fut
  model.hist <- remove.missvals(unlist(model.hist))
  obs.hist <- remove.missvals(unlist(obs.hist))
  model.fut <- remove.missvals(unlist(model.fut))
   ds.fut <- remove.missvals(unlist(ds.fut))
#   print(length(model.hist))
#   print(length(obs.hist))
#   print(length(model.fut))
#   print(length(ds.fut))
  if(any(sapply(list(ds.fut, model.hist, obs.hist), length)==0)){
    #Bypass for missing data
    message("Insufficient data for tail adjustment")
    tmp.mask=rep(0, length(ds.orig))
    #return(list(ds = ds.orig, mask = rep(NA, length(ds.orig)))
  }else{
    #Calculate high and low tails
    if (args$triggertype=='cdft'){
      print('calling upper')
      hitail = cdft_tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='upper'))
      print('calling lower')
      lotail = cdft_tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='lower'))
    }else{
      print('calling upper')
      hitail = tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='upper'))
      print('calling lower')
      lotail = tail_cutoff(model.hist, obs.hist, model.fut, ds.fut, c(args, tail='lower'))
    }
    #Add missing values of condition back into main
    cond.tmp <- cond.missvals
    cond.tmp[cond.tmp] <- hitail$cond
    hitail$cond <- cond.tmp
    cond.tmp <- cond.missvals
    cond.tmp[cond.tmp] <- (lotail$cond)
    lotail$cond <- cond.tmp
    hitail.pts = hitail$cond >= 1
    lotail.pts = lotail$cond >= 1
    
    ds.orig[hitail.pts] = cond.data[hitail.pts] + hitail$offset
    ds.orig[lotail.pts] = cond.data[lotail.pts] + lotail$offset
    tmp.mask <- (hitail$cond)*2 + lotail$cond
    tmp.mask[tmp.mask > 4] = 4
  }
# print(length(ds.orig))
# print(length(tmp.mask))
return(list(ds = ds.orig, mask = (tmp.mask)))
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
  }
# else if(args$triggertype=='OhGh'){
#     if (do.call(adjust.cond, list(do.call(trigger.fxn, list(OH)), do.call(trigger.fxn, list(MH))))){
#       oh.prob = ecdf(x=OH)(do.call(trigger.fxn, list(MH)))
#       #Assume that the sort is going to be the same for both datasetes
#       ds.val  = quantile(DS, probs=oh.prob)
#       mf.val = quantile(MF, probs=oh.prob)
#       #Calculate delta
#       MF = sort(MF)
#       ds.rank = rank(DS)
#       DS = sort(DS)
#       ds.cond <- do.call(adjust.cond, list(DS[ds.rank], ds.val))
#       #mf.idx = tail(which(MF <= mf.val),1)
#       #mf.idx = tail(which(do.call(select.cond, list(MF, mf.val))), 1)
#       mf.points = do.call(select.cond, list(MF, mf.val))
#       mf.idx = do.call(trigger.fxn, list(which(mf.points)))
#       #get delta
#       delta.idx = seq(from=mf.idx, by=delta.by, length.out=delta_sample)
#       delta = mean(MF[delta.idx] - DS[delta.idx])
#       #DS[DS < ds.val] = DS[DS < ds.val] - delta
#       return(list(cond=ds.cond, offset=delta))
#     }else{
#       print('no adjust')
#       cond=rep(FALSE, length(DS))
#       offset=0
#       return(list(cond=cond, offset=offset))
#     }
#   }
}
