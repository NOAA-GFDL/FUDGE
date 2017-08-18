#!/bin/Rscript
#'pairwise_pr_adjust_s5.R
#'post-processing steps for adjusted trace pr
#'

adjust_pr_s5 <- function(test, input, adjusted.output){
  arg.names <- names(test$qc_args)
  if(!('thold'%in%arg.names) || !('conserve'%in%arg.names)){
    stop("Error in PR post-processing: One or more of thold or conserve not present in arguments to function")
  }
  OH <- input$hist.targ
  MH <- input$hist.pred
  MF <- input$fut.pred
  DS <- adjusted.output$ds.out
  #save(file='/home/cew/Code/i_testing/parm_pr/i_run/pre5.R', list=c('OH', 'MH', 'MF', 'DS'))
  ignore0 <- test$qc_args$ignore0 == 'on'
  nrips <- dim(adjusted.output$ds.out)[4]
  if(ignore0){
    #These steps weren't there before. These need to get added back in as well. 
    OH[[1]][test$qc_args$OH] <- 0
    MH[[prime_predictor]][test$qc_args$MH] <- 0
    print(dim(test$qc_args$MF))
    print(dim(MF[[prime_predictor]]))
    #stop('examine this out')
    MF[[prime_predictor]][test$qc_args$MF] <- 0
    #Need to add wetdays back into downscaled data
    if (!ds.ref$downscale_oh){
      #non-OH downscaling is straightforward:
      print(dim(DS))
#      print(summary(as.vector(DS)))
      DS[test$qc_args$MF] <- 0
#       print(summary(as.vector(DS)))
#       stop('examine this out')
    }else{
      #Oh downscaling is more complicated, especially when leapdays get involved
      oh.timelen <- length(OH)
      mf.timelen <- length(pr_args$mf.mask)
      nrips <- dim(adjusted.output$ds.out)[4]
      start <- 1
      for(n in 1:nrips){
        if (oh_timelen >= mf_timelen){
          #If more samples in OH, index by nyears present
          idx <- get.OH.time.index(tmasks$nyears_hist, tmasks$nyears_fut, 
                                   tmasks$hist.year.div, tmasks$fut.year.div, 
                                   start_year=start)
          OH.idx <- idx$index
          start <- idx$end
        }else{
          #If more samples in MF, repeat mask until it matches length of MF sample;
          #repeat for each ensemble
          idx <- get.OH.time.index(tmasks$nyears_hist, tmasks$nyears_fut, 
                                   tmasks$hist.year.div, tmasks$fut.year.div, 
                                   start_year=1)
          OH.idx <- idx$index         
        }
        #After either step is complete, index the mask to the future timseries
        #using index.delta
        loop.ds.prmask <- index.delta(tmasks$hist.leapmask[OH.idx], 
                                      tmasks$fut.leapmask, 
                                      rep(1, oh.timelen), 
                                      rep(1, mf.timelen), 
                                      obs.prmask[,,,n,,])
        adjusted.output$ds.out[,,,n,,][!loop.ds.prmask] <- 0
      }
    }
  }else{
    print('Not applying pr mask. Output may have more wetdays than expected.')
  }
  #Once ignore0 operations are complete, apply trace/conserve settings to DS output
#   print(dim(DS))
# print(summary(as.vector(DS)))
# print(summary(as.vector(test$qc_args$MF)))

#stop(paste('examine this out'))
# if (any(is.na(DS))) {
#   print(summary(as.vector(adjusted.output$ds.out)))
#   print(summary(as.vector(input$fut.pred[[1]])))
#   print(length(DS))
#   print(length(test$qc_args$MF))
#   print(summary(as.vector(test$qc_args$MF)))
#   stop('PR mask and data should fit together evenly!')
# }
  conserve <- test$qc_args$conserve == 'on'
# print(summary(input))
# print((input$data.atts))
# stop('examine this output')
  pr_trace <- get_pr_trace(test$qc_args$thold,  input$data.atts$pr_oh_units)
  for (n in 1:nrips){
    ds.trace.idx <- DS[,,,n,,] < pr_trace
    if (conserve){
      DS[,,,n,,] <- conserve_pr(DS[,,,n,,], ds.trace.idx)
    }else{
      DS[,,,n,,][ds.trace.idx] <- 0
    }
  }
#save(file='/home/cew/Code/i_testing/parm_pr/i_run/post5.R', list=c('OH', 'MH', 'MF', 'DS'))
  input$hist.targ <- OH
  input$hist.pred <- MH
  input$fut.pred <- MF
  adjusted.output$ds.out <- DS
  return(list('input' = input, 'adjusted.output' = adjusted.output))
}

adjust_pr_s5_loop <- function(OH, MH, MF){
  #'Does the neccessary looping by variable for 
  #'downscaled output
  
}