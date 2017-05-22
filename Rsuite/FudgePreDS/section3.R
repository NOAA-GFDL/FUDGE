#'calls3Adjustment.R
#'
#'Initial stab at a pre-processing function called the same way
#'that the post-processing functions are. It's going to be needed
#'for the transforms soon, anyway.
#'IMPORTANT DIFFERENCE: There will never be any masks on these, 
#'or at least not for the first six months. 
#'
#'Creates a mask of the same dimensions as the downscaled data 
#'for which the default behavior is that a 1 means that the data
#'passes the QC check, and a 0 means that the data does not pass the QC
#'check. This behavior can change depending upon the QC function in question,
#'but this is the general behavior to keep in mind.
#'
#'@param s3.instructions: A list of commands controlling which adjustment steps
#'are preformed, and in what order, as well as whether ar QC mask is to be calculated
#'at any point. Consists of a list of lists with elements of the form
#'list(type='SBiasCorr', qc.mask='on', adjust.out='off', args=list('na'))
#'@param var: The variable being downscaled. 
#'------Parameters required for SBiasCorr-------
#'#'@param data: The data undergoing a qc check/adjustment steps
#'@param MH: The historic predictor(s) of a downscaling run
#'@param OH: The historic target of a downscaling fun
#'@param MF: The future predcictor(s) of a downscaling run
#'------Parameters related to time windowing----
#'
#'@returns A vector of values for the time series at the individual x,y, point
#'with 0 for all values that did not pass the test and 1 for all values that did.

callS3Adjustment<-function(s3.instructions=list('na'),
                           MH = NA, 
                           OH = NA, 
                           MF = NA, 
                           adjust.metadata=list(),
                           create.qc.mask=FALSE, create.adjust.out=FALSE, 
                           s5.instructions=list('na'), 
                           missval.args = list(), 
                           prime_predictor='none'){
  #Create list of variables that will not change with iteration
  #input<- list('MH' = MH, 'OH' = OH, 'MF' = MF)
  #Define data that will change with iteration
  adjusted.list <- list(input=list('MH' = MH, 'OH' = OH, 'MF' = MF, 
                                   'att.list'=adjust.metadata), 
                        s5.list=s5.instructions, 
                        missval.args=missval.args)
  #print(paste("lenght of section 3 instructions:", length(s3.instructions)))
  for(element in 1:length(s3.instructions)){
    test <- s3.instructions[[element]]
    #Note that BOTH ELEMENTS get returned for the adjusted output. 
    #The transforms may have elements that will depend on the conditions of the initial transform, 
    #and the order of the backtransform is going to be dependant on the order of the elements
    #that go into it. 
    print(test$type)
    adjusted.list <- switch(test$type,
                            'PR' = callPRPreproc(test, adjusted.list$input, adjusted.list$s5.list, 
                                                 adjusted.list$missval.args, prime_predictor),
                            'transform' = callPRTransform(test, adjusted.list$input, adjusted.list$s5.list, 
                                                          adjusted.list$missval.args),
                            stop(paste('Adjustment Method Error: method', test$type, 
                                       "is not supported for callS3Adjustment. Please check your input.")))
  }
  #print(lapply(adjusted.list, 'summary'))
  #stop('examine this output') 
  return(adjusted.list)
}

callPRPreproc <- function(test, input, postproc.output, 
                          missval.args, prime_predictor){
  #Outputs a set of adjusted input datasets
  #as output by the precipitation adjustment
  #functions
  #Note: you get truly SPECTACULAR fatal errors if one of the units values
  #passed to the function is null.
  ##Revising 4-14 for the 
  #att_table in input$att.list, and is further organized by MH, OH and MF
  
  pr.names <- (names(test$pp.args))
  #Obtain function args
  if('thold'%in%pr.names && 'freqadj'%in%pr.names && 'conserve'%in%pr.names){
    threshold   =  test$pp.args$thold
    lopt.drizzle  = ( test$pp.args$freqadj =='on')
    lopt.conserve = ( test$pp.args$conserve =='on')
  }else{
    stop(paste("Precipitation Pre-Processing Argument Error: one or more of pr_threshold_in, 
               pr_freqadj_in, or pr_conserve_in not present in arguments to precipitation
               pre-processing function."))
  }
  if('ignore0'%in%pr.names){
    apply.0.mask = (test$pp.args$ignore0=='on')
    pr_mod <- postproc.output$propts #Used later to add masks for later application
  }else{
    message("Assuming that an old XML is being used; default behavior in place")
    apply.0.mask=FALSE
  }
  #Select the variables of interest, and put them through the pr pre_processing function
  pr_target <- input$att.list$pr_oh
  nrips <- dim(input$MF[[pr_target]])[4]
  #   for (n in 1:nrips){
  #    MF.slice <- input$MF[[pr_target]][,,n,,,]
  #save(file='/work/cew/pre_adjust_pr.R', list=c('input', 'lopt.drizzle', 'lopt.conserve', 'threshold'))
  #stop('examine this pre output')
#  print(pr_target)
  targ_idx <- pred_idx <- grepl(paste0("^", pr_target, "\\."), names(input$OH))
  pred_idx <- grepl(paste0("^", pr_target, "\\."), names(input$MH))
#   print(pred_idx)
#   print(targ_idx)
  temp.out <- AdjustWetdays(ref.data=input$OH[[prime_predictor]],   ref.units=input$att.list$pr_oh_units, 
                            adjust.data=input$MH[[prime_predictor]],adjust.units=input$att.list$pr_mh_units[[1]],
                            future.data=input$MF[[prime_predictor]],future.units=input$att.list$pr_mf_units[[1]],
                            opt.wetday=threshold, 
                            lopt.drizzle=lopt.drizzle, 
                            lopt.conserve=lopt.conserve, 
                            adj.method=test$pp.args$method,
                            zero.to.na=FALSE) #formerly set by apply.0.mask; used elsewhere now)
  input$OH[[prime_predictor]] <- temp.out$ref$data   #only a single target variable
  input$MH[[prime_predictor]] <- temp.out$adjust$data
  input$MF[[prime_predictor]] <- temp.out$future$data
  if(apply.0.mask){
    tmp.ref <- temp.out$ref$pr_mask
    tmp.adj <- temp.out$adjust$pr_mask
    tmp.ref[tmp.ref==0] <- NA
    tmp.adj[tmp.adj==0] <- NA
    input$OH[[prime_predictor]] <- (input$OH[[prime_predictor]]   * tmp.ref)
    for (el in prime_predictor){ #input$att.list$pr_mh
      input$MH[[el]] <- input$MH[[el]] * tmp.adj
    }
    #This should make sure that when extra missing values are added, all datasets
    #in the predictor datasets will have the missing values added back in
    tmp.fut <- temp.out$future$pr_mask
    tmp.fut[tmp.fut==0] <- NA
    for (el in prime_predictor){ #input$att.list$pr_mf
      input$MF[[el]] <- input$MF[[el]] * tmp.fut
    }
    #Save future pr mask fo application in section 5;
    #note that it is the presence of this mask that 
    #leads to the adjustment being performed.
    if(!(ds.ref$downscale_oh)){
      print(paste('adding future prmask'))
      fut.prmask <- temp.out$future$pr_mask
      pr_mod$qc_args$fut.prmask <- fut.prmask
    }else{
      #Assume that you are downsciang on the OH dataset instead; store accordingly
      print(paste('adding target prmask'))
      pr_mod$qc_args$targ.prmask <-  temp.out$ref$pr_mask
    }
  }
  missval.args$pr_opts <<- list(OH=temp.out$ref$pr_mask, 
                           MH=temp.out$adjust$pr_mask, 
                           MF=temp.out$future$pr_mask, 
                           pr_oh = input$att.list$pr_oh, 
                           pr_mh = input$att.list$pr_mh, 
                           pr_mf = input$att.list$pr_mf, 
                           adjusted.thold = temp.out$adjusted.thold)
  if(apply.0.mask){
    postproc.output$propts <- pr_mod
  }

  return(list('input'=input,
              's5.list'=postproc.output, 
              'missval.args'=missval.args))
}

callPRTransform <- function(test, input, postproc.output, missval.args){
  #Transforms non-gaussian data present in the OH, MH and MF
  #data into a more gaussian form
  
  #Insert check for presence of transfrom in here at a later point
  tform <- test$pp.args$transform
  ds.var <- names(input$OH)
  transform.vars <- test$pp.args$vars
  
  #Do a basic check for the presence of the variables you want to check  
  t.fxn <- return.t.fxn(tform, test$pp.args$arg) 
  #Insert saving of t.fxn in the section 5 list in this bit
  #note that the adjustment gets added to the TOP of the stack
  backtform <- list(type='backtransform',
                    adjust.out='on',
                    qc.mask='off',
                    loc='present', #shouldn't need to be set; 
                    #should be added to the current s5 list and executed
                    #accordingly; check to see whether this needs new entries
                    #in the metadata
                    qc_args=list(fxn=t.fxn[[3]], 
                                 arg=t.fxn[[4]], 
                                 vars=""))
  postproc.output <- c(list(backtform), postproc.output)
  #...you know, for more readable metadata, it might be worth it to do cubrt and 
  #sqrt functions. Since the names wind up in the metadata.
  for (datatype in names(input)){
    if (datatype %in% c("OH", "MH", "MF")){
      for (varname in names(input[[datatype]])){
        #print(varname)
        grep.var <- sapply(transform.vars, 
                           function(x){grepl(paste0("^", x, "\\."), 
                                             varname, perl=T)})
        if ( any(grep.var) ){
          #if var is one of the vars receiving a transform
          if(!is.null(t.fxn[[2]])){
            #If arguments to the transform, use them
            input[[datatype]][[varname]] <- do.call(t.fxn[[1]], 
                                                list(input[[datatype]][[varname]], t.fxn[[2]]))
          }else{
            #Otherwise, call without arguments
            input[[datatype]][[varname]] <- do.call(t.fxn[[1]], 
                                                list(input[[datatype]][[varname]]))
          }
        }
      }
    } 
  }
  #stop('examine the transform out')
  return(list('input'=input,
              's5.list'=postproc.output, 
              'missval.args'=missval.args))
}

return.t.fxn <- function(tform, arg){
  #'Helper function for returning a transform from a transform
  #'arg - now gets used by pr transforms AND MBC
  out <- switch(tform,
                'log' = list('log', NULL, 'exp', NULL), #log base e 
                'log10' = list('log10', NULL, 'exp10', NULL), #log base 10
                'power' = list('^', arg, '^', 1/arg), 
                stop(paste("Error in return.t.fxn: transform", tform, 
                           "was not one of the allowed options!")))
  return(out)
}

exp10 <- function(x){
  #'Convenience function for using a log base 10 transform
  return(10^x)
}
