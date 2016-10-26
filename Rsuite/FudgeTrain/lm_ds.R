#lm and lmstep calls and main downscaling functions.
#Both functions are designed to work on multivar datasets,
#and handle multiple points by decomposing into separate vars.

callMultiLM <- function(OH, MH, MF, args){
  #Pre-processing step to compress vectors into multivariate form
  OH.data <- compress.xy.data(OH)
  MH.data <- compress.xy.data(MH)
  MF.data <- compress.xy.data(MF)
  #It's not possible to remove missing values at this step due to 
  #multi-ensemble considerations for the MF data.                           
  #Multi ensembles in OH and MH get pooled in the next step\
  lm.out <- multi.lm(OH.data, MH.data, MF.data, args)
  #lm.out has two components: ds and qc
  return(lm.out)
}

callMultiLM_new <- function(OH, MH, MF, args){
  #Pre-processing step to compress vectors into multivariate/multi-point form
  OH.data <- compress.xy.data(OH)
  MH.data <- compress.xy.data(MH)
  MF.data <- compress.xy.data(MF)
  targ.var <- names(OH.data)
  nrips <- dim(MF[[1]])[4]
  #It's not possible to remove missing values at this step due to 
  #multi-ensemble considerations for the MF data.                           
  #Multi ensembles in OH and MH get pooled in the next step
  lm.model <- lm.train(OH.data, MH.data)
  for (n in 1:nrips){
    MF.slice <- lapply(MF, '[', ,,,n,,)
    MF.slice <- lapply(MF.slice, 'as.vector')
    ds.out <- lm.downscale(lm.model, MF.slice)
    notna.index <- which(!is.na(unlist(MF.slice[grepl(targ.var, names(MF.slice))])))
    out.data[,,,n,,][notna.index] <- ds.out
  }
  return(list(ds=out.data, qc=NULL))
}

lm.train <- function(OH, MH){
  #OH and MH are named lists of vectors returned by compress.xy.data
  OH <- lapply(OH, as.vector)
  MH <- lapply(MH, as.vector)
  #Missing values between two datasets already synched from missing value removal function
  test.list <- c(list('target'=OH[[1]]), MH)
  test.prod <- data.frame(test.list)
  lm.model <- lm(data=test.prod, na.action='na.omit') #na.omit='donit include incompletes'
  return(lm.model)
}

lm.downscale <- function(lm.model, MF){
  #MF is a named list of vectors returned from compress.xy.data
  return(suppressWarnings(predict.lm(lm.model, newdata=as.data.frame(MF.slice))))
}



multi.lm <- function(OH, MH, MF, args=NA){
  #New assumption of 6-dimensional data
  targ.var <- names(OH)
  OH <- lapply(OH, as.vector)
  MH <- lapply(MH, as.vector)
  #Synchronize missing values between the two datasets
  #train.out <- remove.ds.missvals('synchronous', 
  #                                OH=OH, MH=MH)
  test.list <- c(list('target'=OH[[1]]), MH)
  #Trying this again, based on the output from the missing value removal
  #function
  #test.list <- c(list('target'=train.out$OH[[1]], train.out$MH))
  test.prod <- data.frame(test.list)
  lm.model <- lm(data=test.prod, na.action='na.omit') #na.omit='donit include incompletes'
  nrips = dim(MF[[1]])[4]
  out.data <- array(NA, dim=dim(MF[[1]]))
  #print('dimensions of out.data:')
  #print(dim(out.data))
  for (n in 1:nrips){
    MF.slice <- lapply(MF, '[', ,,,n,,)
    MF.slice <- lapply(MF.slice, 'as.vector')
    notna.index <- which(!is.na(unlist(MF.slice[grepl(targ.var, #paste0(targ.var,".0.0"), 
                                                      names(MF.slice))])))
    #print(paste("length of notna.index:", length(notna.index)))
    #MF.slice <- lapply(MF.slice, "remove.missvals")
    #print('predicting')
    tmp <- suppressWarnings(predict.lm(lm.model, newdata=as.data.frame(MF.slice)))
#     if (length(tmp) != length(out.data[,,,n,,][notna.index])){
#       print(length(tmp))
#       print(length(out.data[,,,n,,][notna.index]))
#       print(length(tmp[!is.na(tmp)]))
#       stop('examine output')
#     }
    out.data[,,,n,,][notna.index] <- tmp[!is.na(tmp)]
  }
  return(list(ds=out.data))
}

callStep.lm <- function(OH, MH, MF, args){
  OH.data <- compress.xy.data(OH)
  MH.data <- compress.xy.data(MH)
  MF.data <- compress.xy.data(MF)
  #It's not possible to remove missing values at this step due to 
  #multi-ensemble considerations for the MF data.                           
  #Multi ensembles in OH and MH get pooled in the next step 
#   save(file='/home/cew/Code/testing/test_out.R', list=c('OH.data', 'MH.data', "MF.data", 'args'))
#   stop('examine data')
  lm.out <- step.lm(OH.data, MH.data, MF.data, args)
  #lm.out contains two components: ds and qc (list of vectors)
  return(lm.out)
}

callStep.lm_new <- function(OH, MH, MF, args){
  #Pre-processing step to compress vectors into multivariate/multi-point form
  OH.data <- compress.xy.data(OH)
  MH.data <- compress.xy.data(MH)
  MF.data <- compress.xy.data(MF)
  targ.var <- names(OH.data)[1]
  nrips <- dim(MF[[1]])[4]
  #It's not possible to remove missing values at this step due to 
  #multi-ensemble considerations for the MF data.                           
  #Multi ensembles in OH and MH get pooled in the next step
  lm.model <- lmstep.train(OH.data, MH.data, c(args, list(targ.var=targ.var)))
  for (n in 1:nrips){
    MF.slice <- lapply(MF, '[', ,,,n,,)
    MF.slice <- lapply(MF.slice, 'as.vector')
    ds.out <- lm.downscale(lm.model, MF.slice)
    notna.index <- which(!is.na(unlist(MF.slice[grepl(targ.var, names(MF.slice))])))
    out.data[,,,n,,][notna.index] <- ds.out
  }
  return(list(ds=out.data, qc=NULL))
}

lmstep.train <- function(OH,MH,args){
  #'Calls R's lm() function using targ as a function
  #'of linear variability of one or more variables in 
  #'pred, optimizes the model with step, and then uses 
  #'the data in new to predict the future results
  #'Does not currently take any arguments according to XMLGen; 
  #'k can be specified for training purposes.
  if(!is.null(args$k)){
    k.val <- args$k
  }else{
    k.val <- 2
  }
  direc='forward'
  targ.var <- args$targ.var
  test.list <- c(OH, MH)
  test.prod <- data.frame(matrix(unlist(test.list), ncol=length(test.list)))
  names(test.prod) <- c('target', names(MH))
  lm.train.model <- step(lm(as.formula(paste0('target',"~1")),
                            data=test.prod, na.action='na.omit'), #.^2, 
                         scope=list(upper=paste0(targ.var,'~',paste0(names(MH), collapse="+"),sep=""),
                                    lower=paste0(targ.var,'~1')),
                         scale=0, direction=direc, 
                         trace=0,
                         keep=NULL, steps=(length(names(MH))-1), k=k.val)
  return(lm.train.model)
}

lmstep.downscale <- function(lm.train.model, MF, targ.var){
  #'Calls downscaling function on lmstep 
  tmp.out <- suppressWarnings(predict(lm.train.model, newdata=MF.slice))
  if(length(tmp.out)==0){
    warning(paste("Warning in callStep.lm: step eliminated all variables,", 
                  "resulting in output of length 0! Returning the MF data for now"))
    return(MF.slice[grepl(targ.var, names(MF.slice))])
  }else{
    return(tmp.out)
  }
}

# 
# step.lm <- function(OH, MH, MF, args=NA, ds.var='tasmax', att_table = NULL){
#   #'Calls R's lm() function using targ as a function
#   #'of linear variability of one or more variables in 
#   #'pred, optimizes the model with step, and then uses 
#   #'the data in new to predict the future results
#   #'Does not currently take any arguments according to XMLGen; 
#   #'k can be specified for training purposes.
#   if(!is.null(args$k)){
#     k.val <- args$k
#   }else{
#     k.val <- 2
#   }
#   direc='forward'
#   targ.var = names(OH)
#   #Now, create your data frame and make the linear model
# #   target = OH[[1]]
# #   print(dim(target))
# #   test.list <- c(list('target'=OH[1]), MH)
#   targ.var <- names(OH)[1]
#   test.list <- c(OH, MH)
#   test.prod <- data.frame(matrix(unlist(test.list), ncol=length(test.list)))
#   names(test.prod) <- c('target', names(MH))
# #   print(list(upper=paste0('target~',paste0(names(MH), collapse="+"),sep=""),
# #              lower=paste0('target~1')))
# #   print(names(test.prod))
# #print(as.formula(paste0(targ.var,"~",paste0(names(MH),collapse="+"))))
# #print(summary(test.prod))
#   lm.train.model <- step(lm(as.formula(paste0('target',"~1")),
#                             data=test.prod, na.action='na.omit'), #.^2, 
#                          scope=list(upper=paste0(targ.var,'~',paste0(names(MH), collapse="+"),sep=""),
#                                     lower=paste0(targ.var,'~1')),
#                          scale=0, direction=direc, 
#                          trace=0,
#                          keep=NULL, steps=(length(names(MH))-1), k=k.val)
#   out.data <- array(NA, dim=dim(MF[[1]]))
#   nrips <- dim(MF[[1]])[4]
#   for(n in 1:nrips){
#     MF.slice <- lapply(MF, '[', ,,,n,,)
#     MF.slice <- lapply(MF.slice, 'as.vector')
#     #notna.index <- which(!is.na(unlist(MF.slice[grepl(targ.var, #paste0(targ.var, ".0.0"), 
#     #                                                  names(MF.slice))])))
#     tmp.out <- suppressWarnings(predict(lm.train.model, newdata=MF.slice))
#     if(length(tmp.out)==0){
#       warning(paste("Warning in callStep.lm: step eliminated all variables,", 
#                     "resulting in output of length 0! Returning the MF data for now"))
#       out.data[,,,n,,] <- MF.slice[grepl(targ.var, #paste0(targ.var, ".0.0"), 
#                                          names(MF.slice))]
#     }else{
#       out.data[,,,n,,] <- tmp.out
#     }
#   }
#   return(list(ds=out.data))
# }

