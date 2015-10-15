#'MissvalFunctions.R
#'Code for counting, assessing, removing and adding back in
#'missing values for the FUDGE downscaling process

assess.missvals <- function(OH, MH, MF, 
                            #OH.mask=NA, MH.mask=NA, MF.mask=NA, 
                            #time.mask=NA, 
                            type='distributional',
                            ds.method="", ds.args="", 
                            threshold=1, retry=FALSE, 
                            qcmask=FALSE){
  #'Determines how to go about removing missing values before downscaling
  #if(!retry){
  ds.list <- switch(type, 
                    'distributional' = remove.all.missvals(OH, MH, MF), 
                    'synchronous' = remove.synch.missvals(OH, MH, MF), 
                    'delta' = remove.delta.missvals(OH, MH, MF, ds.args, threshold),
                    stop(paste("Error in assess.misvals: provided type", type, 
                               "was not one of the available options!")))
  #Threshold call is general for all functions, except those that deal with 
  #it themselves
  if(is.null(ds.list$status)){
    #determine if there are at least that many values present
    MH.kount <- unlist(lapply(ds.list$MH, length))
    OH.kount <- length(ds.list$OH)
    MF.kount <- unlist(lapply(ds.list$MF, length))
    if(threshold >= 1){ #sample size threshold
      ds.list$status <- sum(!(c(MH.kount, OH.kount, MF.kount)>=threshold))
    }else{ #percentage threshold
      MH.korig <- unlist(lapply(MH, length))
      OH.korig <- length(OH)
      MF.korig <- unlist(lapply(MF, length))
      ds.list$status <- sum(mapply("/", c(MH.kount, OH.kount, MF.kount), 
                                   c(MH.korig, OH.korig, MF.korig))>= threshold)
    }
  }
  #Statuses over 100 are from the missing value removal functions and just need
  #a qcmask, if that applies; statuses less than that need a retry
  if(ds.list$status < 100 && ds.list$status > 0){
    print('Recovery option enabled; adding precip zeroes back into data.')
    ds.tmp <- switch(ds.method, 
                      recover.ignore0(OH,MH,MF))
    for(name in (names(ds.tmp))){
      ds.list[[name]] <- ds.tmp[[name]]
    }
    ds.list$status <- 10 #used recover.ignore0 to fix the input data
  }
  if(qcmask){
    #...you know, I am sort of glad that I don't have to worry about the multi-rip implications
    #of this bit just yet.
    ds.list$qcmask <- rep(list(rep(ifelse(ds.list$status==0, 
                                          1, NA), length(MF[[1]]))),3)
  }else{
    ds.list$qcmask <- NA
  }
  return(ds.list)
}

remove.all.missvals <- function(OH, MH, MF){
  #suitable for use in the QM methods; 
  #was used by all methods for the 
  return(list(MH=lapply(MH, 'remove.missvals'),  
         OH=remove.missvals(OH), 
         MF=lapply(MF, 'remove.missvals')))
}

remove.synch.missvals <- function(OH, MH, MF){
                                  #OH.mask, MH.mask){
  synch.mask <- lapply(MH, 'add.missval.vectors', OH)
  MH.new <- lapply(MH, 'apply.any.mask', synch.mask[[1]], 'temporal')
  OH.new <- apply.any.mask(OH, synch.mask[[1]], 'temporal')
  return(list(MH = lapply(MH.new, 'remove.missvals'), 
              OH= remove.missvals(OH.new), 
              MF=lapply(MF, 'remove.missvals')))
}

remove.delta.missvals <- function(OH, MH, MF, args, threshold, 
                                  return.qc.mask=FALSE){
  #'Very similar to distributive, but with a few 
  #'extra checks for missing thresholds in some places
  OH.new <- remove.missvals(OH)
  MH.new <- lapply(MH, 'remove.missvals')
  MF.new <- lapply(MF, 'remove.missvals')
  status = 0
  if(sum(lapply(MF.new, 'length')==0)!=0){
    #MF.new[lapply(MF.new, 'length')==0] <- 0
    message(paste("Warning in remove.delta.missvals: one or more MF  datasets", 
                  "were all 0 or all missing! Zeroes will be added in place of missing pr."))
    status=130
  }
  if(sum(lapply(MH.new, 'length')==0)!=0){
    #If any of the MH datasets are all missing
    if(args$deltaop=='ratio'){
      #You can't have all missing or all 0 when calculating a delta here
      message(paste("Warning in remove.delta.missvals: one or more MH datasets", 
                    "were all 0 or all missing when the ratio option was used!", 
                    "OH data will be returned from the delta method instead."))
      status=120 #Error code picked up by the Delta method later; used to return the 
    }else{
      #All OH args that were of length 0 now have a single 0 for their values
      #MH.new[lapply(MH.new, 'length')==0] <- 0
      message(paste("Warning in remove.delta.missvals: one or more MH datasets", 
                    "were all 0 or all missing! Zeroes will be added in place of missing pr."))
    }
    status=110
  }
  print(paste('final delta status:', status))
  return(list(MH=MH.new, OH=OH.new, MF=MF.new, status=status))
}

remove.mask.missvals <- function(data, mask){
  #under this scheme, you keep track of the places 
  #where the mask was added, and 
}

# remove.delta.missvals <- function(OH, MH, MF, 
#                                   threshold, )

restore.missvals <- function(MF, time.mask, 
                             ds, ds.mask=NA, type='remove.all'){
  #restores missing values to their proper place within a vector, 
  #based on the intersection of a time mask and a MF dataset
  #(since we are trying to restore an OF dataset)
  index.vector <- add.missval.vectors(MF, time.mask)
  out.vector <- index.vector
  out.vector[!is.na(out.vector)] <- ds
  if(!is.na(ds.mask)){
    index.vector[!is.na(index.vector)] <- ds.mask
    return(c(ds=ds, ds.mask=index.vector))
  }else{
    return(c(ds=ds))
  }
}

recover.ignore0 <- function(OH,MH,MF){
  #F-specific code to cover the case of adding the ignore0
  #zeros back into the data, when there would be insufficent
  #data to downscale otherwise.
  #This works as an F-specific hack because nothing else should
  #be capable of introducing missing values; for G, this needs
  #to be extended to the qcmasks, which get returned alongside
  #the data
  #######
  #Please note: as of 10-14, this includes a call to a messily-set
  #global variable. This is going to need to be changed in G.
  out.list <- list()
  for(el in train.names){
    if(el=='MH' || el=='MF'){
      out.list[[el]] <- lapply(eval(parse(text=el)), 'replace.missval', 0)
    }else{
      out.list[[el]] <- replace.missval(OH, 0)
    }
  }
#   return(list(MH=lapply(MH, 'replace.missval', 0),  
#               OH=replace.missval(OH, 0), 
#               MF=lapply(MF, 'replace.missval', 0)))
  return(out.list)
}

replace.missval <- function(x, val){
  #Helper function for recover.ignore0 
  #replacing all missing values in a vector
  replace(x, is.na(x), val)
}

add.back.in <- function(data, ref){
  #adds data back into a reference vector
#   print(summary(data))
#   print(summary(ref))
#   print(sum(!is.na(ref)))
#   print(sum(!is.na(data)))
#   print(length(ref))
#   print(length(data))
  ref[!is.na(ref)] <- data
  return(ref)
}

add.missval.vectors <- function(...){
  #Adds the missing values of two vectors of equal length
  #together, resulting in the union of all points for which
  #both have data
  out <- colSums(rbind(...))
  out[!is.na(out)] <- 1
  return(out)
}

kount.missvals <- function(data, N, zero.valid=TRUE){
  #A function to determine whether there is sufficent data to 
  #perform an operation. Takes three parameters: 
  #'@param data: the data to be observed
    #'@param N: The threshold compared against. If 0 < N < 1, 
    #'this is interpreted as a percentage of values present.
    #'If N <= 1, this is interpreted as an absolute threshold 
    #'of values present in the code
    data.kount <- kount.vals(data)
    if(zero.valid){
      data.kount$present <- data.kount$present + data.kount$zero
    }
    if(N >=1){
      return(data.kount$present >= N)
    }else{
      return(data.kount$present >= length(data)*N)
    }
}



# #DS function now allows for return of two args: 
# #the ds, and the qc masks
# #this is distinguished in the finally
# 
# #This means that ds.out is now a list - and gets initialized as
# #one at that level of the code. Adding the contents of ds.out 
# #back into normal vectorland is what the bits immediately after
# #the call now do
# 
# #Remove missing values from the downscaling data
# 
# ds.out <- tryCatch(out <- callDSFunction(...), 
#                    error=function(out, retry){
#                      if(retry){
#                        return(ds.retry)
#                      }
#                      out$retry <- rep(NA, out.len)
#                      return(out)
#                    }, 
#                    finally=function{
#                      #Assign input returned from the earlier functions
#                      out <- restore.missvals(out, MF, )
#                    }
#                    )
# ds.output[m,] <- ds.out$ds
# if(ds.mask){
#   ds.mask[m,] <- ds.out$mask
# }
# 
# #Insert similar structure for section 5 in here...
# return(list(ds = colSums(ds.output, na.rm=TRUE), 
#             mask = colSums(ds.mask)))
# 
# 
# 
# #################################
# 
# test.array <- array(seq(1:20), dim=c(2,10))
# #test.array[1,1] <- NA
# test.array[2,5] <- NA
# 
# reject.data <- function(data){
#   print(sum(!is.na(data)))
#   if(length(data[!is.na(data)]) < 10){
#     stop(paste("Error in reject.data! Data had too few samples!"))
#   }else{
#     return(data*10)
#   }
# }
# 
# reject.restart <- function(data){
#   data[is.na(data)] <- 0
#   reject.data(data)
# }
# 
# out=array(NA, dim=c(2,10))
# for(i in 1:2){
#   print(i)
#   out[i,] <- withRestarts(reject.data(test.array[i,]), 
#                       #Look, to be ABSOLUTELY CLEAR: 
#                       #C in this case is the error function.
# #                   error=function(c){
# #                     print(paste('prior value of c', c))
# #                     c[is.na(c)] <- 0
# #                     print(paste('current value of c', c))
# #                     return(reject.data(c))
# #                   }, 
#                       error=function(c){invokeRestart('reject.restart')},
#                   finally=function(){
#                     message(paste('this is complete'))
#                   })
# }
# 
# #'Notes: in the above case, c gets interpreted as the input,
# #'test.array[i,] . I think you could sneak several assignments into it,
# #'and have the error code check on each. 
# #'
# 
# retry_without_ignore0 <- function(...){
#   #'for now, it's 'safe' to make it work when ignore0
#   #'is the only mask for the available data that will
#   #'be used - but in the future, that assumption is going
#   #'to be under a fair amount of pressure.
#   # assumes that the datasets are whole and unmasked
#   
# }