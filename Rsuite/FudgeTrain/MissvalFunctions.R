#'MissvalFunctions.R
#'Code for counting, assessing, removing and adding back in
#'missing values for the FUDGE downscaling process

assess.missvals <- function(OH, MH, MF, 
                            #OH.mask=NA, MH.mask=NA, MF.mask=NA, 
                            #time.mask=NA, 
                            type='distributional'){
  #'Determines how to go about removing missing values before downscaling
  print(type)
  switch(type, 
         'distributional' = remove.all.missvals(OH, MH, MF), 
         'synchronous' = remove.synch.missvals(OH, MH, MF), 
         stop(paste("Error in assess.misvals: provided type", type, 
                    "was not one of the available options!")))
}

remove.all.missvals <- function(OH, MH, MF){
  #suitable for use in the QM methods; 
  #was used by all methods for the 
  return(MH=lapply(MH, 'remove.missvals'),  
         OH=remove.missvals(OH), 
         MF=lapply(MF, 'remove.missvals'))
}

remove.synch.missvals <- function(OH, MH, MF){
                                  #OH.mask, MH.mask){
  synch.mask <- lapply(MH, 'add.missval.vectors', OH)
#   print(summary(synch.mask))
#   print(summary(synch.mask[[1]]))
#   save(file='/home/cew/Code/testing/test_out.R', list=c('OH', 'MH', 'MF', 'synch.mask'))
#   stop('examine the output')
  MH.new <- lapply(MH, 'apply.any.mask', synch.mask[[1]], 'temporal')
  OH.new <- apply.any.mask(OH, synch.mask[[1]], 'temporal')
#   print(length(MH.new[[1]]))
#   print(length(OH.new))
#   stop('examine output')
  return(list(MH = lapply(MH.new, 'remove.missvals'), 
              OH= remove.missvals(OH.new), 
              MF=lapply(MF, 'remove.missvals')))
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
  print('lenght MF:')
  print(length(MF))
  print('lenght tmask:')
  print(length(time.mask))
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

add.back.in <- function(data, ref){
  #adds data back into a reference vector
#   print(summary(data))
#   print(summary(ref))
#   print(sum(!is.na(ref)))
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