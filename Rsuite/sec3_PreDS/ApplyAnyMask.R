#ApplyAnyMask.R
#'Applies a mask to a dataset in one of three ways:
#'1) if dim.apply is 'spatial', the mask will be a 2-D spatial mask
#'2) if dim.apply is 'time or 'temporal', the mask will be a 1-D time mask
#'3) if dim.apply is NA, the mask will be of the same dimensions as the input 
#'dataset, and should be applied to all points (such as a pr wetdays mask). 
#'
#'Note that in order to determine which dimensions to apply a mask over, the 
#'data is assumed to have its dimensions in a specific order. x,y will always 
#'be first, and time will always be last.
#'
#'@return A masked dataset of the same dimensions (in the same order) as the data that
#'came in. 
#'
#'@author Carolyn Whitlock, April 2015
#'
apply.any.mask <- function(data, mask, dim.apply=NA, na.rm=FALSE, verbose=FALSE){ #switched 'mask' and 'data' in arg
  #Applies any mask to an array
  #array is assumed to be in x,y,t order, with additional dims between
  #y and t
  if(is.null(dim(mask))){
    mask.dim <- length(mask)
  }else{
    mask.dim <- dim(mask)
  }
  if(verbose){(print(paste("dimensions of mask:", paste(mask.dim, collapse=" "))))}
  
  data.dim <- dim(data) #Assume that this will be in x,y,var,t order
  if(is.null(data.dim)){
    data.dim <- length(data)
  }
  
  if(verbose){(print(paste("dimensions of data:", paste(data.dim, collapse=" "))))}
  if(verbose){print('summary of data'); print(summary(as.vector(data)))}
  
  if(!is.na(dim.apply)){
    if(dim.apply=='time' || dim.apply=='temporal'){
      length.time <- data.dim[length(data.dim)]
      if(length.time!=mask.dim){
        stop(paste("Error in apply.any.mask over temporal points: mask had dimensions of",paste(mask.dim, collapse=" "), 
                   "and data had dimensions of", paste(data.dim, collapse=" ")))
      }else{
        if(length(data.dim) > 1){
          #There is more than one dimension present
          #         print( c(length(data.dim), 1:(length(data.dim)-1)))
          data.perm <- aperm(data, c(length(data.dim), 1:(length(data.dim)-1)))#t,x,y order
          result <- rep(mask, length.out=length(data.perm))*data.perm #t,x,y order(yay vector recycling)
          result <- aperm(result, c(2:(length(data.dim)), 1))
          ##Let's see...removing missing values might get complicated
          if(na.rm){
            mask.time <- sum[!is.na(mask)]
            result <- results[!is.na(result)]
            #This bit isn't done, don't use it yet
            ###Assume that if you are removing the NAs from time, all NAs will wind up being taken from that dim - 
            ###you want to remove them from everything else, as much as possible
            dim(result) <- c(data.dim[1:(length(data.dim)-1)], mask.time) #orig x,y,var, new time
          }
        }else{
          #Assume that the only dimension is time
          result <- data*mask
        }
        if(verbose){print('summary of data'); print(summary(as.vector(result)))}
        
      }
    }else if(dim.apply=='spatial'){
      dim.spatial <- mask.dim
      if(sum(dim.spatial==data.dim[1:2])!=2){
        stop(paste("Error in apply.any.mask over spatial points: mask had dimensions of",paste(mask.dim, collapse=" "), 
                   "and data had dimensions of", paste(data.dim, collapse=" ")))
      }else{
        result <- as.vector(data) * as.vector(mask) #x,y,ens/var,t order (if ens exists)
        dim(result) <- data.dim
      }
      #Not implementing a na.rm option for this; too prone to abuse in current version of code
    }
  }else{
    #If no application dim is returned, assume that the dims of the mask and the 
    #data are supposed to be identical, and act accordingly
    if(sum(mask.dim!=data.dim)==0){
      #If they are not non-identical anywhere
      result <- as.vector(mask)*as.vector(data)
      dim(result) <- data.dim
    }else{
      stop(paste("Error in apply.any.mask over all points: mask had dimensions of",paste(mask.dim, collapse=" "), 
                 "and data had dimensions of", paste(data.dim, collapse=" ")))
    }
  }
  return(result)
}

trim.time.mask <- function(data, mask, verbose=FALSE){ #switched 'mask' and 'data' in arg
  #Trims an array or a vector by the last dimension when provided with a 1-D 
  #mask vector of the same length as teh last dim
  #array is assumed to be in x,y,t order, with additional dims between
  #y and t
  if(is.null(dim(mask))){
    mask.dim <- length(mask)
  }else{
    mask.dim <- dim(mask)
    if(length(mask.dim) > 1){
      stop(paste('error in trim.time.mask: time mask should be one-dimensional'))
    }
  }
  if(verbose){(print(paste("dimensions of mask:", paste(mask.dim, collapse=" "))))}
  
  data.dim <- dim(data) #Assume that this will be in x,y,var,t order
  if(is.null(data.dim)){
    data.dim <- length(data)
  }
  
  if(verbose){(print(paste("dimensions of data:", paste(data.dim, collapse=" "))))}
  if(verbose){print('summary of data'); print(summary(as.vector(data)))}
  
  length.time <- data.dim[length(data.dim)]
  if(length.time!=mask.dim){
    stop(paste("Error in trim.time.mask over temporal points: mask had dimensions of",paste(mask.dim, collapse=" "), 
               "and data had dimensions of", paste(data.dim, collapse=" ")))
  }else{
    if(length(data.dim) > 1){
      #There is more than one dimension present
      #         print( c(length(data.dim), 1:(length(data.dim)-1)))
      perm.dim <- c(length(data.dim), seq(1:(length(data.dim)-1)))
      data.perm <- aperm(data, perm.dim)#t,x,y order
      result <- data.perm[!is.na(rep(mask, length.out=length(data.perm)))] #t,x,y order(yay vector recycling)
#       print(dim(data))
#       print(perm.dim)
#       print(dim(data)[perm.dim])
#       print(length(result))
      #Drop first dimension (time); re-partition time dim
      dim(result) <- c(length(result)/prod(dim(data)[perm.dim[-1]]), dim(data)[perm.dim[-1]])
#       print(length(result))
#       print(dim(result))
#       print(c(seq(2, (length(data.dim))), 1))
      result <- aperm(result, c(seq(2, (length(data.dim))), 1))
      ##Let's see...removing missing values might get complicated
    }else{
      #Assume that the only dimension is time
      result <- data[!is.na(mask)]
    }
    if(verbose){print('summary of data'); print(summary(as.vector(result)))}
    
  }
  return(result)
}