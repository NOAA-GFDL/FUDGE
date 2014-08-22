#CrossValidate.R
#Carolyn Whitlock, August 2014

#' CrossValidate
#' Performs K-fold cross validation upon a predictor and target dataset.
#' Will mask data according to the k-fold validation structure, and call
#' a downscaling method.
#' Predictor and target should be of the same size. Will this always be 
#' the case? Ask Keith later.
#' @param train.predict: a vector of predictor data
#' @param train.target: a vector of the target data
#' @param esd.gen: a vector of the data used to generate downscaled data *OR* a list
#' of vectors to which downscaled data will be applied.This option is not yet implemented in the code, 
#' but this is where it will go. Note that even if there is a list, this data will be unused
#' if k > 1.
#' @param k : the k-fold cross-validation steps to be used. 
#' @param downscale.function: a string of the downscaling function to be used in the data. 
#' Talk to Carlos about the possible inputs into this function.
#' Code currently assumes that file containing downscaling function
#' has already been sourced into the workspace by something else.
#' @param args = NULL: a list of arguments to be passed to the downscaling function.
#' Defaults to NULL (no arguments)
#' @param cols=FALSE: vector of color data to be used for debugging. 
#' @examples 
#' train_predictor <- seq(1:101)
#' train_target <- train_predictor^1.4 + 12
#' esd_gen <-seq(from=1, to=151, by=1)
#' #### Running the cross-validation function
#' k0 <- CrossValidate(train_predictor, train_target, esd_gen, 0, "ESD.Train.totally.fake")
#' k4 <- CrossValidate(train_predictor, train_target, esd_gen, 4, "ESD.Train.totally.fake")
#' @references \url{link to the FUDGE API documentation}
#' TODO: Will predictor and target always be of the same length?
#' TODO: Does MaskMerge need to be sourced in this location?
#' TODO: Modify script to accept a list of esd.gen vectors, call MaskMerge multiple times, 
#' and perform simple error checking to see if there's a conflict with kfold.

CrossValidate <- function(train.predict, train.target, esd.gen, k, downscale.function="ESD.Train.totally.fake", 
                          args=NULL, cols=FALSE){ #downscale.function
  #source('MaskMerge.R')
  #Check the input for consistency
  k0.methods <- c("CDFt")
  crossval = TRUE
  if(downscale.function%in%k0.methods){
    if(k>1){
      stop(paste("Method Selection Error: method", downscale.function, "does not support FUDGE cross-validation"))
    }else{
      crossval = FALSE
    }
  }
  #First things first: determine if cross-validation needs to be performed at all.
  if (k>1){
    #Determine masks for k-fold cross-validation
    #Note: if pressed for time/memory, can eliminate masks and call the index generation
    #function directly. 
    ###I realize that it's going to take code restructuring, but you need to make sure, at some point, 
    ##that the masks which are being used in the cross-validation case actually don't collide.
    ##Colliding masks is making it look like I have data when, in fact, I do not.
    k.mask <- K.FoldMasker(length(train.predict), k)
    #esd.mask <- K.Fold.Masker(length(esd.gen), k)                   #You may need to take these masks into account, too
    #Call the downscaling function to obtain the initial predictions
    #Note: at some point, this should include a flag
    #to specify whether or not to save the downscaling equations
    #for application to a later dataset
    loop.list <- rep(list(), k)
    for (loop in 1:k){
      print(paste("entering k-fold validation loop", loop, "of", k))
      loop.mask <- k.mask[[loop]]
      loop.pred <- train.predict[loop.mask==TRUE]
      loop.target <- train.target[loop.mask==TRUE] 
      ####Institue simple check for all missing values:
       if (sum(!is.na(loop.pred))!=0 && sum(!is.na(loop.target))!=0){
        trained.function <- do.call(downscale.function, list(loop.pred, loop.target))
        #ESD.Train will incorporate checking and evaluating the function
        loop.esd.gen <- train.predict[loop.mask==FALSE]   #Changed from FALSE
        #print("loop.esd.gen:")
        #print(loop.esd.gen)
        output <- do.call("trained.function", list(c(loop.esd.gen, args)))
        #print("output:")
        #print(output)
        print(paste("the length of the output vector is", length(output)))
        temp<-rep(0, length(loop.mask))
        temp[loop.mask==FALSE] <- output #Changed from true to false.
        temp[loop.mask==TRUE] <- NA
        #print("temp:")
        #print(temp)
        loop.list[[loop]] <- temp
#       lines(1:365, loop.list[[loop]], col=cols[k])                          #Option for plotting line segments
        #lines(1:365, do.call("trained.function", list(1:365)), col=cols[k])   #Option for plotting full lines
      }else{
        print("activating all NA training contingency")
        loop.list[[loop]] <- rep(NA, length(k.mask[[k]])) #This might be too clever
     }
      #...There are redundant assignment steps in here, but at the moment it's clear to read.
      #Save that for cleanup.
    }
    #Once outside the loop, save the results of the calculation to a list to return
    print("merging data from cross-validation into single series")
    #print(loop.list)
    #save("loop.list", file="saved_loop.list")
    return(MaskMerge(loop.list, collide=TRUE))
  }else{
    if(crossval==TRUE){  #If this works, I will be annoyed
      #If K is 1 or 0, and the method supports cross-validation, 
      #run the downscaling equations on the esd.gen dataset instead.
      #Training will take place over the entire dataset for both train.predict and
      #train.target
      if (sum(!is.na(loop.pred))!=0 && sum(!is.na(loop.target))!=0){
        trained.function <- do.call(downscale.function, list(train.predict, train.target))
        print(trained.function)
        output <- do.call("trained.function", list(c(esd.gen, args)))
        #print(summary(output))
        return(output)
      }else{
        return(rep(NA, length(esd.gen)))
      }
    }else{
      if (sum(!is.na(loop.pred))!=0 && sum(!is.na(loop.target))!=0){
        #print(paste("non-missing vals:", sum(!is.na())))
        return(CDFt(train.target, train.predict, esd.gen, npas=length(esd.gen))$DS)
        #return(do.call(downscale.function, list(train.predict, train.target, esd.gen, args)))  #Currently, CDFt trips this check
      }else{
        print('tripping NA contingency')
        return(rep(NA, length(esd.gen)))
      }
    }
  }
}

K.FoldMasker<-function(p.len, k){
  # Returns k masks that partition predictor into a predictor set of
  # length length(predictor)/(k-1), and a target set of length
  # length(predictor)/k. In the event of a length(predictor)%%k!=0, 
  # the remainder is passed to the first section for which the
  # decimal round to 1.
  # Yes, the partitioning is nonrandom. This way, it's reproducable.
  p.index <- indices.calc(p.len,k)
  temp <- rep(TRUE, p.len)   #Obtain k vectors of p.len
  p.masks <- as.list(rep(list(temp), k))    #for which all values are true
  for (i in 1:k){
    p.masks[[i]][ (p.index[i]+1):p.index[i+1] ] <- FALSE #Set all values in the i-th partition
  }                                                  #of the i-th mask to FALSE
  return(p.masks)
}

indices.calc <- function(val, k){
  #Calculates indices of a vector
  #for subsetting a vector of length val
  #into k partitions of equal length
  #or as close as integers allow.
  ret<-rep(0,k)
  for (i in 1:k){
    ret[i+1] <- as.integer((val/k) * i)
  }
  print(paste("indices over which to subset within the data:
              "))
  return(ret)
}