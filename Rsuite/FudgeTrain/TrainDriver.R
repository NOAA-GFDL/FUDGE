TrainDriver <- function(target.masked.in, hist.masked.in, fut.masked.in, ds.var='tasmax', att.table=NA,
                        mask.list, ds.method=NULL, k=0,  
                        create.ds.out=TRUE,
                        time.steps=NA, istart = NA,loop.start = NA,loop.end = NA, downscale.args=NULL, 
                        ds.orig=NULL, #Correcting a dimension error
                        s3.instructions=list(onemask=list('na')),
                        s5.instructions=list(onemask=list('na')), 
                        train_qcmask=FALSE,
                        min.present=1,
                        create.qc.mask=FALSE, verbose=FALSE){
  #' Function to loop through spatially,temporally and call the training and downscaling functions.
  #' @param target.masked.in, hist.masked.in, fut.masked.in: The historic target/predictor and 
     #' future predictor datasets to which spatial masks have been applied earlier
     #' in the main driver function
     #' @param mask.list: The list of time windowing masks and their corresponding
     #' time series to be applied to the time windows; returned from (insert link)
     #' TimeMaskQC.
     #' @param ds.method: name of the downscaling method to be applied to the data.
     #' List of currently available methods located at: (insert link)
     #' @param ds.var: The target variable being downscaled. 
     #' @param att.table: The attribute table of the 
     #' @param k: The number of k-fold cross-validation steps to be performed. If k > 1, 
     #' kfold masks will be generated during TrainDriver. Currently only accepts k=0
     #' @param 
     #' 
     #' Note: Many of the other parameters are from previous attempts to make the training functions
     #' more general, and capable of applying section 5 and section 3 adjustments to other data; 
     #' that track has been abandoned, but might be picked up later.
     
     
     # Initialize ds.vector 
     message("Entering downscaling driver function")
     rip.list <- lapply(att.table$fut.pred, '[[', 'rip')
     rips <- unique(rip.list)
     ds.varnames <- paste0(ds.var, ".", rips)
     print(paste("downscaled variable names:", paste(ds.varnames, collapse=", ")))
     if(create.ds.out){
       #Should only be false if explicitly running with the intent of creating only
       #a QC mask - all other cases produce *some* ds.out
       targ.dim <- dim(target.masked.in)
       ds.vector =  array(NA,dim=c(targ.dim[1:2], dim(fut.masked.in[[1]])[3]))    #x,y,t (no ens)
       if(verbose){print(paste("dimensions of downscaling output vector:", paste(dim(ds.vector), collapse=" ")))}
       ds.out <- list()
       for (name in ds.varnames){
         ds.out[[name]] <- ds.vector
       }
     }else{
       ds.out <- NULL
     }
     if(create.qc.mask){
       qc.mask <-  list()
       for(name in ds.varnames){
         qc.mask[[name]] <- ds.vector
       }
       s5.adjust <- TRUE
     }else if(length(s5.instructions) > 0){ #s5.instructions[[1]]!='na' #!is.null(s5.instructions[[1]])
       qc.mask <- NULL
       s5.adjust <- TRUE #It's binary: you either make adjustments, or create a qc mask. It never does nothing. #FALSE
     }else{
       #If s5.instructions=='na'
       qc.mask <- NULL
       s5.adjust <- FALSE
     }
     print('assessing s3 instructions')
     if(length(s3.instructions)!=0){ #!is.null(s3.instructions[[1]])
       #All pre-processing can do is adjust values; that will always happen
       s3.adjust <- TRUE
     }else{
       s3.adjust <- FALSE
     } 
     print(paste('s3.adjust:', s3.adjust))
     
     if(k>1){
       #Create Kfold cross-validation mask
       print('Cross-validation not supported at this time')
     }else{
       #Create mask for which all values are TRUE
       kfold.mask=list('na')
     }   
     #TODO CEW: Add the cross-validation mask creation before looping over the timeseries
     #(assumes that all time series will be of same length)
     #Also keep in mind: both the time windows and the kfold masks are, technically, 
     #time masks. You're just doing a compression step immediately after one but not the other.
     
     for(i.index in 1:targ.dim[1]){  #Most of the time, this will be 1
       for(j.index in 1:targ.dim[2]){ #
       #for(j.index in 1:1){
         if(sum(!is.na(target.masked.in[i.index,j.index,]))!=0 &&
              sum(!is.na(hist.masked.in[[1]][i.index,j.index,]))!=0 &&
              sum(!is.na(fut.masked.in[[1]][i.index,j.index,]))!=0){ #For predictors, use first entry as a proxy for NAs across the board
           message(paste("Begin processing point with i = ", i.index, "and j =", j.index))
           #First, determine the indices of interest for the two predictor datasets
           loop.temp <- LoopByTimeWindow(train.predictor = lapply(hist.masked.in, '[', i.index, j.index,),#hist.masked.in[i.index, j.index,], 
                                         train.target = target.masked.in[i.index, j.index,], 
                                         esd.gen = lapply(fut.masked.in, '[', i.index, j.index,), 
                                         ds.var=ds.var,
                                         att.table=att.table,
                                         mask.struct = mask.list, 
                                         create.ds.out=create.ds.out, downscale.fxn = ds.method, downscale.args = downscale.args, 
                                         kfold=k, kfold.mask=kfold.mask, graph=FALSE, masklines=FALSE, 
                                         ds.orig=ds.orig[i.index, j.index,],
                                         #s5.adjust=s5.adjust, s5.method=s5.method, s5.args = s5.args, 
                                         s3.instructions=s3.instructions, s3.adjust=s3.adjust,
                                         s5.instructions=s5.instructions, s5.adjust=s5.adjust,
                                         train_qcmask=train_qcmask,
                                         create.qc.mask=create.qc.mask,
                                         min.present=min.present,
                                         verbose=verbose, test=FALSE#(j.index==40)
           )
#            save(file="~/Code/testing/test_out.R", save='loop.temp')
#            stop("wanted to look more: output of TrainDriver tests")
           #Previously create.adjust.out here; not needed anymore
           if (create.ds.out){
             #If we are not in the "write only the qc data" case
             for(name in ds.varnames){
               ds.out[[name]][i.index, j.index, ] = loop.temp$downscaled[[name]]
             }
           }
           if(create.qc.mask){
             for(name in ds.varnames){
               qc.mask[[name]][i.index, j.index, ] <- loop.temp$qc.mask[[name]]
             }
           }
         }else{
           #Nothing needs to be done because there is already a vector of NAs of the right dimensions inititalized.
           message(paste("Too many missing values in i =", i.index,",", "j =", j.index,"; skipping without downscaling"))
         }
         #stop('examine output')
       }#for j
     }#for i
     #stop('examine output')
     ####### Loop(1) ends ###################################
#      save(list=c('ds.out', 'hist.masked.in', 'target.masked.in', 'fut.masked.in'), file='/home/cew/Code/testing/test_out.R')
#      print('starting plot commands')
#      plot(seq(1:length(ds.out[[1]][1,1,])), ds.out[[1]][1,1,], type='l', col='green')
#      print(length(fut.masked.in[[1]][1,1,]))
#      print(length(ds.out[[1]]))
#      print(summary(ds.out[[1]]))
#      lines(seq(1:length(ds.out[[1]][1,1,])), fut.masked.in[[1]][1,1,], col='red')
#      stop('examine this output')
     return(list('esd.final' = ds.out, 'qc.mask' = qc.mask)) #'postproc.out'=postproc.out))
     ############## end of TrainDriver.R ############################
}