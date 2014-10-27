TrainDriver <- function(target.masked.in, hist.masked.in, fut.masked.in, mask.list, ds.method=NULL, k=0,  
                        create.ds.out=TRUE,
                        time.steps=NA, istart = NA,loop.start = NA,loop.end = NA, downscale.args=NULL, 
                        ds.orig=NULL, #Correcting a dimension error
#                         s5.adjust=FALSE, s5.method='totally.fake', s5.args = NULL, 
#                         create.qc.mask=FALSE, create.adjust.out=FALSE
                        s5.instructions=list(onemask=list('na')), create.qc.mask=FALSE){
#' Function to loop through spatially,temporally and call the Training guts.
#' @param target.masked.in, hist.masked.in, fut.masked.in: The historic target/predictor and 
#' future predictor datasets to which spatial masks have been applied earlier
#' in the main driver function
#' @param mask.list: The list of time windowing masks and their corresponding
#' time series to be applied to the time windows; returned from (insert link)
#' TimeMaskQC.
#' @param ds.method: name of the downscaling method to be applied to the data.
#' Can currently accept simple.lm, a simple linear model, or CDFt.
#' @param k: The number of k-fold cross-validation steps to be performed. If k > 1, 
#' kfold masks will be generated during TrainDriver.
#' ---Optional arguments for use in debugging---
#' @param  loop.start: J loop start index
#' @param loop.end: J loop end index
#' @param time.steps: the # time steps; defaults to NA
#' @param istart: 

     
     # Initialize ds.vector 
   message("Entering downscaling driver function")
   if(create.ds.out){
     #Should only be false if explicitly running with the intent of creating only
     #a QC mask - all other cases produce *some* ds.out
     ds.vector =  array(NA,dim=c(dim(fut.masked.in))) #c(istart,loop.end,time.steps)
   }else{
     ds.out <- NULL
   }
   if(create.qc.mask){
     qc.mask <-  array(NA,dim=c(dim(fut.masked.in)))
     s5.adjust <- TRUE
   }else{
     qc.mask <- NULL
     s5.adjust <- FALSE
     for (element in 1:length(s5.instructions)){
       if(s5.instructions[[element]]$adjust.out!='na'){
         s5.adjust <- TRUE
       }
     }
   }
    

   print(k)
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
   print(dim(target.masked.in))
     for(i.index in 1:length(target.masked.in[,1,1])){  #Most of the time, this will be 1
       for(j.index in 1:length(target.masked.in[1,,1])){ 
         message(paste("Begin processing point with i = ", i.index, "and j =", j.index))
         ##I'm not entirely sure what this is supposed to do, and I'm relucant to tinker with it too much.
         #if(sum(!is.na(target.masked.in[1,j.index,]))!=length(target.masked.in[,,1])){   ##Why was this specified as [1,jindex,1]?
         #TODO: Specify better method for determining whether to call a downscaling function
         if(sum(!is.na(target.masked.in[i.index,j.index,]))!=0 &&
              sum(!is.na(hist.masked.in[i.index,j.index,]))!=0 &&
              sum(!is.na(fut.masked.in[i.index,j.index,]))!=0){
           loop.temp <- LoopByTimeWindow(train.predictor = hist.masked.in[i.index, j.index,], 
                                         train.target = target.masked.in[i.index, j.index,], 
                                         esd.gen = fut.masked.in[i.index, j.index,], 
                                         mask.struct = mask.list, 
                                         create.ds.out=create.ds.out, downscale.fxn = ds.method, downscale.args = downscale.args, 
                                         kfold=k, kfold.mask=kfold.mask, graph=FALSE, masklines=FALSE, 
                                         ds.orig=ds.orig[i.index, j.index,],
                                         #s5.adjust=s5.adjust, s5.method=s5.method, s5.args = s5.args, 
                                         s5.instructions=s5.instructions, s5.adjust=s5.adjust,
                                         create.qc.mask=create.qc.mask, create.adjust.out=create.adjust.out
                                         )
           if (create.ds.out || create.adjust.out){
             #If we are not in the "write only the qc data" case
             ds.vector[i.index, j.index,] <- loop.temp$downscaled
           }
           if(create.qc.mask){
             qc.mask[i.index, j.index, ] <- loop.temp$qc.mask
           }
#            if(create.postproc.out){
#              print(length(loop.temp$postproc.out))
#              print(summary(loop.temp$postproc.out))
#              postproc.out[i.index, j.index, ] <- loop.temp$postproc.out
#            }
         }else{
           #Nothing needs to be done because there is already a vector of NAs of the right dimensions inititalized.
           print(paste("Too many missing values in i =", i.index,",", "j =", j.index,"; skipping without downscaling"))
         }
       }
     }
     ####### Loop(1) ends ###################################
     return(list('esd.final' = ds.vector, 'qc.mask' = qc.mask)) #'postproc.out'=postproc.out))
     ############## end of TrainDriver.R ############################
}