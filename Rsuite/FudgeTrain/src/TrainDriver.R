TrainDriver <- function(target.masked.in, hist.masked.in, fut.masked.in, mask.list, ds.method, k=0, time.steps=NA, 
                        istart = NA,loop.start = NA,loop.end = NA){
#' Function to loop through spatially,temporally and call the Training guts.
#' @param target.masked.in, hist.masked.in, fut.masked.in: The target, historic and 
#' future datasets to which spatial masks have been applied
#' @param mask.list: The list of time windowing masks to be applied to the 
#' @param ds.method: name of the downscaling method to be applied to the data.
#' @param k: The number of k-fold cross-validation steps to be performed. 
#' ---Optional arguments for use in debugging---
#' @param  loop.start: J loop start index
#' @param loop.end: J loop end index

     
     # Initialize ds.vector 
     ds.vector =  array(NA,dim=c(dim(fut.masked.in))) #c(istart,loop.end,time.steps)
     
     #TODO CEW: Add the cross-validation mask creation before looping over the timeseries
   #(assumes that all time series will be of same length)
   #Also keep in mind: both the time windows and the kfold masks are, technically, 
   #time masks. You're just doing a compression step after one but not the other.
     
     #### Loop(1) through J subset ######################### 
     #TODO loop.start,loop.end could be derived from mask lat dimension
     for(i.index in 1:length(target.masked.in[,1,1])){  #Most of the time, this will be 1
       for(j.index in 1:length(target.masked.in[1,,1])){ 
         if(sum(!is.na(target.masked.in[1,j.index,]))!=length(target.masked.in[,,1])){   ##Why was this specified as [1,jindex,1]?
           #Talk to Aparna - this code should work for both one-D
           #and 3-D data
           ds.vector[i.index, j.index,] <- DownscaleByTimeWindow(train.predictor = hist.masked.in[i.index, j.index,], 
                                                                 train.target = target.masked.in[i.index, j.index,], 
                                                                 esd.gen = fut.masked.in[i.index, j.index,],
                                                                 downscale.fxn = ds.method, 
                                                                 downscale.args = NULL, 
                                                                 kfold = k, 
                                                                 masklist = mask.list)    
           #          if(grepl('CDFt', ds.method)){
           #            list.CDFt.result <- CDFt(target.masked.in[1,jindex,],hist.masked.in[1,jindex,],fut.masked.in[1,jindex,],npas = npas)
           #            ds.vector[istart,jindex,] <- list.CDFt.result$DS
           #          }else{
           #            stop("Method not supported yet")
           #          }
         }else{
           #Nothing needs to be done because there is already a vector of NAs of the right dimensions in 
         }
       }
     }
     ####### Loop(1) ends ###################################
     return(ds.vector)
     ############## end of TrainDriver.R ############################
}