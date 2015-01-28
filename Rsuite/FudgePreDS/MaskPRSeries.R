#'MaskPRSeries.R
#'Function for setting all values of precip
#'less than a threshold to NA
#'------Parameters related to the data being masked-------
#'@param ref.data, adjust.data, adjust.future: The precipitation data
#'for the target and predictor (ref and adjust), as well as an optional
#'future dataset (adjust.fufutre, currently used for CDFt)
#'@param ref.units, adjust.unts, adjust.future.units: The units of 
#'precipitation. Defaults to kg m-2 s-1 for all pr units. 
#'
#'------Parameters related to the internal masking options-----
#'@param opt.wetday: The kind of threshold to apply. Currently accepts
#''us_trace' (0.01 in/day), 'global_trace' (0.1 mm/day), 'zero'
#'(no precipitation), and a user-generated option of the same units
#'as preciptation (i.e. 0.5). 
#'@param lopt.drizzle : The option for applying a drizzle adjustment. 
#'Converts the parameter for trace precipitation in the adjusted data
#'to the 
#'@param lopt.conserve: The option for preserving trace precipitation.
#'If present, the 
#'
#'TODO: Discuss transformation options for the if...else case 
#'with John.
#'TODO: Discuss whether to do this before or after doing the masks. 
#'Will more missing values in the input data affect the results? 
#'TODO: Add *major* lat/lon coordiante agreement QC checks

AdjustWetdays <- function(ref.data, ref.units='kg m-2 s-1', 
                          adjust.data=NA, adjust.units='kg m-2 s-1', 
                          opt.wetday, lopt.drizzle=FALSE, lopt.conserve=FALSE, 
                          lopt.graphics=FALSE, verbose=TRUE,
                          adjust.future=NA, adjust.future.units='kg m-2 s-1'){
  
  #Debug code for testing
  print("ref.data")
  print(summary(ref.data))
  print("adjust.data")
  print(summary(adjust.data))
  print("adjust.future")
  print(summary(adjust.future))
  
  ref.wetdays <- MaskPRSeries(ref.data, ref.units, opt.wetday)
  adjust.wetdays <- MaskPRSeries(adjust.data, adjust.units, opt.wetday)
  #print(length(adjust.future))
  if(length(adjust.future) > 1 ){ #|| !is.na(adjust.future)){
    print("activiating future quantile option")
    future.wetdays <- MaskPRSeries(adjust.future, adjust.future.units, opt.wetday)
  }
  ##Loop over all lat/lon points available in the input datasets
  ref.data.new <- ref.data
  adjust.data.new <- adjust.data
  if(length(adjust.future) > 1 ){ #|| !is.na(adjust.future)){
    future.data.new <- adjust.future
  }
  #print(dim(ref.data))
  for (i in 1:dim(ref.data)[1]){
    for (j in 1:dim(ref.data)[2]){ 
      loop.ref <- ref.data[i,j,][!is.na(ref.data[i,j,])]
      loop.ref.wetdays <- ref.wetdays[i,j,][!is.na(ref.wetdays[i,j,])]
      loop.adj <- adjust.data[i,j,][!is.na(adjust.data[i,j,])]
      loop.adj.wetdays <- adjust.wetdays[i,j,][!is.na(adjust.wetdays[i,j,])]
      if(length(adjust.future) > 1) {#||!is.na(adjust.future)){
        loop.fut <- adjust.future[i,j,][!is.na(adjust.future[i,j,])]
        loop.fut.wetdays <- future.wetdays[i,j,][!is.na(future.wetdays[i,j,])]
      }
      if (lopt.drizzle == TRUE && length(loop.ref)!=0) { #Avoid running if all NA values present
        fraction.wet.ref <- sum(loop.ref.wetdays) / length(loop.ref.wetdays)
        fraction.wet.adj <- sum(loop.adj.wetdays) / length(loop.adj.wetdays)
        # Perform the following calculations and adjustments to the GCM time series
        # only if the user has asked for the drizzle adjusment to be applied 
        if(j%%10==0 || j==1){
          print(" Consider applying drizzle adjustment")
        }
        if (fraction.wet.adj > fraction.wet.ref) {
          if(j%%10==0 || j==1){
            print(" Need to do drizzle adjustment")
          }
          first.above.threshold <- quantile(loop.ref,  probs=(1.0-fraction.wet.ref),na.rm=TRUE)  
          small.fraction <- 1.0/length(loop.ref)
          last.below.threshold <- quantile(loop.ref, probs=(1.0-fraction.wet.ref-small.fraction), na.rm=TRUE)
          #       if (under.development == TRUE) {
          #         print(c(" last, first", last.below.threshold, first.above.threshold))
          #         print(c(" last, first", last.below.threshold, first.above.threshold))
          #       }
          num.zero.in.adjusted <- ((1.0-fraction.wet.ref)*length(loop.adj)) - 1
          threshold.wetday.adj <- quantile(loop.adj, names = FALSE,
                                           probs=(num.zero.in.adjusted/length(loop.adj)), 
                                           na.rm=TRUE)
                     print(threshold.wetday.adj)
                    print(paste("threshold:", threshold.wetday.adj))
          loop.adj.wetdays <- loop.adj > threshold.wetday.adj
          #           print(paste("length of adj.wetdays:", length(adj.wetdays)))
          if(length(adjust.future) > 1 ){ #|| !is.na(adjust.future)){
            loop.fut.wetdays <- loop.fut > threshold.wetday.adj
          }
        } else {
          if(j%%10==0 || j==1){
            print(" No need to do drizzle adjustment")
          }
        }
      } else {
        if(j%%10==0 || j==1){
          print(" Not considering drizzle adjustment ")
        }
      }
      ###Now consider the conserve option
      if(lopt.conserve==TRUE){
        if(j%%10==0 || j==1){
          print("entering conserve option")
        }
  #      total.trace.pr <- sum(loop.ref[loop.ref.wetdays==FALSE])
        ####REALLY think about how to do this compare
  #      pr.adjust <- total.trace.pr/sum(loop.ref.wetdays[!is.na(loop.ref.wetdays)])
  #      loop.ref[loop.ref.wetdays==TRUE] <- (loop.ref[loop.ref.wetdays==TRUE] + pr.adjust)
        loop.ref <- conserve.prseries(loop.ref, loop.ref.wetdays)
        
        ###And do the same thing for the adjusted data
        if(j%%10==0 || j==1){
          print("starting adjust section")
        }
#         total.trace.pr <- sum(loop.adj[loop.adj.wetdays==FALSE])
#         pr.adjust <- total.trace.pr/sum(loop.adj.wetdays)
#         loop.adj[loop.adj.wetdays==TRUE] <- (loop.adj[loop.adj.wetdays==TRUE]  + pr.adjust)
 #       loop.adj[loop.adj.wetdays==TRUE] <- conserve.prseries(loop.adj, loop.adj.wetdays)
        loop.adj <- conserve.prseries(loop.adj, loop.adj.wetdays)
        ###and the future, if that applies
        if(length(adjust.future) > 1){ #|| !is.na(adjust.future)){
          total.trace.pr <- sum(loop.fut[loop.fut.wetdays==FALSE])
          
          pr.adjust <- total.trace.pr/sum(loop.fut.wetdays)
          loop.fut[loop.fut.wetdays==TRUE] <- (loop.fut[loop.fut.wetdays==TRUE] + pr.adjust)
        }
      } 
      ref.data[i,j,][!is.na(ref.data[i,j,])] <- loop.ref
      ref.wetdays[i,j,][!is.na(ref.wetdays[i,j,])] <- loop.ref.wetdays
      adjust.data[i,j,][!is.na(adjust.data[i,j,])] <- loop.adj 
      adjust.wetdays[i,j,][!is.na(adjust.wetdays[i,j,])] <- loop.adj.wetdays 
      #Future 'if' scenario
      if(length(adjust.future) > 1 ){   #||!is.na(adjust.future))
        adjust.future[i,j,][!is.na(adjust.future[i,j,])] <- loop.fut
        future.wetdays[i,j,][!is.na(future.wetdays[i,j,])] <- loop.fut.wetdays
      }
    }
  }
  out.list <- list("ref" = list("data"=as.numeric(ref.wetdays)*ref.data, "pr_mask"=ref.wetdays), 
                   "adjust" = list("data" = as.numeric(adjust.wetdays)*adjust.data, "pr_mask" = adjust.wetdays))
  if(length(adjust.future) > 1) {#||!is.na(adjust.future)){
    out.list$future <- list("data" = as.numeric(future.wetdays)*adjust.future, "pr_mask" = future.wetdays)
  }
  print("about to return results")
  print("ref.data")
  print(summary(as.numeric(ref.wetdays)*ref.data))
  print("adjust.data")
  print(summary(as.numeric(adjust.wetdays)*adjust.data))
  print("adjust.future")
  print(summary(as.numeric(future.wetdays)*adjust.future))
  return(out.list)
}

conserve.prseries <- function(data, mask){
#   print(length(data))
#   print(length(mask))
  total.trace.pr <- sum(data[mask==FALSE])
  if(total.trace.pr > 0){
  pr.adjust <- total.trace.pr/sum(mask)
  #print("data before adjustment")
 # print(summary(data))
  data[mask==TRUE]  <- data[mask==TRUE] + pr.adjust
  #print("data after adjustment")
  #print(summary(data))
  }else{
    #Do nothing (justified by a few papers that should be cited later)
  }
  return(data)
}


MaskPRSeries <- function(data, units, index){
  #'Converts all values within the dataset
  #'less than the threshold to an NA, 
  #'depending upon the argument passed. 
  #'Requires the Udunits2 package to 
  #'work. 
  print(paste("Entering masking function with threshold", index))
  #   print(data[1:100])
  #   print(data[1:100] > index)
  units <- units.CF.convert(units)
  #Set options for determining what qualifies as a 'wet day'
  zero.thold = 0
  us.trace.thold = ud.convert(0.01, "inches/day", units)
  global.trace.thold = ud.convert(0.1, "mm/day", units)
  ####TODO: Move away from hard-coded constants
#   us.trace.thold = 2.939815e-06
#   global.trace.thold = 1.157407e-06
  switch(index, 
         #'zero' = return(replace.vals(data, data==zero.thold)),
         #'zero' = return(apply(apply(data, c(1,2,3), all.equal, zero.thold), c(1,2,3), isTRUE))
         #Ignore the above. Machine precision or no, it's hideous. 
         'zero' = return(data > zero.thold),
         'us_trace' = return(data > us.trace.thold), 
         'global_trace' = return(data > global.trace.thold), 
         return((data > index))) #If own threshold supplied,
}                               #It is assumed that you know the units  

replace.vals<-function(series, tf.index){
  #Replaces all values for which the index is
  #true with a NA
  out<- series
  out[tf.index==TRUE]<-NA
  return(out)
}

units.CF.convert <- function(unit.string){
  #Performs simple conversions needed between CF-compliant
  #units and the udunits2 package. 
  unitlist<-sub('kg m-2 s-1', "l m-2 s-1", unit.string)
  #unitlist<-sub('C', "°C", unitlist)
}