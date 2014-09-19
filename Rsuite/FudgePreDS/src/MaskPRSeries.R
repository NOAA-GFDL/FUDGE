#'MaskPRSeries.R
#'Function for setting all values of precip
#'less than a threshold to NA
#'@param data
#'@param index: The kind of threshold to apply. 
#'TODO: Discuss transformation options for the if...else case 
#'with John.
#'TODO: Discuss whether to do this before or after doing the masks. 
#'Will more missing values in the input data affect the results? 
#'TODO: Add *major* lat/lon coordiante agreement QC checks

AdjustWetdays <- function(ref.data, ref.units='kg m-2 s-1', 
                          adjust.data, adjust.units='kg m-2 s-1', 
                          opt.wetday, lopt.drizzle=FALSE, lopt.conserve=FALSE, 
                          lopt.graphics=FALSE, verbose=TRUE,
                          adjust.future=NA, adjust.future.units='kg m-2 s-1'){
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
  print(dim(ref.data))
  for (i in 1:dim(ref.data)[1]){
    for (j in 1:dim(ref.data)[2]){ 
      loop.ref <- ref.data[i,j,][!is.na(ref.data[i,j,])]
      loop.ref.wetdays <- ref.wetdays[i,j,][!is.na(ref.wetdays[i,j,])]
      loop.adj <- adjust.data[i,j,][!is.na(adjust.data[i,j,])]
      loop.adj.wetdays <- adjust.wetdays[i,j,][!is.na(adjust.wetdays[i,j,])]
      if(length(adjust.future) > 1) {#||!is.na(adjust.future)){
        #print(length(adjust.future))
        loop.fut <- adjust.future[i,j,][!is.na(adjust.future[i,j,])]
        loop.fut.wetdays <- future.wetdays[i,j,][!is.na(future.wetdays[i,j,])]
      }
      if (lopt.drizzle == TRUE && length(loop.ref)!=0) { #Avoid running if all NA values present
        fraction.wet.ref <- sum(loop.ref.wetdays) / length(loop.ref.wetdays)
        #         print(fraction.wet.ref)
        fraction.wet.adj <- sum(loop.adj.wetdays) / length(loop.adj.wetdays)
        #         print(fraction.wet.adj)
        # Perform the following calculations and adjustments to the GCM time series
        # only if the user has asked for the drizzle adjusment to be applied 
        if(j%%10==0 || j==1){
          print(" Consider applying drizzle adjustment")
        }
      #  print(fraction.wet.adj)
      #  print(fraction.wet.ref)
        if (fraction.wet.adj > fraction.wet.ref) {
          if(j%%10==0 || j==1){
            print(" Need to do drizzle adjustment")
          }
          #      print(c(fraction.wet.adj," > ",fraction.wet.ref), sep =" ")
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
          #           print(threshold.wetday.adj)
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
        total.trace.pr <- sum(loop.ref[loop.ref.wetdays==FALSE])
        ####REALLY think about how to do this compare
        pr.adjust <- total.trace.pr/sum(loop.ref.wetdays[!is.na(loop.ref.wetdays)])
        loop.ref[loop.ref.wetdays==TRUE] <- (loop.ref[loop.ref.wetdays==TRUE] + pr.adjust)
        
        ###And do the same thing for the adjusted data
        if(j%%10==0 || j==1){
          print("starting adjust section")
        }
        total.trace.pr <- sum(loop.adj[loop.adj.wetdays==FALSE])
        pr.adjust <- total.trace.pr/sum(loop.adj.wetdays)
        #         print(sum(loop.adj.wetdays))
        #         print(pr.adjust)
#         print(paste("pr adjust:", pr.adjust))
#         if(pr.adjust >0){
#        print(paste("pr adjust", pr.adjust))
#         }
#         print(paste("loop adjust:", sum(loop.adj)))
        loop.adj[loop.adj.wetdays==TRUE] <- (loop.adj[loop.adj.wetdays==TRUE]  + pr.adjust)
#         loop.new <- (loop.adj + rep(pr.adjust, length(loop.adj)))
#         print(paste("loop adjust:", sum(loop.new)))
#         print(sum(loop.new-loop.adj))
#         loop.adj[loop.adj.wetdays==TRUE] <- loop.new[loop.adj.wetdays==TRUE]
        ###and the future, if that applies
        if(length(adjust.future) > 1){ #|| !is.na(adjust.future)){
          # print("line 118")
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
  return(out.list)
}


MaskPRSeries <- function(data, units, index){
  #'Converts all values within the dataset
  #'less than the threshold to an NA, 
  #'depending upon the argument passed. 
  #'Requires the Udunits2 package to 
  #'work. 
  print(index)
  #   print(data[1:100])
  #   print(data[1:100] > index)
#   units <- units.CF.convert(units)
  #Set options for determining what qualifies as a 'wet day'
  zero.thold = 0
#   us.trace.thold = ud.convert(0.01, "inches/day", units)
#   global.trace.thold = ud.convert(0.1, "mm/day", units)
  ####TODO: Move away from hard-coded constants
  us.trace.thold = 2.939815e-06
  global.trace.thold = 1.157407e-06
  switch(index, 
         #'zero' = return(replace.vals(data, data==zero.thold)),
         #'zero' = return(apply(apply(data, c(1,2,3), all.equal, zero.thold), c(1,2,3), isTRUE))
         #Ignore the above. Machine precision or no, it's hideous. 
         'zero' = return(data==zero.thold),
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
# 
# 
# ####Consider carefully how to store each when returning from adjust/transforms
# fraction.wet.ref <- sum(ref.wetdays[!is.na(ref.wetdays)]) / sum(!is.na(ref.wetdays))
# print(fraction.wet.ref)
# #Possibly problematic because it counts all days with "NA" as having no precipitation
# fraction.wet.ref <- sum(ref.wetdays[!is.na(ref.wetdays)]) / sum(!is.na(ref.wetdays))
# print(fraction.wet.adj)
# if (lopt.drizzle == TRUE) {
#   # Perform the following calculations and adjustments to the GCM time series
#   # only if the user has asked for the drizzle adjusment to be applied 
#   print(" Consider applying drizzle adjustment ")
#   if (fraction.wet.adj > fraction.wet.ref) {
#     print(" Need to do drizzle adjustment")
#     #      print(c(fraction.wet.adj," > ",fraction.wet.ref), sep =" ")
#     first.above.threshold <- quantile(ref.data,  probs=(1.0-fraction.wet.ref),na.rm=TRUE)  
#     small.fraction <- 1.0/length(!is.na(ref.data))
#     last.below.threshold <- quantile(ref.data, probs=(1.0-fraction.wet.ref-small.fraction), na.rm=TRUE)
#     #       if (under.development == TRUE) {
#     #         print(c(" last, first", last.below.threshold, first.above.threshold))
#     #         print(c(" last, first", last.below.threshold, first.above.threshold))
#     #       }
#     num.zero.in.adjusted <- ((1.0-fraction.wet.ref)*length(adjust.data)) - 1
#     threshold.wetday.adj <- quantile(adjust.data, names = FALSE,
#                                      probs=(num.zero.in.adjusted/length(adjust.data)), 
#                                      na.rm=TRUE)
#     print(threshold.wetday.adj)
#     print(paste("threshold:", threshold.wetday.adj))
#     #adjust.wetdays <- MaskPRSeries(adjust.data, adjust.units, threshold.wetday.adj)
#     adjust.wetdays <- adjust.data > threshold.wetday.adj
#     print(paste("length of adjust.wetdays:", length(adjust.wetdays)))
#     if(!is.na(adjust.future)){
#       print('calculating future parameters')
#       #future.wetdays <- MaskPRSeries(adjust.future, adjust.future.units, threshold.wetday.adj)
#       future.wetdays <- adjust.future > threshold.wetday.adj
#     }
#   } else {
#     print(" No need to do drizzle adjustment")
#     #       print(c(fraction.wet.adj," <= ",fraction.wet.ref), sep =" ")
#     #      threshold.wetday.adj <- threshold.wetday.ref
#   }
# } else {
#   print(" Not considering drizzle adjustment ")
#   #    threshold.wetday.adj <- threshold.wetday.ref
# }
# ####And add the conserve option
# ###Evenly divides all trace precipitation on 'dry' days
# ###between the wet days of the dataset
# ###Should be performed by all the 
# if(lopt.conserve==TRUE){
#   print("entering conserve option")
#   total.trace.pr <- sum(ref.data[!is.na(ref.data) && ref.wetdays==FALSE])
#   pr.adjust <- total.trace.pr/sum(ref.wetdays[!is.na(ref.wetdays)])
#   print(total.trace.pr)
#   print(pr.adjust)
#   print(sum(ref.wetdays[!is.na(ref.wetdays)]))
#   print(dim(ref.wetdays))
#   print(dim(ref.data))
#   ref.data[!is.na(ref.data) && ref.wetdays==TRUE] <- (ref.data + pr.adjust)
#   
#   ###And do the same thing for the adjusted data
#   print("starting adjust section")
#   total.trace.pr <- sum(adjust.data[!is.na(adjust.data) && adjust.wetdays==FALSE])
#   pr.adjust <- total.trace.pr/sum(adjust.wetdays)
#   print(sum(adjust.wetdays))
#   print(pr.adjust)
#   adjust.data[adjust.wetdays==TRUE] <- adjust.data + pr.adjust
#   ###and the future, if that applies
#   if(!is.na(adjust.future)){
#     total.trace.pr <- sum(adjust.future[!is.na(adjust.future) && future.wetdays==FALSE])
#     pr.adjust <- total.trace.pr/sum(future.wetdays)
#     print(sum(future.wetdays))
#     print(pr.adjust)
#     adjust.data[future.wetdays==TRUE] <- adjust.future + pr.adjust
#   }
# }