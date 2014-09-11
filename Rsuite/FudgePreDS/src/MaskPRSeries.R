#'MaskPRSeries.R
#'Function for setting all values of precip
#'less than a threshold to NA
#'@param data
#'@param index: The kind of threshold to apply. 

MaskPRSeries <- function(data, index){
  #'Converts all values within the dataset
  #'less than the threshold to an NA, 
  #'depending upon the argument passed. 
  #'Requires the Udunits2 package to 
  #'work. 
  switch(index, 
         'zero' = return(replace.vals(data, data==0)), 
         'us_trace' = return(replace.vals(data, data < 2.4)), 
         'global_trace' = return(replace.vals(data, data < 1.162791e-06)), 
         return(replace.vals(data, data < index)))
}
replace.vals<-function(series, tf.index){
  out<- series
  out[tf.index==TRUE]<-NA
  return(out)
}