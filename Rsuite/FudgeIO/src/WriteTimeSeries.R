WriteDSTimeSeries <- function(data.mon, out.calendar,predictor.startyr,predictor.endyr) {
  #' @param data.mon: Data for every month that is to be written to form one continuous series 
  #' @param out.calendar: calendar type ,Handles julian and noleap  
 
  #' PUT THE MONTHS BACK TOGETHER TO FORM ONE CONTINUOUS TIME SERIES
  month_lengths_leap <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  month_lengths <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ############################################################################################
  data.tseries <- c() #data time series 
  k <- rep(1,12)
  #check if passed time period and actual data samples/time steps are in sync
  if(length(data.mon[[1]])/31 == ((predictor.endyr - predictor.startyr)+1)) {
    print("Time checks:ok")
  }else{
    stop("ERROR: Time checks failed.")
  }  
  for(j in predictor.startyr:predictor.endyr){   #Do this loop once for each year
    leapflag <- 0
    for (i in 1:12){
      if (((calendar == 'julian') && (i == 2)) && ((j == 2) || (j == 6) || (j == 10) || (j == 14) || (j == 18) || (j == 22) || (j == 26) || (j == 30) || (j == 32) || (j == 36) || (j == 40) || (j == 44) || (j == 48) || (j == 52) || (j == 56) || (j == 60)) || (j == 63) || (j == 67)){
      #print(paste('','Its a february leap year',sep=''))
        message("Its a leap year......",i,j)
        nyrleap <- nyrleap + 1                      
        data.tseries <- c(data.tseries,data.mon[[i]][k[i]:(k[i]+month_lengths_leap[i]-1)])                     
        leapflag <- 1
        }else{
        data.tseries <- c(data.tseries,data.mon[[i]][k[i]:(k[i]+month_lengths[i]-1)])               
        }
      }
      if(leapflag == 1){
        k <- k + month_lengths_leap        
      }else{
        k <- k + month_lengths
      }
    }
  return(data.tseries)
}
