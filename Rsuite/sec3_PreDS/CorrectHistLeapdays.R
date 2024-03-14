
correct.hist.leapdays <- function(h.time.len, f.time.len,
                                  #hist.timeseries, fut.timeseries, 
                                  hist.start.year, hist.end.year, 
                                  fut.start.year, fut.end.year){
  #'Corrects the index of the leapdays in the OH data to match the 
  #'index of the leapdays in the MF data by removing days from 
  #'the front and back of the OH sample. Returns the offsets for the
  #'front and back of the historical sample, labelled as front.offset
  #'and back.offset
  print('applying OH mask correction!')
  years <- seq(1960, 2100)
  leapyears <- c(1960,1964,1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,
                  2008,2012,2016,2020,2024,2028,2032,2036,2040,2044,2048,2052,
                  2056,2060,2064,2068,2072,2076,2080,2084,2088,2092,2096)
  days.per.year <- rep(365, length(years))
  days.per.year[years%in%leapyears] = 366 #366-day correction for leapdays  
  hist.range <- c(substr(hist.start.year, 1, 4), substr(hist.end.year, 1, 4))
  fut.range <- c(substr(fut.start.year, 1, 4), substr(fut.end.year, 1, 4))
  hist.range <- as.numeric(hist.range)
  fut.range <- as.numeric(fut.range)
  start.year <- hist.range[1]
  end.year <- hist.range[2]
  fut.leap.index <- leapyears[leapyears >= fut.range[1]][1] - fut.range[1]
  hist.leap.index <- leapyears[leapyears >= hist.range[1]][1] - hist.range[1]
  if((hist.leap.index==fut.leap.index) && (h.time.len >= f.time.len)){
    #No adjustments need to be made
    return(c(front.offset=0, back.offset=0, start.year=start.year, end.year=end.year))
  }else{
    #remove enough years from front of vector to match the leapday index
    leapday.shift <- (hist.leap.index-fut.leap.index)%%4
    print(paste("leapday.shift:", leapday.shift))
    start.year <- start.year + leapday.shift
    leapday.offset <- sum(days.per.year[(years > start.year) & (years <= start.year + leapday.shift)])
    start.year <- start.year + leapday.shift
    print(paste("leapday.offset:", leapday.offset))
    if( (h.time.len-leapday.offset) < f.time.len){
      #Make sure that the historical time period is comensurable by 4, for leapdays
      #(the centennial leap years shouldn't matter now, because of y2k)
      max.hist.index <- floor((end.year - start.year)/4)*4 # of commensurable leapday periods covered
      end.year <- start.year + (max.hist.index - 1)
      leapday.range <- sum(days.per.year[(start.year) < years & (start.year + max.hist.index) >= years]) #start.year + max.hist.index
      print(leapday.range)
      divide.offset <- h.time.len-(leapday.range + leapday.offset)
    }else{
      #You're never going to need to divide anything
      divide.offset <- 0
    }
  }
  return(c(front.offset=leapday.offset, back.offset=divide.offset, start.year=start.year, end.year=end.year))
}
