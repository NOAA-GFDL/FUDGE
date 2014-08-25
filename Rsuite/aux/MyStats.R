#Author: Diana/OU 
#Modified: Aparna 
MyStats <- function(x,verbose="off") {
  # This function computes and prints some basic statistics of a numeric vector.
  #
  # Arg:
  #  x: numerical vector for which a small set of statistics will be calculated
  # Returns:
  #  A named list of 5 elements, each being a particular statistic
  a=mean(x,na.rm=TRUE)
  b=var(x,na.rm=TRUE)
  c=max(x,na.rm=TRUE)
  d=min(x,na.rm=TRUE)
  e=sum(x != 0)
  f=sum(x==0)
  #TODO calculate totalno. of missing values and data points like ferret stats
  g=sum(x,na.rm=FALSE)
  
  if(verbose == "yes"){
 print("..........................")
 print(paste("Minimum Value: ",d,sep=''))
 print(paste("Maximum Value: ",c,sep=''))
 print(paste("Mean Value   : ",a,sep=''))
 print(paste("Variance     : ",b,sep=''))
 print("..........................") 
  }
 
  return(list("mean"=a,
              "variance"=b,
              "max.value"=c,
              "min.value"=d,
              "nonzero.count"=e,
              "zero.count"=f))
}
