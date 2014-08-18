#Author: Diana/OU 
MyStats <- function(x) {
  # This function computes and prints some basic statistics of a numeric vector.
  #
  # Arg:
  #  x: numerical vector for which a small set of statistics will be calculated
  # Returns:
  #  A named list of 5 elements, each being a particular statistic
  a=mean(x)
  b=var(x)
  c=max(x)
  d=min(x)
  e=sum(x != 0)
  f=sum(x==0)
  return(list("mean"=a,
              "variance"=b,
              "max.value"=c,
              "min.value"=d,
              "nonzero.count"=e,
              "zero.count"=f))
}
