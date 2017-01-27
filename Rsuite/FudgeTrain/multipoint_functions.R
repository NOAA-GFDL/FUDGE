#multipoint_functions.R
#functions for dealing with more than one xy point's worth of data
#in the input dataset. Generally used as a preprocessing step
#before the 

compress.xy.data <- function(data){
  #Takes a list of 6-dimensional vars
  #and compresses x and y dims into separate vars
  output <- list()
  for(el in names(data)){
    output <- c(output, compress.xy.var(data[[el]], el))
  }
  return(output)
}

compress.xy.var <- function(data, varname){
  #Receives a 6-dimensional set of data; 
  #compresses each x and y element into separate vectors
  out <- list()
  dim.x <- dim(data)[1]
  dim.y <- dim(data)[2]
  x.indices <- seq( -(floor(dim.x/2)), by=1, length.out=dim.x)
  y.indices <- seq( -(floor(dim.y/2)), by=1, length.out=dim.y)
  x.indices <- sub("-", "n",x.indices)
  y.indices <- sub("-", "n",y.indices)
  for(x in 1:dim.x){
    for(y in 1:dim.y){
      loop.varname <- paste(varname,x.indices[x],y.indices[y], sep=".")
      if (!all(is.na(data[x,y,,,, ,drop=F]))){
        out[[loop.varname]] <- data[x,y,,,, ,drop=F]
      }else{
        #...this print so many messages guys. so many.
        #message(paste("All missing in", x, y, "; dropping"))
        #Note that if  we get to the point where we write out more than
        #one point at a time, this is also going to need to return which
        #indices got dropped.
      }
    }
  }
  #Relatively simple way to make sure 0.0 is first var. Position of other vars
  #may be up for discussion later.
  print(names(out))
  out <- out[sort(names(out))]
  return(out)
}

