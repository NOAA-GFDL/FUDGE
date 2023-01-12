

decimate_dim <- function(dim_len, stride, offset=0, type='include'){
  #'Function for calculating decimation coordinates for a dimension.
  #'Will generate a sequence from 1:dim_len:dim_stride; 
  #'if 'include', will return coords to include those points
  #'(include every 10th, data_size = data_size/10)
  #''exclude' will exclude them (remove every 10th; data_size = data_size * 0.9)
#   print('decimate_dim args:')
#   print(offset)
#   print(str(dim_len))
#   print(str(stride))
  dim_seq <- seq(from=(1+offset), to=dim_len, by=stride)
  if (type == 'include'){
    return(dim_seq)
  }else if(type == 'exclude'){
    dim_vals <- 1:dim_len
    dim_seq <- dim_vals[dim_seq * (-1)]
    return(dim_seq)
  }else{
    stop(paste("Error in decimate_dim:"))
  }
}

decimate_var <- function(var_array, dec_axes=c(1), dec_stride=c(2), dec_offset=c(0)){
  #Decimates a variable that presents as an array. 
  #The axes of the array are identified.
  var_dims <- dim(var_array)
  new_dims <- rep(NA, length.out = length(var_dims))
  #...okay. So if I do my prior approach of filling each dim, 
  #I'm going to wind up creating a set of indices that are the same size as 
  #the array. That's nto memory-efficient, even if it might be fast.
  for (d in 1:length(dec_axes)){
    dim = dec_axes[d]
    stride = dec_stride[d]
    offset = dec_offset[d]
# #     print('decimate_var args:')
# #     print(stride)
# #     print(offset)
# print('dimensions of var_array')
# print(dim(var_array))
# print(d)
# print(var_dims[d])
    dcoord <- decimate_dim(var_dims[dim], stride, offset=offset, type='include')
    var_array <- subset_array(var_array, dim=dim, dim_select=dcoord, drop=FALSE)
    gc()
  }
  return(var_array)
}

subset_array <- function(var_array, dim=1,dim_select=c(1,2), drop=FALSE, verbose=F){
  #Subsets the ${dim}th dimension of an array by the criteria provided
  #in dim_select.
  #...This is a somehat more elegant subtitute for eval and parse.
  #Some of technique taken from this thread:
  #https://stackoverflow.com/questions/14500707/select-along-one-of-n-dimensions-in-array
  indices <- rep(list(bquote()), length(dim(var_array)))
  indices[[dim]] <- dim_select
  #bquote() evaluates an empty object in the chosen environment.
  #here, it's the list of subsets - all but the one we want are empty
  
  call <- as.call(c(
    list(as.name("["), quote(var_array)),
    indices,
    list(drop = drop)))
  # Print it, just to make it easier to see what's going on
  if (verbose){ print(call) }
  
  # Finally, evaluate it
  eval(call)
}