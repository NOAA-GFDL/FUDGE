#!/bin/Rscript
#stride and subset metadata functions
#general goal: get stride and subset working without needing
#conditional vars within the main body of the program

add_slicing_metadata <- function(meta_list, att_list){
  #meta_list is a subset of the args list that includes the 
  #specs you are interested in - e.g. subset, stride, inset. 
  #list that gets passed in. That has the parsing you are interested in.
  none_strings <- c('none', "", NULL, 'fake')
  for (el in names(meta_list)){
    if ( !is.null(meta_list[[el]]) && !(meta_list[[el]] %in% none_strings) ){
      att_list[[el]] = meta_list[[el]]
    }
  }
  return(att_list)
}
