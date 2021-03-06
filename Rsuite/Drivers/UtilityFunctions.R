#UtilityFunctions.R
#'Useful internal functions for FUDGE that do not fit within
#'a specific subfunction
#'Functions may switch out of here as the code adapts
#'Carolyn Whitlock, January 2015


index.a.list <- function(list, index, val){
  #Returns the list if the member of the 
  #list indicated by index is equal to val
  # (helper function for lapply - doesn't speed code, 
  #  but will make it more readable)
  if(list[[index]]==val){
      return(list)
  }
}

compact <- function(x){
  #Removes null values from a function;
  #Needed if your lapply function will return
  #null values a lot of the time
  #Taken from a forum post by Hadley Wickham
  #as the preferred way to deal with nulls in apply()
  Filter(Negate(is.null), x)
}
  
convert.list.to.string <- function(this.vector){
  #Converts a list into a string representation
  #Does not assume that the list is named
  #(easy to convert though; just count off of the names)
  if(length(this.vector)!=0){
    if(length(this.vector) > 1){
      out <- paste(c(this.vector[1:length(this.vector)-1], paste("and", this.vector[length(this.vector)])), collapse=",")
      return(out)
    }else{
      #no 'and' needed
      return(paste(this.vector))
    }
  }else{
    #No string to convert
    return(NA)
  }
}

adapt.pp.input <- function(mask.list=list('na'), pr_opts=list('na')){
  #' Fast and dirty modifications to make the pre- and post-processing 
  #' scheme that the XML currently supports match the R infrastucture
  #' for the more general version yet to be implemented
  pre_ds=list()
  post_ds=list()
  if(mask.list[[1]]!='na'){
    for(i in 1:length(mask.list)){
      post_ds[[i]] <- list(type=mask.list[[i]]$type,
                           adjust.out=mask.list[[i]]$adjust.out,
                           qc.mask=mask.list[[i]]$qc.mask,
                           loc='inloop',
                           qc_args=mask.list[[i]]$qc_options)
      ##DISCUSS RENAMING THESE
    }
    print(post_ds)
  }
  if(pr_opts[[1]]!='na'){
    pre_ds$propts <- list(type='PR', var='pr', apply='all', loc='outloop', 
                          pp.args=list(thold=pr_opts$pr_threshold_in,
                                       freqadj=pr_opts$pr_freqadj_in,
                                       conserve=pr_opts$pr_conserve_in, 
                                       apply_0_mask=pr_opts$apply_0_mask))
    post_ds$propts <- list(type='PR', 
                           adjust.out='on', 
                           qc.mask='off', 
                           loc='outloop', 
                           qc_args=list(thold=pr_opts$pr_threshold_out,
                                           conserve=pr_opts$pr_conserve_out))
  }
  return(list('pre_ds'=pre_ds, 'post_ds'=post_ds))  
}