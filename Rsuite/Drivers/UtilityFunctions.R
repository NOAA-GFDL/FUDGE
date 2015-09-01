#UtilityFunctions.R
#'Useful internal functions for FUDGE that do not fit within
#'a specific subfunction
#'Functions may switch out of here as the code adapts
#'Carolyn Whitlock, January 2015

# post_ds = list(mask1=list(type='PR', qc.mask='off', adjust.out='off', loc='outloop', 
#                            qc_args=list(thold='us_trace', freqadj='off')), 
#                mask2=list(type='flag.neg',adjust.out='off',qc.mask='on', loc='inloop',
#                            qc_options=list('na')))

add.over.missvals <- function(val1, val2){
  #Adds the two values.
  #If one value is a missing value, returns
  #the value of the non-missing value
  #If both are missing, returns a missval
  if(sum(is.na(val1), is.na(val2))!=1){
    return(sum(val1, val2))
  }else{
    return(ifelse(is.na(val1), val2, val1))
  }
}

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
      if(length(this.vector)==2){
        out <- paste(this.vector, collapse=" and ")
      }else{  
        out <- paste(c(this.vector[1:length(this.vector)-1], paste("and ", this.vector[length(this.vector)])), collapse=", ")
      }
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

count.missvals <- function(data){
  #Counts the number of missing values in a vector
  #Helper function for indexing lapply
  sum(is.na(data))
}

adapt.pp.input <- function(mask.list=list('na'), pr_opts=list('na')){
  #' Fast and dirty modifications to make the pre- and post-processing 
  #' scheme that the XML currently supports match the R infrastucture
  #' for the more general version yet to be implemented
  pre_ds=list()
  post_ds=list()
  if(pr_opts[[1]]!='na'){
    if('apply_0_mask' %in% names(pr_opts)){
      message(paste("Warning in adapt.pp.input: use of apply_0_mask as an opt name is deprecated!", 
                    "Changing to new name, ignore0"))
      pr_opts['ignore0'] <- pr_opts$apply_0_mask
    }
    pre_ds$propts <- list(type='PR', var='pr', apply='all', loc='outloop', 
                          pp.args=list(thold=pr_opts$pr_threshold_in,
                                       freqadj=pr_opts$pr_freqadj_in,
                                       conserve=pr_opts$pr_conserve_in, 
                                       ignore0=pr_opts$ignore0))
                                       #apply_0_mask=pr_opts$apply_0_mask))
    post_ds$propts <- list(type='PR', 
                           adjust.out='on', 
                           qc.mask='off', 
                           loc='outloop', 
                           qc_args=list(thold=pr_opts$pr_threshold_out,
                                        conserve=pr_opts$pr_conserve_out, 
                                        ignore0=pr_opts$ignore0))
                                        #apply_0_mask=pr_opts$apply_0_mask))
  }
  if(mask.list[1]!='na'){
    i.index <- length(post_ds)
    for(i in 1:length(mask.list)){
      post_ds[[i+i.index]] <- list(type=mask.list[[i]]$type,
                                   adjust.out=mask.list[[i]]$adjust.out,
                                   qc.mask=mask.list[[i]]$qc.mask,
                                   loc=ifelse(mask.list[[i]]$type=='flag.neg', 
                                              'outloop', 'inloop'),
                                   qc_args=mask.list[[i]]$qc_options)
      ##DISCUSS RENAMING THESE
    }
  }
  #Adding bugfix for flagging negative values that may be introduced
  #by the pr s5 options
#   if('flag.neg' %in% names(post_ds)){
#     post_ds$flag.neg$loc <- 'outloop'
#   }
  return(list('pre_ds'=pre_ds, 'post_ds'=post_ds))  
}

remove.missvals <- function(data){
  #Removes missing values from an array (presumed 1-d)
  return(data[!is.na(data)])
}

interp.points <- function(startpoint, endpoint, len.out, mode){
  #Linear interpolation of len.outvalues between two points, including the starting point
  #Obtained by non-random sampling
  if(mode=='linear'){
    return(startpoint + ((endpoint-startpoint)/(len.out-1))*seq(from=0, by=1, to=(len.out-1)))
  }else{
    return(c(startpoint, rep(endpoint, len.out-1)))
  }
}

################### Interpolation functions
interpolate.points <- function(invec, len.outvec, interp.mode='linear'){
  #Adds or subtracts points in a vector invec, maintaining its
  #distribution, in order to match an input length, len.outvec.
  #Is not meant to be called if len.outvec == len(invec)
  #Obtains points from random sampling
  outvec <- rep(NA, len.outvec)
  
  if(length(invec) > len.outvec){
    #If fewer points are needed
    set.seed(seed=8675309, kind="Mersenne-Twister", normal.kind="Inversion")
    indices <- sort(sample.int(n=length(invec), len.outvec, replace=FALSE))
    outvec <- invec[indices]
  }else{
    #okay, trying a new technique here: 
    set.seed(seed=8675309, kind="Mersenne-Twister", normal.kind="Inversion")
    #Need one set of vectors for every point in the out vector, 
    #And one set of randomly selected points for the remainder
    indices <- sort(c( rep(seq(1:length(invec)), floor(len.outvec/length(invec)) ), 
                       sample.int(n=(length(invec)), size=(len.outvec%%length(invec)), replace=FALSE)))
    outvec <- invec[indices]
  }
  return(outvec)
}

#TODO: replace once R.utils installed
simpleCap <- function(x){
  #Capitalizes the first letter of the word
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#Helper function for assigining using time windowing masks in 
#the downscaling by time window
assign.by.index <- function(array1, array2, index){
  array1[!is.na(index)] <- array2[!is.na(array2)]
  return(array1)
}