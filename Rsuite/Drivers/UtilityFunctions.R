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

index.a.list <- function(lst, index, val){
  #Returns the list if the member of the 
  #list indicated by index is equal to val
  # (helper function for lapply - doesn't speed code, 
  #  but will make it more readable)
  #if (index %in% names(lst)){
    if(lst[[index]]==val){
      return(lst)
    }
  #}
}

both.na <- function(x, y){
  #Convenience frunction for checking if
  #two vectors are both NA in a location
  return(is.na(x) && is.na(y))
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

count.present.vals <- function(data){
  #Counts number of values not-missing in array
  return(sum(!is.na(data)))
}

count.vals <- function(data, val, op="=="){
  return(sum(do.call(op, list(data, val)), na.rm=TRUE))
}

kount.vals <- function(data, thold=NA){
  #Returns a vector of the number of missing values, the number of
  #zero values, and the count of everything not zero or missing
  d.len <- length(data)
  miss.vals <- count.missvals(data)
  zero.vals <- count.vals(data,0)
  if(is.na(thold)){
    thold.vals=NULL
  }else{
    thold.vals=count.vals(data, thold, op=">")
  }
  return(c(missing=miss.vals, zero=zero.vals, present=(d.len-(miss.vals+zero.vals)), thold=thold.vals))
}

adapt.pp.input <- function(mask.list=list('na'), pr_opts=list('na'), ds.ref=list('na')){
  #' Fast and dirty modifications to make the pre- and post-processing 
  #' scheme that the XML currently supports match the R infrastucture
  #' for the more general version yet to be implemented
  pre_ds=list()
  post_ds=list()
  train_qcmask <- FALSE
  if(pr_opts[[1]]!='na'){
    #As of early I, pairwise methods (lm, lmAsync, PARM) all get adjusted from
    #within the twindow loops
    if (ds.ref$is_pairwise){
      pr.loc='inloop'
      allow.low.mh <- TRUE
    }else{
      pr.loc='outloop'
      allow.low.mh <- FALSE
    }
    if('apply_0_mask' %in% names(pr_opts)){
      message(paste("Warning in adapt.pp.input: use of apply_0_mask as an opt name is deprecated!", 
                    "Changing to new name, ignore0"))
      pr_opts['ignore0'] <- pr_opts$apply_0_mask
    }
    pre_ds$propts <- list(type='PR', var='pr', apply='all', loc=pr.loc, 
                          pp.args=list(thold=pr_opts$pr_threshold_in,
                                       freqadj=pr_opts$pr_freqadj_in,
                                       conserve=pr_opts$pr_conserve_in, 
                                       ignore0=ifelse(pr_opts$ignore0=='off', 
                                                      'off', 'on'), 
                                       allow_low_mh = allow.low.mh)
    )
    if ('pr_adjmethod' %in% names(pr_opts)){
      pre_ds$propts$pp.args['pr_adjmethod'] <- pr_opts$pr_adjmethod
    }else{
      #Maintain H-compatible default until wrapup of I
      pre_ds$propts$pp.args['pr_adjmethod'] <- 'exact'
    }
                          ###Change added 9-23 to test the ppr opts)
                                       #apply_0_mask=pr_opts$apply_0_mask))
    #print(str(pr_opts))
    if('transform' %in% names(pr_opts)){
      #print(str(pr_opts))
      if(pr_opts$transform!='none'){
        if ('vars' %in% names(pr_opts)){
          vars = unlist(strsplit(pr_opts$vars, ","))
        }else{
          #This is an option from legacy XML
          vars = "pr"
        }
        pre_ds <- c(pre_ds, 
                    list(list(type='transform', vars=vars, 
                              apply='by.var', loc='inloop', 
                         pp.args=list(transform=pr_opts$transform, 
                                      arg=pr_opts$arg, 
                                      vars=vars
                                      ))))
      }
      #print(lapply(pre_ds, 'summary'))
    }else{
      message(paste("Warning in adapt.pp.input: no transfrom option was present in the", 
                    "XML, and no transform will be added to the pr data."))
    }
    if ('ignore0' %in% names(pr_opts)){
      ignore0 = ifelse(pr_opts$ignore0=='off', 
                       'off', 'on')
    }else{
      #Legacy code; use old setting
      ignore0 = FALSE
    }
    post_ds$propts <- list(type='PR', 
                           adjust.out='on', 
                           qc.mask='off', 
                           loc=pr.loc, 
                           qc_args=list(thold=pr_opts$pr_threshold_out,
                                        conserve=pr_opts$pr_conserve_out, 
                                        ignore0=ignore0))
  }
  if ('pr_adjmethod' %in% names(pr_opts)){
    post_ds$propts$qc_args['pr_adjmethod'] <- pr_opts$pr_adjmethod
  }else{
    #Maintain H-compatible default until wrapup of I
    post_ds$propts$qc_args['pr_adjmethod'] <- 'exact'
  }
  flag.negvar <- FALSE
  qc.maskvar <- FALSE
  if(mask.list[[1]]$type!='na'){
    i.index <- length(post_ds)
    for(i in 1:length(mask.list)){
      if(mask.list[[i]]$type!='qc.train'){
        if(mask.list[[1]]$type=='flag.neg'){
          flag.negvar <- TRUE
        }
        if(mask.list[[i]]$qc.mask=='on'){
          qc.maskvar <- TRUE
        }
        post_ds[[i+i.index]] <- list(type=mask.list[[i]]$type,
                                     adjust.out=mask.list[[i]]$adjust.out,
                                     qc.mask=mask.list[[i]]$qc.mask,
                                     loc=ifelse(mask.list[[i]]$type=='flag.neg', 
                                                'outloop', 'inloop'),
                                     qc_args=mask.list[[i]]$qc_options)
        ##DISCUSS RENAMING THESE
      }else{
        train_qcmask=TRUE
        qc.maskvar <- TRUE
      }
    }
  }
  if('flag_neg'%in%names(pr_opts)){
    if(flag.negvar){
      stop(paste("Error in adapt.pp.input: flag.neg was specified in s5 while the", 
                 " flag_neg tag was used in the pr_opts section"))
    }
    if(pr_opts$flag_neg=='on'){
      flag.negopt <- list(type='flag.neg', 
                          adjust.out='on', 
                          qc.mask=(ifelse(!qc.maskvar, 
                                          'on', 'off')),
                          loc='outloop',
                          qc_args=list('na')) #Changed for user readability 7-1-16
      #Check for prior qc option that returns a qc mask
      post_ds <- c(post_ds, list(flag.negopt))
    }#else{
    #stop('check on why this is failing')
    #}
    #print('post_ds')
    #print(lapply(post_ds, 'summary'))
    #print(post_ds)
    #stop('examine post_ds structure')
  }
#   print(pre_ds)
#   print(post_ds)
#   stop('examine this output')
  return(list('pre_ds'=pre_ds, 'post_ds'=post_ds,'train_qcmask'=train_qcmask))  
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

override.f.defaults <- function(ds.args=NA, 
                                pr_opts=NA){
  #Temporary F to G-bridging code to set some of the parameters
  #needed for the targeted F development
  #Includes the minsamplesize and the overriding of missing value 
  #handling at the XML level
  out.list <- list()
  if(!is.na(ds.args[1])){
    if(!is.null(ds.args$minsamplesize)){
      out.list$threshold <- ds.args$minsamplesize
      #remove minsamplesize from args
      if(length(ds.args)==1){
        out.list$args <- list('na')
      }else{
        out.list$args <- ds.args[names(ds.args)!='minsamplesize']
      }
    }else{
      out.list$threshold <- 1
      out.list$args <- ds.args
    }
  }
# Pretty sure as of 2017-05-29 that this bit isn't used by anything 
# else anymore. Commenting out to check.
#   if(!is.na(pr_opts[1])){
#     if ('ignore0' %in% names(pr_opts)){
#       if(!(pr_opts$ignore0=='off' || pr_opts$ignore0=='on')){
#         missval.handling <<- pr_opts$ignore0
#       }
#     }
#   }
  #Final check added for ignore0.Delta and the pr ignore0 option
  if('ignore0.Delta'%in%names(ds.args)){
    if(ds.args$ignore0.Delta=='on' && pr_opts$ignore0!='off'){
      stop(paste("Error in override.f.defaults! The options", 
                 "ignore0 in the pr args and ignore0.Delta", 
                 "in the DeltaSD downscaling arguments are mutually", 
                 "exclusive! Please turn one of them off!"))
    }
  }
  return(out.list)
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

#An inverse function for the log10 operation. 
#Used by the precipitation transforms
exp10 <- function(x){
  return(10^x)
}

get_orig_experiment <- function(in.file, var='tasmax'){
  #Gets the original experiment from an input file
  #according to the metadata present
  print(paste("obtaining file", in.file))
  test.nc <- nc_open(in.file)
  atts = ncatt_get(test.nc, 0)
  nc_close(test.nc)
  att.names <- names(atts)
  test.1 <- c("model_id", 'experiment', 'realization')
  test.2 <- c('parent_experiment', "experiment_id", 'realization')
  test.3 <- c("model_id", 'experiment_id')
  test.4 <- c('title')
  if(sum(sapply(test.1, '%in%', att.names))==3){
    orig_exp <- paste0(atts['model_id'], ", ", atts['experiment'])
  }else if (sum(sapply(test.2, '%in%', att.names))==3){
    orig_exp <- paste0(atts['parent_experiment'], ", ", atts['experiment_id'])
  }else if (sum(sapply(test.3, '%in%', att.names))==2){
    orig_exp <- paste0(atts['model_id'], ", " ,atts['experiment_id'])
  }else{
    #Assume only title att present
    orig_exp <- gsub(paste0(var,", "), "", atts['title']) #var with or without a trailing comma
  }
if (grepl("GFDL-HIRAM-C360-COARSENED", orig_exp)){
  #Perfect model-specific corrections
  print(atts['experiment_id'])
  emissions_scenario <- switch(atts[['experiment_id']], 
                               'sst2030' = 'RCP 4.5', 
                               'sst2090' = 'RCP 8.5')
  if('pyhsics_version'%in%names(atts)){
    physics <- switch(as.character(atts[['physics_version']]), 
                      '1' = 'ESM', 
                      '2' = 'CM3')
  }else{
    physvers = grep("[0-9]+p(.*)_", in.file, perl=T)
    physics <- switch(physvers, 
                      '1'='ESM', 
                      '2'='CM3')
  }
  orig_exp <- paste("GFDL-HIRAM", emissions_scenario, physics, 
                    atts['experiment_id'])
  
}
  return(orig_exp)
}

all.identical <- function(x){
  #Taken from this stackoverflow exchange: 
  #http://stackoverflow.com/questions/4752275/
  #test-for-equality-among-all-elements-of-a-single-vector
  test <- diff(range(x)) < .Machine$double.eps ^ 0.5
  if (is.na(test)){
    return(sum(is.na(x))==length(x))
  }else{
    return(test)
  }
}
