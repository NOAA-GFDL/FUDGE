#!/bin/Rscript
#'Pairwise_adjut_pr.R
#'Modified precip adjust for the pairwise downscaling methods. 
#'These are the ones that need equal sample sizes.
#'

#Note: the equal sample size steps imply that frequency adjustment and
#ignore0 steps are done at the same time. This is due to the fact that
#you will probably have many identical zeroes at the bottom of the distribution
#for MH.

adjustTrace <- function(OH, MH, MF, 
                        oh.ord2, mh.ord2,
                        pr_trace, allow_lower_mh, ignore0, conserve){
  #MF never needs to be referenced

#   OH <- OH[oh.ord1]
#   MH <- MH[mh.ord1]
#   mh.ord <- rank(MH, ties='first')
#   oh.ord <- rank(OH, ties='first')
#   OH <- sort(OH)
#   MH <- sort(MH)
  #assume that missvals are removed before this step
  oh.dry <- sum(OH < pr_trace)
  mh.dry <- sum(MH < pr_trace)
  is_zero <- F
  adjust_low <- F
  if (mh.dry <= oh.dry){ #This was prev. GTE on 7/31 morning. Trying again with LTE
    #This is the most common case
    mh.dry <- oh.dry
    zed.idx <- oh.dry 
    mh.trace <- MH[zed.idx]
  }else{
    adjust_low <- T
    if(allow_lower_mh){
      mh.dry <- oh.dry
      zed.idx <- oh.dry 
      mh.trace <- MH[zed.idx]
      if (mh.trace <= 0){
        tmp.idx <- tail(which(MH == 0), n=1)
        mh.trace <- MH[tmp.idx+1]
        is_zero <- T
      }
    }else{
      #Don't adjust the trace; keep it the same for MH
      mh.trace <- pr_trace
    }
  }
  
  OH[1:oh.dry] <- 0
  MH[1:mh.dry] <- 0
  
  OH <- OH[oh.ord2]#[oh.ord]
  MH <- MH[mh.ord2]#[mh.ord]
  oh.mask <- c(rep(T, oh.dry), rep(F, (length(OH)-oh.dry)) )[oh.ord2]#[oh.ord]
  mh.mask <- c(rep(T, mh.dry), rep(F, (length(MH)-mh.dry)) )[mh.ord2]#[mh.ord]
  
  oh.ss <- sum(oh.mask)
  mh.ss <- sum(mh.mask)
  MF[MF < mh.trace] <- 0
  mf.mask <- MF != 0
  
  if (conserve){
    OH <- conserve_pr(OH, oh.mask)
    MH <- conserve_pr(MH, mh.mask)
    MF <- conserve_pr(MF, mf.mask)
  }
  return(list(OH=OH, MH=MH, MF=MF, 
              oh.trace=pr_trace, mh.trace=mh.trace, 
              oh.mask=oh.mask, mh.mask=mh.mask, mf.mask=mf.mask))
}

adjustPR <- function(OH, MH, MF, 
                     pr_trace, pr_units, 
                     adjust_trace, allow_lower_mh, 
                     ignore0, conserve, return_masks=F){
  #'Does the more general precip adjustments, including some missval indexing
    
  #indexing steps:
  oh.idx <- is.na(OH)
  mh.idx <- is.na(MH)
  mf.idx <- is.na(MF)
  OH <- OH[!oh.idx]
  MH <- MH[!mh.idx]
  MF <- MF[!mf.idx]
  
  pr.trace <- get_pr_trace(pr_trace, pr_units)
  
  mh.ord1 <- sort.list(MH)
  mh.ord2 <- sort.list(mh.ord1)
  oh.ord1 <- sort.list(OH)
  oh.ord2 <- sort.list(oh.ord1)
  #Sort OH and MH
  OH <- OH[oh.ord1]
  MH <- MH[mh.ord1]
  
  if (adjust_trace){
    tmp.out <- adjustTrace(OH, MH, MF, 
                           oh.ord2, mh.ord2,
                           pr.trace, allow_lower_mh, ignore0, conserve)
    OH <- tmp.out$OH
    MH <- tmp.out$MH
    MF <- tmp.out$MF
    oh.mask <- tmp.out$oh.mask
    mh.mask <- tmp.out$mh.mask
    mf.mask <- tmp.out$mf.mask
    adjust.trace <- tmp.out$mh.trace
  }else{
    adjust.trace <- pr.trace
    oh.dry <- sum(OH < pr.trace)
    mh.dry <- sum(MH < pr.trace)
    OH[1:oh.dry] <- 0
    MH[1:mh.dry] <- 0
    OH <- OH[oh.ord2]
    MH <- MH[mh.ord2]
    oh.mask <- c(rep(T, oh.dry), rep(F, length(MH)-oh.dry))[oh.ord2]
    mh.mask <- c(rep(T, mh.dry), rep(F, length(MH)-mh.dry))[mh.ord2]
    
    MF[MF < pr.trace] <- 0
    mf.mask <- MF == 0
    #print(summary(MF==0))
    #print(summary(mf.mask))
    #stop('examine the summary!')
    if (conserve){
      OH <- conserve_pr(OH, oh.mask)
      MH <- conserve_pr(MH, mh.mask)
      MF <- conserve_pr(MF, mf.mask)
    }
  }
  #...OKAY THEN:
  #as of 2017-08-01, oh.mask was producing the correct results as !oh.mask
  #in the pairwise pr adjust code and as oh.mask in the FUDGE code.
  #This is baffling, and needs to be investigated.
  if (ignore0){
    OH[oh.mask] <- NA
    MH[mh.mask] <- NA
    MF[mf.mask] <- NA
  }
  #output indexing steps:
  out.list <- list(OH=oh.idx, 
                   MH=mh.idx, 
                   MF=mf.idx)
  out.list$OH[!oh.idx] <- OH
  out.list$OH[oh.idx] <- NA
  out.list$MH[!mh.idx] <- MH
  out.list$MH[mh.idx] <- NA
  out.list$MF[!mf.idx] <- MF
  out.list$MF[mf.idx] <- NA  
#   out.list$OH <- ifelse(oh.idx, NA, OH)
#   out.list$MH <- ifelse(mh.idx, NA, MH)
#   out.list$MF <- ifelse(mf.idx, NA, MF)
  if (return_masks){
    out.list$oh.mask <- oh.idx
    out.list$oh.mask[!oh.idx] <- oh.mask
    out.list$oh.mask[oh.idx] <- NA
    out.list$mh.mask <- mh.idx
    out.list$mh.mask[!mh.idx] <- mh.mask
    out.list$mh.mask[mh.idx] <- NA
    out.list$mf.mask <- mf.idx
    out.list$mf.mask[!mf.idx] <- mf.mask
    out.list$mf.mask[mf.idx] <- NA
#     out.list$oh.mask <- ifelse(oh.idx, NA, oh.mask)
#     out.list$mh.mask <- ifelse(mh.idx, NA, mh.mask)
#     out.list$mf.mask <- ifelse(mf.idx, NA, mf.mask)  
  }  
  out.list$oh.trace <- pr.trace
  out.list$mh.trace <- adjust.trace
  return(out.list)
}

AdjustPRPoints <- function(OH, MH, MF, 
                           OH.units, MH.units, MF.units,
                           pr_vars, prime_predictor=pr_vars[1],
                           pr_trace, freqadj, conserve, ignore0, 
                           allow_lower_mh=F){
  #'Does the spatial looping when there are
  #'multiple x and y (or multivar pairs) for
  #'precipitation adjustment
  #'Note that if you get to this point, you have at least one value in all OH, 
  #'MH and MF vars present (this is post-stencil application)
  #'
  
  #These are going to need to be passed in formally at a later date
  allow_lower_mh <- T
  pr_args=list(oh=list(), mh=list, mf=list())
  
  #print(pr_vars)
  for (v in pr_vars){
    #print(v)
    
    oh_present <- T
    mh_present <- T
    mf_present <- T
    
    search.fxn <- function(x, patt, ...){
      return(grepl(patt, x, ...))
    }
    oh.idx <- sapply(names(OH), search.fxn, paste0(v, "."), fixed=T)
    mh.idx <- sapply(names(MH), search.fxn, paste0(v, "."), fixed=T)
    mf.idx <- sapply(names(MF), search.fxn, paste0(v, "."), fixed=T)
    
    oh.vars <- names(OH)[oh.idx]
    mh.vars <- names(MH)[mh.idx]
    mf.vars <- names(MF)[mf.idx]
    
    oh.units <- OH.units[oh.idx]
    mh.units <- MH.units[mh.idx]
    mf.units <- MF.units[mf.idx]
    
    if (length(mh.vars) != length(mf.vars)){
      stop(paste("You should have equal numbers of mh and mf vars!"))
    }
    

    
    #Okay, so this is still supposed to take place where there is a single OH
    #var in place. So presence/absence of OH is going to be sort of binary
    if (length(oh.vars)==0){
      oh.data <- rep(0, length(MH[[mh.vars[1]]]))
      oh_present <- F
      tmp.freqadj <- F
    }else{
      oh.data <- OH[[oh.vars[1]]]
      oh_present <- T
      tmp.freqadj <- freqadj
    }
    
    for (v.idx in 1:length(mh.vars)){
      mh.var <- mh.vars[v.idx]
      mf.var <- mf.vars[v.idx]
      
      pr.units <- mf.units[v.idx]
      
      if (mf.var == prime_predictor){
        #Only the prime_var gets the ignore0 settings
        tmp <- adjustPR(oh.data, MH[[mh.var]], MF[[mf.var]], 
                        pr_trace, pr.units, tmp.freqadj, allow_lower_mh, 
                        ignore0=ignore0, conserve=conserve,
                        return_masks=T)
#         print(summary(as.vector(tmp$oh.mask)))
#         print(summary(as.vector(tmp$OH)))
#         stop('examine this output')
        if (oh_present){
          OH[[1]] <- tmp$OH
          pr_args$OH <- tmp$oh.mask
        }
        if (mh_present){
          MH[[mh.var]] <- tmp$MH
          pr_args$MH <- tmp$mh.mask
        }
        if (mf_present){
          MF[[mf.var]] <- tmp$MF
          pr_args$MF <- tmp$mf.mask
        }
      }else{
        #Other vars just get the frequency adjustment
        tmp <- adjustPR(oh.data, MH[[mh.var]], MF[[mf.var]], 
                        pr_trace, pr.units, tmp.freqadj, allow_lower_mh, 
                        ignore0=F, conserve=conserve, 
                        return_masks=F)
        if (oh_present){
          OH[[1]] <- tmp$OH
        }
        if (mh_present){
          MH[[mh.var]] <- tmp$MH
        }
        if (mf_present){
          MF[[mf.var]] <- tmp$MF
        }   
      }
    }
save(file='/home/cew/Code/i_testing/parm_pr/i_run/post3.R', list=c('OH', 'MH', 'MF'))
  }
  return(list(OH=OH, MH=MH, MF=MF, pr_args=pr_args))
}

################################################################################
conserve_pr <- function(data, index){
  #pr conservation function
  #note that there's no provision for
  #negative precip in here; you can get more negative pr
  #than previously existed this way
  dat.sum <- sum(data[index])
  dat.avg <- dat.sum/sum(!index)
  data[index] <- 0
  data[!index] <- data[!index] + data.avg
  return(dat)
}

get_pr_trace <- function(index, units){
  #'Converts a threshold to the units of the input
  #'dataset. 
  #'Requires the Udunits2 package to work. 
  #stop('this is where the errors are')
  if(!is.null(units)){
    units <- units.CF.convert(units)
    #Set options for determining what qualifies as a 'wet day'
    zero.thold = 0
    us.trace.thold = ud.convert(0.01, "inches/day", units)
    global.trace.thold = ud.convert(0.1, "mm/day", units)
  }else{
    #Null case can pop up in synthetic data and should be dealt with
    if (index=='us_trace'||index=='global_trace'){
      stop(paste("Error in MaskPRSeries: argument units was null,", 
                 "and is required for calculating index", index))
    }else{
      #Zero is only constant that needs to be set
      zero.thold=0
    }
  }
  switch(index, 
         'zero' = return(zero.thold),
         'us_trace' = return(us.trace.thold), 
         'global_trace' = return(global.trace.thold), 
         return((index))) #If own threshold supplied,
  #It is assumed that you know the units  
}

units.CF.convert <- function(unit.string){
  #Performs simple conversions needed between CF-compliant
  #units and the udunits2 package. 
  unitlist<-sub('kg m-2 s-1', "l m-2 s-1", unit.string)
}
################################################################################
