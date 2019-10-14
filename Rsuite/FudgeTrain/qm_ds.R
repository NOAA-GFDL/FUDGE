#qm_ds.R
#Contains a wrapper/setup method (callQM)
#and several quantile mapping methods
#(BCQM, CFQM, QDM)

callQM <- function(OH, MH, MF, args, type="",
                   missval.args, tmasks, train_qcmask=F){
  #'Wrapper for the single-var, single-point
  #'quantile mapping downscaling methods,
  #'which share a lot of DNA with each other.
  #'For now, even methods that can do the training once
  #'all do training within looping by ensemble.
  #'

  #Note: pr adjustments now effectively require this to be called before
  #the retry options can work properly.

  #   OH.data <- compress.xy.data(OH)
  #   MH.data <- compress.xy.data(MH)
  #   MF.data <- compress.xy.data(MF)

  #Prepare output and indexing
  out.data <- array(NA, dim=dim(MF[[1]]))
  nrips <- dim(out.data)[4]
  #Trying a workaround for the quantile mapping workflow issue -
  #The new indexing works on non-overlap cases, but the s5 examples really
  #need the overlapped data in order to reproduce past results.
  #Deleting the more complicated indexing in here and moving that indexing step to just
  #before the addition step *should* solve that particular issue.
  index.mask <- tmasks$MF$masks[[w]]
  olap <- FALSE
  if(train_qcmask){
    if(tmasks$apply_tmasks){
      qc <- mapply(rep, index.mask, nrips)
    }else{
      qc <- rep(1, dim(MF[[1]]))
    }
    dim(qc) <- dim(out.data)
  }else{
    qc <- NULL
  }
  #  MF_mask <- missval.args$pr$MF
  start_year <- 1
  for (n in 1:nrips){
    MF.slice <- lapply(MF, '[', ,,,n,, , drop=F)
    missval.new <- missval.args
    missval.new$pr_opts$MF <- missval.new$pr_opts$MF[,,,n,, , drop=F]
    missval.out <- remove.FUDGE.missvals(OH, MH, MF.slice,
                                         missval.args$minsamplesize,
                                         missval.new,
                                         missval.method='distributional')
    if(missval.out$ds.possible){
      OH.slice <- missval.out$OH[[1]]
      MH.slice <- missval.out$MH[[1]]
      MF.slice <- missval.out$MF[[1]]
      mf.missing <- sum(is.na(MF.slice))

      args$is.uniform.Oh <- length(unique(OH.slice[!is.na(OH.slice)])) <= 1
      args$is.uniform.Mh <- length(unique(MH.slice[!is.na(MH.slice)])) <= 1
      args$is.uniform.Mf <- length(unique(MF.slice[!is.na(MF.slice)])) <= 1

      train.stat <- missval.out$missval.status
      out <- switch(type,
                    #'BCQM' = callBCQMv4(OH.slice, MH.slice, MF.slice, args),
                    'BCQM' = bcqm(OH.slice, MH.slice, MF.slice, args),
                    #Note: CFQM is now called from CallDSMethod
                    'KDDM' = callKDDM(OH.slice, MH.slice, MF.slice, args),
                    'BCMM' = callBCMM(OH.slice, MH.slice, MF.slice, args)
      )
      # print(length(out))

      if (train_qcmask){
        if(type=="KDDM" & !is.null(attr(out, "uncorrectable"))){
          correctable <- rep(1, length(index.mask))
          correctable[attr(out, "uncorrectable")] <- NA
          qc[,,,n,,] <- index.qm(index.mask, correctable, olap)
          out <- as.numeric(out)
        }else if (type == 'BCMM'){
          bias <- ifelse(is.na(index.mask), NA, attr(out, 'bias'))
          qc[,,,n,,] <- index.qm(index.mask, bias, olap)
        }
      }
      
      out.data[,,,n,,] <- index.qm(index.mask, out, olap)

      if(missval.out$mv.retry & train_qcmask){
        #Double-check on pr settings for this - are there different settings
        #for retry and insufficient points?
        qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }else{
      print('downscaling not possible at this point')
      out.data[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      if(train_qcmask){
        qc[,,,n,,] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }
  }
  #stop('examine idx returned')
  return(list(ds=out.data, qc=qc))
}


cfqm <- function(Oh, Mh, Mf, args){
  DSout <- Mf + NA
  Oh <- Oh[!is.na(Oh)]
  Mh <- Mh[!is.na(Mh)]
  Mf.orig <- Mf
  Mf.nas <- is.na(Mf)
  Mf <- Mf[!(Mf.nas)]
  if (args$is.uniform.Mh || args$is.uniform.Oh){
    print(paste0(ifelse(args$is.uniform.Oh,"Oh",""), ifelse(args$is.uniform.Oh & args$is.uniform.Mh," and ",""),
                 ifelse(args$is.uniform.Mh,"Mh",""), ifelse(args$is.uniform.Oh & args$is.uniform.Mh," are"," is"),
                 " uniform. Returning Mf."))
    return(Mf.orig)
  }else{
    DSout[!Mf.nas] <- qm(Mh, Oh, Mf)
    return(DSout)
  }
}

bcqm <- function(Oh, Mh, Mf, args){
  DSout <- Mf + NA
  Oh <- Oh[!is.na(Oh)]
  Mh <- Mh[!is.na(Mh)]
  Mf.orig <- Mf
  Mf.nas <- is.na(Mf)
  Mf <- Mf[!(Mf.nas)]
  if (args$is.uniform.Mh || args$is.uniform.Oh){
    print(paste0(ifelse(args$is.uniform.Oh,"Oh",""), ifelse(args$is.uniform.Oh & args$is.uniform.Mh," and ",""),
                 ifelse(args$is.uniform.Mh,"Mh",""), ifelse(args$is.uniform.Oh & args$args$is.uniform.Mh," are"," is"),
                 " uniform. Returning Mf."))
    return(Mf.orig)
  }else{
    DSout[!Mf.nas] <- qm(Oh, Mh, Mf)
    return(DSout)
  } 
}

qm <- function(y, x, z){
  # Common code for the BCQM and CFQM functions. 
  n.max <- max(length(y), length(x))
  tau <- c(0, (1:n.max)/(n.max+1), 1)
  x <- quantile(x, tau, type=8, na.rm=TRUE)
  y <- quantile(y, tau, type=8, na.rm=TRUE)
  yout <- approx(x, y, xout=z, rule=1)$y
  extrap.lower <- is.na(yout) & (z <= min(x))
  extrap.upper <- is.na(yout) & (z >= max(x))
  yout[extrap.lower] <- min(y) + (z[extrap.lower]- min(x))
  yout[extrap.upper] <- max(y) + (z[extrap.upper]- max(x))
  return(yout)
}

callBCQMv4 <- function(Oh, Mh, Mf, args) {
  #' Modified BCQM in light of observations of QDM()
  #' This is a version of the bias-corrected quantile mapping method, updated
  #' to produce less of a step function effect with quantile windows.
  #' For more information on the general method, please consult
  #' Ho, C-K., D. B. Stephenson, M. Collins, C. A. T. Ferro, and S. J. Brown, 2012: Calibration strategies: A source of additional uncertainty in climate change projections. Bull. Amer. Meteor. Soc., 93(1), 21-26, doi:10.1175/2011BAMS3110.1.
  #' and the ESD wiki page

  DSout <- Mf + NA
  Oh <- Oh[!is.na(Oh)]
  Mh <- Mh[!is.na(Mh)]
  Mf.orig <- Mf
  Mf <- Mf[!(Mf.nas <- is.na(Mf))]

  if (!args$is.uniform.Oh & !args$is.uniform.Mh) {
    n.max <- max(length(Oh), length(Mh))
    tau <- c(0, (1:n.max)/(n.max+1), 1)
    x <- quantile(Mh, tau, type=8, na.rm=TRUE)
    y <- quantile(Oh, tau, type=8, na.rm=TRUE)
    yout <- approx(x, y, xout=Mf, rule=1)$y
    extrap.lower <- is.na(yout) & (Mf <= min(Mh))
    extrap.upper <- is.na(yout) & (Mf >= max(Mh))
    yout[extrap.lower] <- min(Oh) + (Mf[extrap.lower]- min(Mh))
    yout[extrap.upper] <- max(Oh) + (Mf[extrap.upper]- max(Mh))
    DSout[!Mf.nas] <- yout
    return(DSout)
  } else {
    print(paste0(ifelse(args$is.uniform.Oh,"Oh",""), ifelse(args$is.uniform.Oh & args$is.uniform.Mh," and ",""),
                 ifelse(args$is.uniform.Mh,"Mh",""), ifelse(args$is.uniform.Oh & args$is.uniform.Mh," are"," is"),
                 " uniform. Returning Mf."))
    return(Mf.orig)
  }
}

callBCMM <- function(Oh, Mh, Mf, args){
  #' Function for calculating and correcting model bias. 
  #' Bias is calculated between Mh and obs and subtracted from the
  #' future dataset.
  #' It's effectively delta but for the future time period.
  bias = mean(Mh, na.rm=T) - mean(Oh, na.rm=T)
  DS = Mf - bias
  attr(DS, "bias") <- bias
  return(DS)
}

set.KDDM.defaults <- function(args, ..., env=parent.frame()) {

  ## KDDM needs to know about the YEARS in each dataset, this processes the time axis and creates factors KDDM can use for its origami
  # 	Oh.time.units <- strsplit(OH$metadata[[1]]$dim$time$units, " ")[[1]][1]       # : chr "days since 1961-01-01 00:00"
  
  #Year separation isn't essential for non-precip data; 
  #for the moment, it's probably okay to ignore pr case, assume all cal-less data 
  #is tasmax
  if (OH$metadata[[1]]$dim$time$calendar != 'none'){
    Oh.time <- OH$metadata[[1]]$dim$time
    env$Oh.all.yrz <- format(get.PCICt.time.series(Oh.time$units, Oh.time$vals, Oh.time$calendar), "%Y")
    Mh.time <- MH$metadata[[1]]$dim$time
    env$Mh.all.yrz <- format(get.PCICt.time.series(Mh.time$units, Mh.time$vals, Mh.time$calendar), "%Y")
    Mf.time <- MF$metadata[[1]]$dim$time
    MF.all.yrz <- format(get.PCICt.time.series(Mf.time$units, Mf.time$vals, Mf.time$calendar), "%Y")
    env$Mf.all.yrz <- MF.all.yrz
    env$mf.yr.length <- median(table(MF.all.yrz))
  }else{
    #This can describe the csv case; run with synthetic data
    env$Oh.all.yrz <- rep(1000, length(OH$metadata[[1]]$dim$time$values))
    env$Mh.all.yrz <- rep(1000, length(MH$metadata[[1]]$dim$time$values))
    env$Mf.all.yrz <- rep(1000, length(MF$metadata[[1]]$dim$time$values))
    env$mf.yr.length <- length(MF$metadata[[1]]$dim$time$values)
  }

  if (!("kdetype" %in% names(args))){
    env$ds.ref$args$kdetype <- "KernSmooth::bkde"
  }else{
    if (args$kdetype=="bkde"){
      env$ds.ref$args$kdetype <- "KernSmooth::bkde"
    }
  }

  return(env)
}

callKDDM <- function(Oh, Mh, Mf, args){
  #'Bias correction of climate model output using KDDM on a single variable
  #'from the 'climod' package https://github.com/sethmcg/climod
  #'@param Oh: Observed Historical (a.k.a. observations, obs)
    #'@param Mh: Model Historical (a.k.a. GCM historical, cur)
    #'@param Mf: Model Future (a.k.a GCM future, fut)
    #'@param norm: Choice of normalization technique. Contained in args parameter.
    #'Contains no other arguments in the args parameter.
    #' Not valid for pr variables or julian calendar. When updated, remove check in line 351 of MasterDriver.R

    if (!(args$norm %in% c("boxcox", "zscore", "none"))) {
      stop(paste('KDDM norm must be one of {"zscore", "boxcox", "none"}'))
    }
    # Prepare Downscaled output array the same as Mf
    DSout <- Mf

    if (!args$is.uniform.Oh & !args$is.uniform.Mh ) {
      # Prepare Downscaled output array of NAs the same dimension as Mf
      DSout <- DSout + NA
      if (tmasks$apply_tmasks) {
        oh.mask <- !is.na(tmasks$OH$masks[[w]])
        mh.mask <- !is.na(tmasks$MH$masks[[w]])
        mf.mask <- !is.na(tmasks$MF$masks[[w]])
      } else {
        oh.mask <- rep(TRUE, length(Oh))
        mh.mask <- rep(TRUE, length(Mh))
        mf.mask <- rep(TRUE, length(Mf))
      }
      # Subset to remove the masked NAs
      subset.OH <- as.vector(Oh)[oh.mask]
      subset.MH <- as.vector(Mh)[mh.mask]
      subset.MF <- as.vector(Mf)[mf.mask]

      pad <- 0
      # CHECK for Dec/Jan overlap implying need for padding
      # Create relative window-width indices
      #       pad1.relative.indices <- (1:half.window)-half.window
      #       pad2.relative.indices <- 1:(half.window-1)
      #       #Check those indices for MH and MF masks on each side
      #       pad1 <- max(sum(mh.mask[mf.yr.length + pad1.relative.indices]), sum(mf.mask[mf.yr.length + pad1.relative.indices]))
      #       pad2 <- max(sum(mh.mask[mf.yr.length + pad2.relative.indices]), sum(mf.mask[mf.yr.length + pad1.relative.indices]))

      # otherwise, the smaller side should be padded
      #       pad <- abs(c(pad1, pad2))[padside <- which.max(c(pad1, pad2))]
      #           message(paste("Adjusting for 31DEC/1JAN overlap.",
      #                         ifelse(padside==2,"Left","Right"),
      #                         "side padded with",pad,"NAs"))

      ####################
      ## Need a better way to KNOW window width
      ####################
      # Find the time window width
      #         window.width <- sum(mf.mask[1:mf.yr.length])
      window.width <- max(sum(mh.mask[1:mf.yr.length]), sum(mf.mask[1:mf.yr.length]))
      ####################
#       print(paste("window width ", window.width, "and w ", w))
      # Find the width of a half of the time window, to determine closeness to Dec31/Jan1 boundary.
      half.window <- ceiling(window.width/2)

      if (window.width != mf.yr.length) {
        mh.jan.check <- sum(mh.mask[1:window.width])
        mh.dec.check <- sum(mh.mask[(mf.yr.length - window.width):mf.yr.length])
        if (mh.jan.check > 0 & mh.dec.check > 0) {
          pad <- min(mh.jan.check, mh.dec.check)
          padside <- which.min(c(mh.jan.check, mh.dec.check))
        }

        mf.jan.check <- sum(mf.mask[1:window.width])
        mf.dec.check <- sum(mf.mask[(mf.yr.length - window.width):mf.yr.length])
        if (mf.jan.check > 0 & mf.dec.check > 0) {
          pad <- min(mf.jan.check, mf.dec.check)
          padside <- which.min(c(mf.jan.check, mf.dec.check))
        }
      }

      if (target.var != "pr") { # then we have to fold it up and deal with Dec31/Jan1 overlap
        bctrunc <- FALSE
        bctrim  <- NULL
        densfun <- eval(parse(text=args$kdetype))

        Oh.yrz <- Oh.all.yrz[oh.mask]
        Mh.yrz <- Mh.all.yrz[mh.mask]
        Mf.yrz <- Mf.all.yrz[mf.mask]


        # Initialize a padding variable to shift December data across the boundary if necessary.
        # If the first or last NAs are the first or last array entry, then mask does not wrap.
        #         ####################
        #         if ( (pad1 <- min(which(!mf.mask)) - 1) == 0 |
        #                (pad2 <- length(mf.mask) - max(which(!mf.mask))) == 0 |
        #                window.width == mf.yr.length) {
        #           ####################
        # if ( (pad2 <- w - half.window) >= 0  |
        #        (pad1 <- w + half.window - mf.yr.length) >= 0 |
        #        window.width == mf.yr.length ) {

        if ( pad == 0 ) {
          # If pad is 0 then Dec31/Jan1 boundary is not an issue, fold vector into lists of window.width length
          folded.OH <- tapply(subset.OH, Oh.yrz, list)
          folded.MH <- tapply(subset.MH, Mh.yrz, list)
          folded.MF <- tapply(subset.MF, Mf.yrz, list)
          #           Oh.yrz <<- Oh.yrz
#           print(paste("pad is", pad))
#           print(paste("the length of Oh.yrz is", length(unlist(Oh.yrz)), "for",ival,jval))
#           print(paste("the length of subset.OH is", length(unlist(subset.OH)), "for",ival,jval))

        } else {
          NApads <- as.factor(rep(NA,pad))
          # If window is beginning of year...
          if (padside == 1) {
            Oh.yrz <- unlist(list(NApads, Oh.yrz[-c((length(Oh.yrz)-pad+1):length(Oh.yrz))]))
            Mh.yrz <- unlist(list(NApads, Mh.yrz[-c((length(Mh.yrz)-pad+1):length(Mh.yrz))]))
            Mf.yrz <- unlist(list(NApads, Mf.yrz[-c((length(Mf.yrz)-pad+1):length(Mf.yrz))]))
          } else {
            # otherwise window is end of year
            Oh.yrz <- unlist(list(Oh.yrz[-c(1:pad)], NApads))
            Mh.yrz <- unlist(list(Mh.yrz[-c(1:pad)], NApads))
            Mf.yrz <- unlist(list(Mf.yrz[-c(1:pad)], NApads))
          }
          #           Oh.yrz <<- Oh.yrz
#           print(paste("pad is", pad))
#           print(paste("padside is", padside))
#           print(paste("the length of Oh.yrz is", length(unlist(Oh.yrz)), "for",ival,jval))
#           print(paste("the length of subset.OH is", length(unlist(subset.OH)), "for",ival,jval))

          folded.OH <- tapply(subset.OH, Oh.yrz, list)
          folded.MH <- tapply(subset.MH, Mh.yrz, list)
          folded.MF <- tapply(subset.MF, Mf.yrz, list)
        }
      } else {
        # var is precip and do not fold
        folded.OH <- list(subset.OH)
        folded.MH <- list(subset.MH)
        folded.MF <- list(subset.MF)

        # truncate and trim using lof1d
        bctrunc <- TRUE
        #         bctrim  <- chauvenet
        #         bctrim  <- lof1d
        # using arg passed in, per Seth McGinnis, this should be 'ptrim'
        bctrim  <- eval(parse(text=args$bctrim))
        densfun <- eval(parse(text=args$kdetype))
      }
      # Bias Correct using the climod KDDM routine, return the unlisted array of future values
      # NOTE: Using GFDL modfified KDDM biascorrect() function.  When NCAR updates, this can be changed back.
      which.norm <- ifelse(args$norm=="none", "identity", args$norm)

      kddm.bc.out <- unlist(gfdl.kddm.biascorrect(list(obs=folded.OH, cur=folded.MH, fut=folded.MF), norm=which.norm, truncate=bctrunc, trim=bctrim, densfun=densfun)$fut)

      # If pad was increased to shift data, shift it back
      if(pad != 0) {
        if ( padside == 2 ) {
          kddm.bc.out <- c(kddm.bc.out, rep(NA,pad))
        } else {
          kddm.bc.out <- c(rep(NA,pad), kddm.bc.out)
        }
      }
      # Write the bias corrected results into the DSout array in the unmasked indices and return.
      DSout[mf.mask] <- kddm.bc.out

      if (!is.null(attr(kddm.bc.out,"uncorrectable"))) {
        DS.out.uncorrectable <- rep(FALSE, length.out=length(DSout))
        DS.out.uncorrectable[mf.mask] <- attr(kddm.bc.out,"uncorrectable")
        attr(DSout,"uncorrectable") <- DS.out.uncorrectable
      }
    } else {
      print(paste0(ifelse(args$is.uniform.Oh,"Oh",""), ifelse(args$is.uniform.Oh & args$is.uniform.Mh," and ",""),
                   ifelse(args$is.uniform.Mh,"Mh",""), ifelse(args$is.uniform.Oh & args$is.uniform.Mh," are"," is"),
                   " uniform. Returning Mf."))
    }

    return(DSout)
}



# NOTE: GFDL modfified KDDM biascorrect() function includes "na.rm=TRUE" fix for missing ESTCP December bug.
#       When NCAR updates, this can be deleted
gfdl.kddm.biascorrect <- function (bcdata, norm = "zscore", minobs = 10, dmap = FALSE, ...)
{
  if (sum(is.finite(unlist(bcdata$obs))) < minobs | sum(is.finite(unlist(bcdata$cur))) <
        minobs) {
    unfixable <- function(x) {
      x@uncorrectable <- is.finite(x)
      #       x + NA
      #####  BIG CHANGE HERE, from what McGinnis probably intended to what we normally do.
      x + 0
    }
    unfix.out <- rapply(bcdata, unfixable, how = "replace")
    return(unfix.out)
  }
  if (norm == "boxcox") {
    gamma <- megamma(unlist(bcdata, use.names = FALSE))
    nbcd <- rapply(bcdata, normalize, how = "replace", norm = norm,
                   gamma = gamma)
  }
  else {
    nbcd <- rapply(bcdata, normalize, how = "replace", norm = norm)
  }
  mapping <- distmap.new(unlist(nbcd$cur), unlist(nbcd$obs), ...)
  fixed <- rapply(nbcd, function(x) {
    predict(mapping, x)
  }, how = "replace")
  adj <- rapply(fixed, attributes, how = "replace")
  adj <- lapply(adj, renest)
  adj <- lapply(adj, function(x) {
    lapply(x, function(y) {
      unlist(y)
    })
  })
  adj <- lapply(adj, function(x) {
    x$norm <- NULL
    x
  })
  ## FIX for NA/NaN here...
  # adj <- rapply(adj, mean, how = "replace")
  adj <- rapply(adj, mean, how = "replace", na.rm=TRUE)
  adj <- renest(adj)
  shift <- NA
  scale <- NA
  pscale <- NA
  if (norm == "zscore") {
    shift <- adj$mu$obs - adj$mu$cur
    scale <- adj$sigma$obs/adj$sigma$cur
  }
  if (norm == "boxcox") {
    shift <- 0
    pscale <- 1
  }
  result <- list()
  result$obs <- bcdata$obs
  result$cur <- rapply(fixed$cur, denormalize, how = "replace",
                       shift = shift, scale = scale, pscale = pscale)
  result$fut <- rapply(fixed$fut, denormalize, how = "replace",
                       shift = shift, scale = scale, pscale = pscale)
  if (dmap) {
    result$distmap <- mapping
  }
  return(result)
}
### BELOW: Can be removed when new version of climod package is installed -djas 2018-01-03


##' Trims the top percentile of values from a vector.
##'
##' Because precipitation is heavy-tailed, the process of
##' constructing a distribution mapping (a \code{\link{distmap}}
##' object) for precipitation tends to overfit the upper tails.  To
##' avoid this problem, it is useful in this case to trim extreme
##' values from the input datasets. This function discards the top
##' \code{p}% of values from the input vector.
##'
##' This function uses the \code{stats::quantile} function to find a
##' cutoff value.  Because any quantile with probability less than 1
##' is always less than the maximum value, the function will always
##' trim the maximum value from x.
##'
##' @param x a vector of values.
##'
##' @param p the percentage of values to discard.  Defaults to 1%.
##'
##' @param ... further arguments passed on to \code{stats::quantile}
##'
##' @return The input vector \code{x} with the top \code{p} percent
##' of values (minimum 1) removed.
##'
##' @examples
##'
##' set.seed(222)
##' x <- c(rnorm(20), 2, 5, 10, 50, 100)
##' print(x)
##' print(ptrim(x, p=0.10))
##' print(quantile(x, p=0.9))
##' @export

ptrim <- function(x, p=0.01, ...){
  q <- quantile(x, probs=(1-p), ...)
  x[x < q]
}



##' Trims extreme values from a vector based on the local outlier
##' factor, simplified for the one-dimensional case.
##'
##' The local outlier factor (LOF) compares the local density of a
##' value to the average local density of its k nearest neighbors.  If
##' the ratio of the densities (the LOF) is significantly higher than
##' 1, the point is an outlier and can be discarded.  This function
##' uses a highly simplified calculation to trim outlier values from
##' the upper tail of 1-dimensional data.
##'
##' If length(x) is less than 3*k, the vector is returned unchanged.
##' See the Wikipedia page for a description of the LOF algorithm:
##' \url{https://en.wikipedia.org/wiki/Local_outlier_factor}
##'
##' @param x A vector of values.
##'
##' @param k The number of nearest neighbors to consider.  Also the
##' number of upper tail values considered as possible outliers.
##'
##' @param cutoff The maximum LOF for a value to be retained.
##'
##' @return The input vector \code{x} with any of the \code{k} largest
##' values that have an LOF greater than \code{cutoff} removed.
##'
##' @examples
##'
##' set.seed(222)
##' x <- c(-5, rnorm(20), 2, 5, 10, 50, 100)
##' print(x)
##' print(lof1d(x))
##'
##' @export
lof1d <- function(x, k=5, cutoff=3){

  if(length(x) < 3*k){warning("lof1d: too few values to calculate 1-D LOF"); return(x)}

  ## To look at the top k values, we need the k nearest neighbors of the
  ## kth value, and then the k nearest neighbors of each of those, so we
  ## might need the 3*kth value.

  ox <- order(x, decreasing=TRUE)[1:(3*k)]
  y <- x[ox]

  ## Find the k nearest neighbors for each value and their indices
  ## Note: k+1 index because A is an element of y, so first neighbor is self

  knni <- lapply(y, function(A){ order(abs(y-A))[(1:k)+1]})
  knn  <- lapply(knni, function(i){y[i]})

  ## K-distance = distance from A to its k-th nearest neighbor
  kdist <- abs(y-sapply(knn, `[`, k))

  ## Reachability distance of A from B = dist from A to B, but at least kd(B).
  ## Note!  This funciton operates on indices, not values
  rdist <- function(i,j){max(abs(y[i]-y[j]), kdist[j])}

  ## Local reachability density = inverse of avg rdist(A) *from* its knn
  N <- length(y)
  lrd <- sapply(seq(N), function(i){
    1/mean(sapply(knni[i], function(j){rdist(i,j)}))
  })

  ## Local outlier factor = avg knn lrd / own lrd
  lof <- sapply(1:N, function(i){mean(lrd[knni[[i]]]) / lrd[i]})

  ## trim values with LOF > cutoff

  if(any(lof > cutoff)){
    z <- x[-ox[lof > cutoff]]
  } else {
    z <- x
  }

  return(z)

}

chauvenet <- function(x, upper=TRUE, lower=TRUE){

  n <- length(x) + 1/2
  mu <- median(x)
  sig <- mad(x)

  if(lower){ x <- x[!(x < qnorm(1/n, mu, sig))] }
  if(upper){ x <- x[!(x > qnorm(1-1/n, mu, sig))] }

  return(x)
}

distmap.new <- function(x, y, densfun = KernSmooth::bkde,
                        pgrid = ppoints(1000), truncate = FALSE, trim = NULL,
                        na.rm = TRUE, ...){
  if(na.rm){
    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
  }
  stopifnot(is.atomic(x))
  stopifnot(all(is.finite(x)))
  stopifnot(is.atomic(y))
  stopifnot(all(is.finite(y)))
  stopifnot(is.atomic(pgrid))
  stopifnot(all(is.finite(pgrid)))
  stopifnot(all(pgrid >= 0))
  stopifnot(all(pgrid <= 1))
  stopifnot(all(diff(pgrid) > 0))

  ############
  if(!(is.null(trim))){
    x <- trim(x)
    y <- trim(y)
  }
  ############

  xpdf <- densfun(x, ...)
  ypdf <- densfun(y, ...)
  if (truncate){
    xpdf$y[xpdf$x < min(x)] <- 0
    ypdf$y[ypdf$x < min(y)] <- 0
  }
  xcdf <- pdf2cdf(xpdf)
  ycdf <- pdf2cdf(ypdf)
  xq <- splinefun(xcdf$y, xcdf$x, method = "monoH.FC")(pgrid)
  yq <- splinefun(ycdf$y, ycdf$x, method = "monoH.FC")(pgrid)
  transfer <- splinefun(xq, yq, method = "monoH.FC")
  result <- namelist(x, y, pgrid, xpdf, ypdf, xq, yq, transfer)
  class(result) <- "distmap"
  return(result)
}

##### ABOVE ### Can be removed when new version of climod package is installed -djas 2018-01-03

callBCQMv2<-function(LH,CH,CF,args){
  #'Performs an bias-correction quantile mapping on a single variable
  #'over the historical period, in order to generate downscaled data.
  #' @param LH: Local Historical (a.k.a. observations)
     #' @param CH: Coarse Historical (a.k.a. GCM historical)
     #' @param CF: Coarse Future (a.k.a GCM future)
     #' Contains no other arguments in the args parameter.
     #'
     #' Note: Even though there is no training step on this method, and no
     #' intention of implementing multivar downscaling for the QM methods,
     #' they need to be named to fit into the new indexing scheme.

     #Unlist data from its input form
     #     CH <- unlist(CH)
     lengthCH<-length(CH)
     lengthLH<-length(LH)
     lengthCF<-length(CF)

     # if (lengthCF>lengthCH) maxdim=lengthCF else maxdim=lengthCH
     maxdim=max(c(lengthCH, lengthCF, lengthLH))

     # first define vector with probabilities [0,1]
     #previously, prob was lengthCF
     cf.present <- sum(!is.na(CF))
     prob<-seq(0.001,0.999,length.out=cf.present)

     # initialize data.frame
     #index used ot be 1:lengthCF; doing 1:maxdim instead
     #c(seq(1,lengthCF), rep(NA,maxdim-lengthCF))
     if(cf.present != 0){
       temp<-data.frame(index=1:maxdim,CF=rep(NA,maxdim),CH=rep(NA,maxdim),LH=rep(NA,maxdim),
                        qCF=rep(NA,maxdim),ecdfCHqCF=rep(NA,maxdim),qLHecdfCHqCF=rep(NA,maxdim))
       temp$CF[1:lengthCF]<-CF
       temp$CH[1:lengthCH]<-CH
       temp$LH[1:lengthLH]<-LH #THIS is the line that's throwing things off
       temp.CFsorted<-temp[order(temp$CF),]
       temp.CFsorted$qCF[1:cf.present]<-quantile(temp.CFsorted$CF,prob,na.rm =TRUE)
       temp.CFsorted$ecdfCHqCF[1:cf.present]<-ecdf(temp$CH)(quantile(temp$CF,prob,na.rm =TRUE))
       temp.CFsorted$qLHecdfCHqCF[1:cf.present]<-quantile(temp$LH,ecdf(temp$CH)(quantile(temp$CF,prob,na.rm =T)),
                                                          na.rm =T)
       temp<-temp.CFsorted[order(temp.CFsorted$index),]
       return(temp$qLHecdfCHqCF[1:lengthCF])
     }else{
       print("No present values in CF; returning all NA")
       return(CF)
     }
}
