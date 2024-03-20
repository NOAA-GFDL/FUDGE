#'Wrapper function for the FUDGE calls to MBC and QDM,
#'since both use the same package and there is similar
#'argument checking between the two of them

library("rlang")
callMBC <- function(OH, MH, MF, args, ds.method = "", tmasks, train_qcmask = FALSE) {
  if (args$force_reproducibility) {
    # i_idx, j_idx, i_offset and j_offset are passed in from the command-line.
    # This is meant to enforce reproducibility when we have alternative
    # subsetting in place. -cew
    # seedval <- ( 31 * (ival + i_idx) ) + ( 29 * (jval + j_idx) )
    # set.seed(seedval)
  }
  # Create output data
  out.data <- array(NA, dim = dim(MF[[1]]))
  nrips <- dim(out.data)[4]
  index.mask <- tmasks$MF$masks[[w]]
  olap <- FALSE
  if (train_qcmask) {
    if (tmasks$apply_tmasks) {
      qc <- mapply(rep, index.mask, nrips)
    } else {
      qc <- rep(1, dim(MF[[1]]))
    }
    dim(qc) <- dim(out.data)
  } else {
    qc <- NULL
  }
  MF_mask <- missval.args$pr_opts$MF
  tmp.missvals <- missval.args
  for (n in 1:nrips) {
    nval <<- n
    MF.slice <- lapply(MF, "[", ,,,n,, , drop = FALSE)
    # pr-specific check for now; only do if present
    missval.args$pr_opts$MF <- MF_mask[,,,n,, ,drop = FALSE]
    missval.out <- remove.FUDGE.missvals(OH, MH, MF.slice,
                                         missval.args$minsamplesize,
                                         missval.args,
                                         missval.method = "distributional")

    if (args$adjust_trace) {
      adj.thold <- missval.args$pr_opts$adjusted.thold
      if (args$transform) {
        if (is.null(args$tform_fxn[[2]])) {
          adj.thold <- do.call(args$tform_fxn[[1]], list(adj.thold))
        } else {
          adj.thold <- do.call(args$tform_fxn[[1]], list(adj.thold, args$tform_fxn[[2]]))
        }
      }
      if (args$trace > adj.thold) {
        args$trace <- adj.thold
      }
    }

    if (missval.out$ds.possible) {
      out <- tryCatch({callMBC_any(missval.out$OH, missval.out$MH, missval.out$MF,
                                   args$variant, missval.out$complete.hist,
                                   missval.out$complete.fut, args = args)},
                      error = function(e) {
                        is_known <- grepl("need at least two non-NA values to interpolate",
                                          e$message)
                        if (is_known) {
                          message(paste("Warning in callQDM: QDM did not have",
                                        "enough unique values in input datasets",
                                        "to interpolate!"))
                          message(paste("Returning MF unchanged."))
                          return(MF.slice[[1]])
                        } else {
                          print(e)
                          #print(traceback())
                          stop("Unknown error in QDM: Please examine outputs!")
                        }
                        stop("examine error output")
                      })

      #... the indexing on this function is going to be a pain.
      #I sort of think that we have to do synchronous missing values
      #and that the synch missing values also need some sort of master
      #OH and MH vector for missing points.
      #We also CAN'T use the pr masking functions with this method, I think -cew
      # out.data[,,,n,,!is.na(index.mask)] <- index.qm(index.mask, out, olap)
      loop.mask <- index.mask
      loop.mask[is.na(MF.slice[[1]])] <- NA
      if (missval.out$mv.retry) {
        # We need to limit out to those values for which MF has data
        out <- apply.fudge.tmask(data = out, mask = loop.mask)
      }
      print(length(out.data[,,,n,,]))
      print(length(out))
      out.data[, , , n, , !is.na(index.mask)] <- index.qm(index.mask, out, olap)
      if (train_qcmask & missval.out$mv.retry) {
        print("adding the pr retry mask")
        qc[, , , n, , !is.na(index.mask)] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    } else {
      print("downscaling not possible at this point")
      out.data[ , , , n, , !is.na(index.mask)] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      if (train_qcmask) {
        qc[, , , n, , !is.na(index.mask)] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }
  }
  return(list(ds = out.data, qc = qc))
}




callMBC_any <- function(OH, MH, MF, variant, complete.hist, complete.fut, args) {
  #Okay...this is the step when any x.y.vars which are all missing get removed.
  #This is then going to have to be applied to the same list of vars that -cew
  OH.slice <- as.matrix(as.data.frame(lapply(OH, function(x, y) {return(x[y])},
                                             complete.hist)))
  MH.slice <- as.matrix(as.data.frame(lapply(MH, function(x, y) {return(x[y])},
                                             complete.hist)))
  MF.slice <- as.matrix(as.data.frame(lapply(MF, function(x, y) {return(x[y])},
                                             complete.fut)))


  if (args$variant == "spearman") {
    mbc_any_out <- MBC::MBCr(OH.slice, MH.slice, MF.slice,
                            iter = args$iter, jitter.factor = args$jitter.factor,
                            ratio.seq = args$ratio.seq, trace = args$trace,
                            ties = args$ties, silent = TRUE)
  } else if (args$variant == "pearson") {
    mbc_any_out <- MBC::MBCp(OH.slice, MH.slice, MF.slice,
                            iter = args$iter, jitter.factor = args$jitter.factor,
                            ratio.seq = args$ratio.seq, trace = args$trace,
                            ties = args$ties, silent = TRUE)
  } else if (args$variant == "all") {
    mbc_any_out <- MBC::MBCn(OH.slice, MH.slice, MF.slice,
                            iter = args$iter, jitter.factor = args$jitter.factor,
                            ratio.seq = args$ratio.seq, trace = args$trace,
                            ties = args$ties, silent = TRUE)
  } else {
    stop(paste("Error in callMBC_any: variant was not one of",
             "pearson (MBCp), spearman (MBCr), or all (MBCn)!"))
  }
  #Need to remove vars that aren't target var at this step
  #Oh[[1]] should still work, even for multi-point target
  #...also need to do more advanced checking of missing values.
  #Need to add back in (from the MF) any values that we could not
  #downscale over/are NA -cew
  result <- mbc_any_out$mhat.p[, 1]
  print(dim(MF.slice))
  print(length(result))
  return(result)
}

set.MBC.defaults <- function(args, predictor.vars, ..., env = parent.frame()) {
  #'Sets common defaults for the inputs to MBC that we want
  #'to control. Most of these also apply to QDM.
  # moving the seed setting here to attempt to only do it once per I,J
  # not in every time window -d1a
  seedval <- (31 * (ival + i_idx)) + (29 * (jval + j_idx))
  set.seed(seedval)

  iter <- 30
  jitter.fac <- 0
  ratio.seq <- rep(FALSE, length(predictor.vars))

  # -------------------------------
  # defaults in QDM needed for MBC?
  # -------------------------------
  is_jitter <- FALSE
  jitter_val <- 0
  adjust_trace <- FALSE
  transform <- FALSE
  tform_fxn <- "none"
  screen_NaNs <- FALSE
  # -------------------------------
  # -------------------------------

  #This is going to have implications for the multi-point downscaling.
  #Need access to npts_by in order to expand this list...
  #or really just need 1; multivar-targets are going to have that effect. -cew
  trace <- 0.05
  ties <- "first"
  #Trace is going to be in the same notation as our pr units adjuster.
  #It'll work, but we need to have an idea of which vars are getting a unit
  #conversion.
  #...I'm pretty sure those are globals anyway; we can check. -cew
  force_reproducibility <- args$force_reproducibility
  if ("trace" %in% names(args)) {
    non.numeric.trace <- is.na(suppressWarnings(as.numeric(args$trace)))
    #if trace is non-numeric (i.e an arg we'd pass to the pr functions
    #get OH pr units from adjust.metadata (created back in main driver) -cew
    if (non.numeric.trace) {
      pr.units <- adjust.metadata$pr_oh_units
      if (length(pr.units)==0) {
        stop(paste("Error in set.MBC.defaults: no pr vars found in OH!"))
      }
      pr.trace <- convert.pr.threshold(args$trace, pr.units)
      trace <- pr.trace
    } else {
      trace <- as.numeric(args$trace)
    }
    if ("transform" %in% names(args)) {
      tform <- return.t.fxn(args$transform, args$transform_arg)
      if (is.null(tform[[2]])) {
        trace <- do.call(tform[[1]], list(trace))
      } else {
        trace <- do.call(tform[[1]], list(trace, tform[[2]]))
      }
    }
  }
  rat.names <- c("ratio", "ratio.seq", "ratio_seq")
  if (any(rat.names %in% names(args))) {
    rat.name <- rat.names[which(rat.names %in% names(args))]
    in.rat.seq <- unlist(strsplit(args[[rat.name]], ","))
    #...oh_by is going to blow this entire bit up hard at some point in the
    #future. I'm not sure I can get a good check going without it, though.
    #...it's also going to blow up when we get to using time-varying amplitudes
    #in here - need to make sure that only one val gets used for those.
    ##Okay - as of 2-14-17, adding this in two steps: one for the -cew
    #     in.rat.seq <- sapply(in.rat.seq, function(x,r) {rep(x,r)}, oh_by**2)
    #     ratio.seq <- in.rat.seq
    if (length(in.rat.seq) != length(ratio.seq)) {
      stop(paste("Error in set.MBC.defaults: string", args$ratio.seq,
                 "did not have as many elements as MH has vars!"))
    }
    ratio.seq <- in.rat.seq
  }

  jitter.names <- c("jitter", "jitter.fac", "jitter_fac")
  if (any(jitter.names %in% names(args))) {
    jitter.name <- jitter.names[which(jitter.names %in% names(args))]
    jitter.fac <- args[[jitter.name]]
  }

  return.args <- list(iter = iter,
                      jitter.factor = jitter.fac,
                      ratio.seq = ratio.seq,
                      trace = trace,
                      ties = ties,
                      variant = args$variant,
                      var.names = unlist(predictor.vars),
                      # is_jitter <- FALSE
                      # jitter_val <- NULL
                      adjust_trace = adjust_trace,
                      # transform <- FALSE
                      # tform_fxn <- "none"
                      # screen_NaNs <- FALSE
                      force_reproducibility = force_reproducibility)
  # env$ds.args <- return.args
  env$ds.ref$args <- return.args
  return(env)
}

#
# set.QDM.defaults <- function(args, predictor.vars, ..., env = parent.frame()) {
#   #'Sets common defaults for the inputs to MBC that we want
#   #'to control. Most of these also apply to QDM.
#   iter <- 30
#   is_jitter <- FALSE
#   jitter_val <- NULL
#   jitter.fac <- 0
#   ratio.seq <- FALSE #should be 1 for this case
#   trace <- 0.05
#   adjust_trace <- FALSE
#   transform <- FALSE
#   tform_fxn <- "none"
#   ties <- "first"
#   force_reproducibility <- FALSE
#   screen_NaNs <- FALSE
#   if ("trace" %in% names(args)) {
#     non.numeric.trace <- is.na(suppressWarnings(as.numeric(args$trace)))
#     #if trace is non-numeric (i.e an arg we'd pass to the pr functions
#     #get OH pr units from adjust.metadata (created back in main driver) -cew
#     if (non.numeric.trace) {
#       pr.units <- adjust.metadata$pr_oh_units
#       if (length(pr.units)==0) {
#         stop(paste("Error in set.QDM.defaults: no pr vars found in OH!"))
#       }
#       pr.trace <- convert.pr.threshold(args$trace, pr.units)
#       trace <- pr.trace
#     } else {
#       trace <- as.numeric(args$trace)
#     }
#     #Now check the section 5 args for the
#     if (exists("pre.ds.train")) {
#       pre.ds.types <- sapply(pre.ds.train, "[[", "type")
#       if ("transform" %in% pre.ds.types) {
#         transform <- T
#         tform.idx <- which(pre.ds.types=="transform")
#         tform_fxn <- return.t.fxn(pre.ds.train[[tform.idx]]$qc_args$transform,
#                               pre.ds.train[[tform.idx]]$qc_args$arg)
#         if (is.null(tform_fxn[[2]])) {
#           trace <- do.call(tform_fxn[[1]], list(trace))
#         } else {
#           trace <- do.call(tform_fxn[[1]], list(trace, tform_fxn[[2]]))
#         }
#       }
#     }
#   }
#   rat.names <- c("ratio", "ratio.seq", "ratio_seq")
#   if ("type" %in% names(args)) {
#     if (args$type=="add") {
#       ratio.seq <- FALSE
#     }else if (args$type=="mult") {
#       ratio.seq <- TRUE
#     }
#   }
#   #behavior changed 2022-10 - explicitly exit with error if using old args -cew
#   jitter.old.names <- c("jitter", "jitter.fac", "jitter_fac")
#   if (any(jitter.old.names %in% names(args))) {
#     stop(paste("Error in set.qdm.defaults: QDM requires specification of is_jitter and possibly jitter_val!"))
#   } else {
#     if ("is_jitter" %in% names(args)) {
#       if (args$is_jitter) {
#         jitter.fac <- args$jitter_val
#         if (args$jitter_val == "default") {
#           jitter.fac <- .Machine$double.eps^2
#         } else {
#           jitter.fac <- as.numeric(args$jitter_val)
#         }
#       } else {
#         if (!is.null(jitter_val) | ("jitter_val" %in% names(args)) ) {
#           stop(paste("Error in set.qdm.defaults: when is_jitter = FALSE, cannot set jitter_val in config!"))
#         } else {
#           jitter.fac <- 0
#         }
#       }
#     }
#   }
#   if ("adjust_trace" %in% names(args)) {
#     adjust_trace <- ifelse(args$adjust_trace=="on", T, F)
#   }
#   if ("force_reproducibility" %in% names(args)) {
#     if (args$force_reproducibility %in% c("on", "off", TRUE, FALSE)) {
#       if (args$force_reproducibility %in% c("on", TRUE)) {
#         force_reproducibility <- TRUE
#       } else {
#         force_reproducibility <- FALSE
#       }
#     } else {
#       stop(paste("Error in set.QDM.defautls: force_reproducibility should be one of",
#                  paste(c("on", "off", TRUE, FALSE), collapse=","), "but was",
#                  args$force_reproducibility, "!"))
#     }
#   } else {
#     if (args$type == "mult" && jitter.fac != 0) {
#       stop(paste("Error in set.QDM.defaults: as of October 2022,",
#                  "force_reproducibility argument ('on' or 'off')",
#                  "is required to be set in the JSON for QDM when type == 'mult'",
#                  "and is_jitter is true!"))
#     } else {
#       #default changed 2022-10 - was false, is now true
#       force_reproducibility <- TRUE
#     }
#   }
#   #Put together QDM-specific args - but keep trace_calc default of 0.5 * trace
#   print(paste("jitter.fac: ", jitter.fac))
#   return.args <- list(iter=iter, jitter.factor=jitter.fac, ratio.seq=ratio.seq, trace=trace, ties=ties,
#                       var.names=unlist(predictor.vars),
#                       adjust_trace=adjust_trace, transform=transform, tform_fxn = tform_fxn,
#                       force_reproducibility = force_reproducibility)
#   if ("qdm_mult_noise" %in% names(args)) {
#     if (args$qdm_mult_noise == "off") {
#       return.args$trace.calc <- -99 # setting to 0 can still hit eps. compares
#       if (ratio.seq) {
#         screen_NaNs <- TRUE
#       }
#     }
#   }
#   return.args$screen_NaNs = screen_NaNs
#   #Deal with sample size
#   if ("minsamplesize" %in% names(args)) {
#     return.args$minsamplesize <- args$minsamplesize
#   }
#   env$ds.ref$args <- return.args
#   return(env)
# }

