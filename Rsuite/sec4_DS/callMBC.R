#'Wrapper function for the FUDGE calls to MBC and QDM,
#'since both use the same package and there is similar
#'argument checking between the two of them

library("rlang")

callMBC <- function(OH, MH, MF, args, ds.method = "", tmasks, train_qcmask = FALSE) {
  if (args$force_reproducibility & w == 1) {
    # i_idx, j_idx, i_offset and j_offset are passed in from the command-line.
    # This is meant to enforce reproducibility when we have alternative
    # subsetting in place. -cew
    set.seed((31 * (ival + i_idx)) + (29 * (jval + j_idx)))
  }
  # Create output data
  out_data <- array(NA, dim = dim(MF[[1]]))
  nrips <- dim(out_data)[4]
  index.mask <- tmasks$MF$masks[[w]]
  olap <- FALSE
  if (train_qcmask) {
    if (tmasks$apply_tmasks) {
      qc <- mapply(rep, index.mask, nrips)
    } else {
      qc <- rep(1, dim(MF[[1]]))
    }
    dim(qc) <- dim(out_data)
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
      out <- tryCatch({callMBCvariant(missval.out$OH, missval.out$MH, missval.out$MF,
                                   args$variant, missval.out$complete.hist,
                                   missval.out$complete.fut, args = args)},
                      error = function(e) {
                        is_known <- grepl("need at least two non-NA values to interpolate",
                                          e$message)
                        if (is_known) {
                          message(paste("Warning in callMBC: MBC did not have",
                                        "enough unique values in input datasets",
                                        "to interpolate!"))
                          message(paste("Returning MF unchanged."))
                          return(MF.slice[[1]])
                        } else {
                          print(e)
                          #print(traceback())
                          stop("Unknown error in MBC: Please examine outputs!")
                        }
                        stop("examine error output")
                      })

      #... the indexing on this function is going to be a pain.
      #I sort of think that we have to do synchronous missing values
      #and that the synch missing values also need some sort of master
      #OH and MH vector for missing points.
      #We also CAN'T use the pr masking functions with this method, I think -cew
      loop.mask <- index.mask
      loop.mask[is.na(MF.slice[[1]])] <- NA
      if (missval.out$mv.retry) {
        # We need to limit out to those values for which MF has data
        out <- apply.fudge.tmask(data = out, mask = loop.mask)
      }
      print(length(out_data[,,,n,,]))
      print(length(out))
      out_data[, , , n, , !is.na(index.mask)] <- index.qm(index.mask, out, olap)
      if (train_qcmask & missval.out$mv.retry) {
        print("adding the pr retry mask")
        qc[, , , n, , !is.na(index.mask)] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    } else {
      print("downscaling not possible at this point")
      out_data[ , , , n, , !is.na(index.mask)] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      if (train_qcmask) {
        qc[, , , n, , !is.na(index.mask)] <- index.qm(index.mask, rep(NA, length(index.mask)), olap)
      }
    }
  }
  return(list(ds = out_data, qc = qc))
}



callMBCvariant <- function(OH, MH, MF, variant, complete.hist, complete.fut, args) {
  # remove incomplete/missing days/levels from each of the inputs
  OH.slice <- as.matrix(as.data.frame(lapply(OH, function(x, y) {return(x[y])}, complete.hist)))
  MH.slice <- as.matrix(as.data.frame(lapply(MH, function(x, y) {return(x[y])}, complete.hist)))
  MF.slice <- as.matrix(as.data.frame(lapply(MF, function(x, y) {return(x[y])}, complete.fut)))

  if (!(tolower(args$variant) %in% c("spearman", "pearson", "n-pdft", "all"))) {
    stop(paste("Error in callMBCvariant: variant was not one of",
               "Pearson (MBCp), Spearman (MBCr), N-pdft (MBCn) or all (MBCn)!"))
  }
  mbc.fun <- switch(tolower(args$variant),
                    "spearman" = MBC::MBCr,
                    "pearson" = MBC::MBCp,
                    "n-pdft" = MBC::MBCn,
                    "all" = MBC::MBCn,
  )

  mbc_out <- mbc.fun(OH.slice, MH.slice, MF.slice,
                          iter = args$iter, jitter.factor = args$jitter.factor,
                          ratio.seq = args$ratio.seq, trace = args$trace,
                          ties = args$ties, silent = TRUE)
  # Care was taken to make sure the variable in the first column was the one
  # that should be returned, so we return only the first column of MBC results.
  return(mbc_out$mhat.p[, 1])
}

set.MBC.defaults <- function(args, predictor.vars, ..., env = parent.frame()) {
  #' Sets common defaults for the inputs to MBC that we want to control.
  jitter.old.names <- c("jitter", "jitter.fac", "jitter_fac")
  if (any(jitter.old.names %in% names(args))) {
    stop(paste("Error in set.MBC.defaults: MBC requires specification of is_jitter and possibly jitter_val!"))
  } else {
    if ("is_jitter" %in% names(args)) {
      if (args$is_jitter == TRUE) {
        if (args$jitter_val == "default") {
          jitter_val <- .Machine$double.eps^2
        } else {
          jitter_val <- as.numeric(args$jitter_val)
        }
      } else {
        if (!is.null(jitter_val) | ("jitter_val" %in% names(args)) ) {
          stop(paste("Error in set.MBC.defaults: when is_jitter = FALSE, cannot set jitter_val in config!"))
        } else {
          jitter_val <- 0
        }
      }
    }
  }

  # -------------------------------
  # defaults in QDM needed for MBC?
  # -------------------------------
  adjust_trace <- FALSE
  transform <- FALSE
  tform_fxn <- "none"
  screen_NaNs <- FALSE
  trace <- 0.05
  # -------------------------------
  # -------------------------------
  if ("trace" %in% names(args)) {
    non.numeric.trace <- is.na(suppressWarnings(as.numeric(args$trace)))
    #if trace is non-numeric (i.e an arg we'd pass to the pr functions
    #get OH pr units from adjust.metadata (created back in main driver) -cew
    if (non.numeric.trace) {
      pr.units <- adjust.metadata$pr_oh_units
      if (length(pr.units) == 0) {
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

  ratio_seq <- rep(FALSE, length(predictor.vars))
  rat.names <- c("ratio", "ratio.seq", "ratio_seq")
  if (any(rat.names %in% names(args))) {
    rat.name <- rat.names[which(rat.names %in% names(args))]
    in.rat.seq <- unlist(strsplit(args[[rat.name]], ","))
    if (length(in.rat.seq) != length(ratio_seq)) {
      stop(paste("Error in set.MBC.defaults: string", args$ratio.seq,
                 "did not have as many elements as MH has vars!"))
    }
    ratio_seq <- in.rat.seq
  }

  if ("adjust_trace" %in% names(args)) {
    adjust_trace <- ifelse(args$adjust_trace == "on", TRUE, FALSE)
  }

  # <simpleError in if (ratio) {    epsilon <- .Machine$double.eps    o.c[o.c < trace.calc] <- runif(sum(o.c < trace.calc), min = epsilon,         max = trace.calc)    m.c[m.c < trace.calc] <- runif(sum(m.c < trace.calc), min = epsilon,         max = trace.calc)    m.p[m.p < trace.calc] <- runif(sum(m.p < trace.calc), min = epsilon,         max = trace.calc)}: argument is of length zero>
  # Error in value[[3L]](cond) :


  return_args <- list(iter = args$iter,
                      jitter.factor = jitter_val,
                      ratio.seq = ratio_seq,
                      ties = args$ties,
                      trace = trace,
                      adjust_trace = adjust_trace,
                      variant = args$variant,
                      var.names = unlist(predictor.vars),
                      force_reproducibility = args$force_reproducibility)

  env$ds.ref$args <- return_args
  return(env)
}
