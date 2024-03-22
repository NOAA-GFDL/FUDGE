#'Wrapper function for the FUDGE calls and QDM,

callQDM <- function(OH, MH, MF, args, ds.method = "",
                    tmasks, train_qcmask = FALSE) {
  if (args$force_reproducibility & w == 1) {
    # i_idx, j_idx, i_offset and j_offset are passed in from the command-line.
    # This is meant to enforce reproducibility when we have alternative
    # subsetting in place. -cew
    seedval <- ( 31 * (ival + i_idx) ) + ( 29 * (jval + j_idx) )
    set.seed(seedval)
  }
  # Create output data
  out_data <- array(NA, dim = dim(OH[[1]]))
  nrips <- dim(out_data)[4]
  index_mask <- tmasks$OH$masks[[w]]
  olap <- FALSE
  if (train_qcmask) {
    if (tmasks$apply_tmasks) {
      qc <- mapply(rep, index_mask, nrips)
    } else {
      qc <- rep(1, dim(OH[[1]]))
    }
    dim(qc) <- dim(out_data)
  } else {
    qc <- NULL
  }
  MF_mask <- missval.args$pr_opts$MF
  tmp_missvals <- missval.args
  for (n in 1:nrips) {
    nval <<- n
    MF_slice <- lapply(MF, "[", ,,,n,, , drop = FALSE)
    #pr-specific check for now; only do if present
    missval.args$pr_opts$MF <- MF_mask[,,,n,, ,drop = FALSE]
    missval.out <- remove.FUDGE.missvals(OH, MH, MF_slice,
                                         missval.args$minsamplesize,
                                         missval.args,
                                         missval.method = "distributional")

    if (args$adjust_trace) {
      adj_thold <- missval.args$pr_opts$adjusted.thold
      if (args$transform) {
        if (is.null(args$tform_fxn[[2]])) {
          adj_thold <- do.call(args$tform_fxn[[1]], list(adj_thold))
        } else {
          adj_thold <- do.call(args$tform_fxn[[1]], list(adj_thold, args$tform_fxn[[2]]))
        }
      }
      if (args$trace > adj_thold) {
        args$trace <- adj_thold
      }
    }
    if (missval.out$ds.possible) {
      # In place of the old QDM_mod, now we set pp.type = 3
      pp_type <- 7
      if (missval.out$mv.retry) {
        print("switching QDM pp.type = 7 to pp.type = 3")
        pp_type <- 3
      }
      if (ds.method == "QDM") {
        ds_arglist <- list(missval.out$OH, missval.out$MH, missval.out$MF, c(args, pp.type = pp_type))
      } else if (ds.method == "QQDC") {
        ds_arglist <- list(missval.out$MF, missval.out$MH, missval.out$OH, c(args, pp.type = pp_type))
      } else {
        stop("calling QDM without a ds.method arg specified!")
      }
      out <- tryCatch({do.call("callQDM_internal", args = ds_arglist)},
                      error = function(e) {
                        is_known <- grepl("need at least two non-NA values to interpolate",
                                          e$message)
                        if (is_known) {
                          message(paste("Warning in callQDM: QDM did not have",
                                        "enough unique values in input datasets",
                                        "to interpolate!"))
                          message(paste("Returning MF unchanged."))
                          return(MF_slice[[1]])
                        } else {
                          print(e)
                          stop("Unknown error in QDM: Please examine outputs!")
                        }
                        stop("examine error output")
                      })

      loop_mask <- index_mask
      loop_mask[is.na(MF_slice[[1]])] <- NA
			if (missval.out$mv.retry) {
			  out <- apply.fudge.tmask(data = out, mask = loop_mask)
			}
      out_data[,,,n,,] <- index.qm(index_mask, out, olap)
      if (train_qcmask &  missval.out$mv.retry) {
        print("adding the pr retry mask")
        qc[,,,n,,] <- index.qm(index_mask, rep(NA, length(index_mask)), olap)
      }
    } else {
      print("downscaling not possible at this point")
      out_data[,,,n,,] <- index.qm(index_mask, rep(NA, length(index_mask)), olap)
      if (train_qcmask) {
        qc[,,,n,,] <- index.qm(index_mask, rep(NA, length(index_mask)), olap)
      }
    }
  }
  return(list(ds = out_data, qc = qc))
}

callQDM_internal <- function(OH, MH, MF, args) {
  OH_idx <- !is.na(unlist(as.vector(OH)))
  MF_idx <- !is.na(unlist(as.vector(MF)))
  OH_slice <- remove.missvals(unlist(as.vector(OH), use.names = FALSE))
  MH_slice <- remove.missvals(unlist(as.vector(MH), use.names = FALSE))
  MF_slice <- remove.missvals(unlist(as.vector(MF), use.names = FALSE))

  args$ratio <- args$ratio.seq
  screen_NaNs <- args$screen_NaNs

  not.args <- c("adjust.trace", "transform", "tform_fxn",
                "force_reproducibility", "var.names",
                "ratio.seq", "iter", "adjust_trace", "screen_NaNs", "ds.method")
  args <- args[!(names(args) %in% not.args)]

  q.args <- c(list(o.c = OH_slice, m.c = MH_slice, m.p = MF_slice), args)

  #Feb 2023 - suppressing *all* warnings; once per tmask is too many
  tmp <- withCallingHandlers(do.call("QDM", q.args)$mhat.p,
                             warning = function(w) {
                               if ((conditionMessage(w) == c("collapsing to unique 'x' values"))) {
                                 rlang::cnd_muffle(w)
                               }
                             }
  )
  if (screen_NaNs) { # Screen out NaNs from 0/0
    tmp[is.nan(tmp)] <- 0
  }
  out <- rep(NA, length(MF))
  # out[OH_idx] <- tmp
  out[MF_idx] <- tmp
  if ("trace.calc" %in% names(args)) {
    out[unlist(as.vector(MF), use.names = FALSE) == 0] <- 0
  }
  return(out)
}

set.QDM.defaults <- function(args, predictor.vars, ..., env = parent.frame()) {
  #'Sets common defaults for the inputs to QDM that we want to control.
  iter <- 30
  is_jitter <- FALSE
  jitter_val <- NULL
  jitter.fac <- 0
  ratio.seq <- FALSE #should be 1 for this case
  trace <- 0.05
  adjust_trace <- FALSE
  transform <- FALSE
  tform_fxn <- "none"
  force_reproducibility <- FALSE
  screen_NaNs <- FALSE
  if ("trace" %in% names(args)) {
    non_numeric_trace <- is.na(suppressWarnings(as.numeric(args$trace)))
    #if trace is non-numeric (i.e an arg we'd pass to the pr functions
    #get OH pr units from adjust.metadata (created back in main driver)
    if (non_numeric_trace) {
      pr_units <- adjust.metadata$pr_oh_units
      if (length(pr_units) == 0) {
        stop(paste("Error in set.QDM.defaults: no pr vars found in OH!"))
      }
      pr_trace <- convert.pr.threshold(args$trace, pr_units)
      trace <- pr_trace
    } else {
      trace <- as.numeric(args$trace)
    }
    # Now check the section 5 args for the
    if (exists("pre.ds.train")) {
      pre.ds.types <- sapply(pre.ds.train, "[[", "type")
      if ("transform" %in% pre.ds.types) {
        transform <- TRUE
        tform.idx <- which(pre.ds.types == "transform")
        tform_fxn <- return.t.fxn(pre.ds.train[[tform.idx]]$qc_args$transform,
                              pre.ds.train[[tform.idx]]$qc_args$arg)
        if (is.null(tform_fxn[[2]])) {
          trace <- do.call(tform_fxn[[1]], list(trace))
        } else {
          trace <- do.call(tform_fxn[[1]], list(trace, tform_fxn[[2]]))
        }
      }
    }
  }
  rat.names <- c("ratio", "ratio.seq", "ratio_seq")
  if ("type" %in% names(args)) {
    if (args$type == "add") {
      ratio.seq <- FALSE
    } else if (args$type == "mult") {
      ratio.seq <- TRUE
    }
  }
  #behavior changed 2022-10 - explicitly exit with error if using old args
  jitter.old.names <- c("jitter", "jitter.fac", "jitter_fac")
  if (any(jitter.old.names %in% names(args))) {
    stop(paste("Error in set.QDM.defaults: QDM requires specification of is_jitter and possibly jitter_val!"))
  } else {
    if ("is_jitter" %in% names(args)) {
      if (args$is_jitter) {
        jitter.fac <- args$jitter_val
        if (args$jitter_val == "default") {
          jitter.fac <- .Machine$double.eps^2
        } else {
          jitter.fac <- as.numeric(args$jitter_val)
        }
      } else {
        if (!is.null(jitter_val) | ("jitter_val" %in% names(args)) ) {
          stop(paste("Error in set.QDM.defaults: when is_jitter = FALSE, cannot set jitter_val in config!"))
        } else {
          jitter.fac <- 0
        }
      }
    }
  }
  if ("adjust_trace" %in% names(args)) {
    adjust_trace <- ifelse(args$adjust_trace == "on", T, F)
  }
  if ("force_reproducibility" %in% names(args)) {
    if (args$force_reproducibility %in% c("on", "off", TRUE, FALSE)) {
      if (args$force_reproducibility %in% c("on", TRUE)) {
        force_reproducibility <- TRUE
      } else {
        force_reproducibility <- FALSE
      }
    } else {
      stop(paste("Error in set.QDM.defaults: force_reproducibility should be one of",
                 paste(c("on", "off", TRUE, FALSE), collapse = ","), "but was",
                 args$force_reproducibility, "!"))
    }
  } else {
    if (args$type == "mult" && jitter.fac != 0) {
      stop(paste("Error in set.QDM.defaults: as of October 2022,",
                 "force_reproducibility argument ('on' or 'off')",
                 "is required to be set in the JSON for QDM when type == 'mult'",
                 "and is_jitter is true!"))
    } else {
      #default changed 2022-10 - was false, is now true
      force_reproducibility <- TRUE
    }
  }
  #Put together QDM-specific args - but keep trace_calc default of 0.5 * trace
  return.args <- list(iter = iter, jitter.factor = jitter.fac, ratio.seq = ratio.seq, trace = trace,
                      var.names = unlist(predictor.vars),
                      adjust_trace = adjust_trace, transform = transform, tform_fxn = tform_fxn,
                      force_reproducibility = force_reproducibility)
  return.args$screen_NaNs <- screen_NaNs
  #Deal with sample size
  if ("minsamplesize" %in% names(args)) {
    return.args$minsamplesize <- args$minsamplesize
  }
  env$ds.ref$args <- return.args
  return(env)
}
