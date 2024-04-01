#Note: regex instead of pattern means that you can get all dims by using the
#empty string as your pattern

get.ncdim.names <- function(obj.nc, nc.var, include.none = FALSE) {
  #'Gets the names of all the dimensions a given var will reference
  get.dim.names()
  dim.names <- unlist(lapply(obj.nc$var[[nc.var]]$dim, "[[", "name"), use.names = F)
  if (length(dim.names) == 0) {
    if (include.none) {
      dim.names <- "none" #...okay, it's been '' for a long time. But that breaks the regex later.
    } else {
      dim.names <- ""
    }
  } else {
    if (include.none && any(check.fudge.dimname.match(dim.names, c(x.dims, y.dims)))) {
      dim.names <- c(dim.names, "none")
    }
  }
  return(dim.names)
}

get.ncvar.dims <- function(obj.nc, nc.var) {
  #Gets the dimensions of a variable stored in an open netcdf file, but
  #not yet read into memory
  return(sapply(lapply(test.nc[[nc.var]]$metadata$dim, "[[", "vals"), length))
}

obtain.fudge.var.old <- function(obj.nc, nc.var, coords) {
  #'Grabs a varaible about to be used in FUDGE
  #'and makes sure that its diemsnions correspond with the
  #'dimensions expected for FUDGE use.
  var.data <- ncvar_get(obj.nc, nc.var, collapse_degen = FALSE)
  var.dims <- get.ncdim.names(obj.nc, nc.var)
  get.dim.names()
  #Dimension order is different for the older code
  name.list <- list(x.dims, y.dims, z.dims, ens.dims, time.dims)
  dim.order <- c(0, 0, 0, 0, 0)
  dim.len   <- c(1, 1, 1, 1, 1)
  for(n in 1:5) {
    dim.loc <- which(var.dims %in% name.list[[n]])
    if (length(dim.loc) == 0) {
      dim.loc <- 0
      dim(var.data) <- c(dim(var.data), 1)
    }
    dim.order[n] <- dim.loc
  }
  num.zero <- sum(dim.order == 0)
  dim.order[dim.order == 0] <- seq( (6-num.zero), by=1, length.out=num.zero)
  #if (any(diff(dim.order)) < 1) {
  var.data <- aperm(var.data, perm=dim.order)
  gc()
  #}
  test.dim <- dim(var.data)
  #Force data into four dimensions
  if ( !(test.dim[3] == 1 || test.dim[4] == 1)) {
    stop(paste("Error in obtain.fudge.var.old: More than one of plev",
               "and ensemble was non-degenerate!"))
  } else {
    test.dim <- c(test.dim[1:2], prod(test.dim[3:4]),test.dim[5])
    dim(var.data) <- test.dim
  }
  return(var.data)
}

obtain.fudge.var <- function(obj.nc, nc.var, subset=list(), decimate=list(),
                             select=NA, verbose = FALSE) {
  #'Grabs a varaible about to be used in FUDGE
  #'and makes sure that its diemsnions correspond with the
  #'dimensions expected for FUDGE use.
  #var.data <- ncvar_get(obj.nc, nc.var, collapse_degen = FALSE)
  if (verbose) {print("in obtain.fudge.var")}
  #First, check to make sure that var is present in file
  if (! nc.var%in%names(obj.nc$var)) {
    stop(paste("Error in obtain.fudge.var: var", nc.var, "not found in file",
               obj.nc$filename,"!"))
  }
  var.dims <- get.ncdim.names(obj.nc, nc.var)
  get.dim.names()
  check_subset <- !(is.null(names(subset)))
  if (any(!(var.dims %in% c(x.dims, y.dims, z.dims, time.dims, ens.dims, n.dims))) && var.dims[1] != "") {
    dims.notlist <- var.dims[which(!(var.dims %in% c(x.dims, y.dims, time.dims, ens.dims, n.dims)))]
    stop(paste("Error in obtain.fudge.var! Var", nc.var, "uses the dimension(s)",
               paste(dims.notlist, collapse=","), "that are unknown to fudge;",
               "please add to classification function!"))
  }
  name.list <- list(x.dims, y.dims, z.dims, ens.dims, n.dims, time.dims)
  dim.order <- c(0, 0, 0, 0, 0, 0)
  dim.len   <- c(1, 1, 1, 1, 1, 1)
  dim.pres  <- c(T, T, T, T, T, T)
  dim.start <- c()
  dim.count <- c()
  decimate_idx    <- c()
  decimate_stride <- c()
  decimate_offset <- c()
  decimate_this <- F
  #print(name.list)
  for(n in 1:6) {
    dim.loc <- which(var.dims %in% name.list[[n]])
    if (length(dim.loc) == 0) {
      dim.loc <- 0
      dim.pres[n] <- F
    }
    dim.order[n] <- dim.loc
    if (any(names(subset) %in% name.list[[n]])) {
      dim.subset <- subset[[which(names(subset) %in% name.list[[n]])]]
      dim.start <- c(dim.start, dim.subset[[1]])
      dim.count <- c(dim.count, dim.subset[[2]])
    } else {
      dim.start <- c(dim.start, 1)
      dim.count <- c(dim.count, (-1))
    }
    #Similar reasoning for decimate - carried out by var, on array, after reading in
    if (any(names(decimate) %in% name.list[[n]])) {
      decimate_this   <- T
      decimate_idx    <- c(decimate_idx, n)
      decimate_stride <- c(decimate_stride, decimate[[which(names(decimate) %in% name.list[[n]])]][1])
      decimate_offset <- c(decimate_offset, decimate[[which(names(decimate) %in% name.list[[n]])]][2])
      #do the activation of these BEFORE the ordering step
    }
  }
  num.zero <- sum(dim.order == 0)
  dim.order[dim.order == 0] <- seq( (7-num.zero), by=1, length.out=num.zero)
  var.data <- ncvar_get(obj.nc, nc.var, collapse_degen = FALSE,
                        start=dim.start[dim.pres],
                        count=dim.count[dim.pres])
  #2017-11-2: Adding manual correction for the dims of var.data being returned
  #as 0 when there is only one element
  var.dim <- dim(var.data)
  if (decimate_this) {
    var.data <- decimate_var(var.data, dim.order[decimate_idx], decimate_stride, decimate_offset)
    var.dim[dim.order[decimate_idx] ]<- dim(var.data)[dim.order[decimate_idx]]
  }
  var.dim[var.dim == 0] <- 1
  dim.len[dim.pres] <- var.dim
  dim(var.data) <- dim.len
  return(var.data)
}

obtain.fudge.mask.var <- function(obj.nc, nc.var, subset=list(), decimate=list(), force_6_dim = FALSE, verbose = FALSE) {
  #'Grabs a varaible about to be used in FUDGE
  #'and makes sure that its diemsnions correspond with the
  #'dimensions expected for FUDGE use.
  #var.data <- ncvar_get(obj.nc, nc.var, collapse_degen = FALSE)
  if (verbose) {print("in obtain.fudge.mask.var")}
  var.dims <- get.ncdim.names(obj.nc, nc.var)
  #var.dim.lengths <- get.ncvar.dims(obj.nc, nc.var)
  get.dim.names()
  name.list <- list(x.dims, y.dims, z.dims, ens.dims, n.dims, time.dims)
  dim.order <- c(0, 0, 0, 0, 0, 0)
  dim.len   <- c(1, 1, 1, 1, 1, 1)
  dim.pres  <- c(T, T, T, T, T, T)
  dim.start <- c()
  dim.count <- c()
  decimate_idx    <- c()
  decimate_stride <- c()
  decimate_offset <- c()
  decimate_this <- F
  for(n in 1:6) {
    dim.loc <- which(var.dims %in% name.list[[n]])
    if (length(dim.loc) == 0) {
      dim.loc <- 0
      #var.dim.lengths <- c(var.dim.lengths, 1)
      dim.pres[n] <- F
    }
    dim.order[n] <- dim.loc
    if (any(names(subset) %in% name.list[[n]])) {
      dim.subset <- subset[[which(names(subset) %in% name.list[[n]])]]
      dim.start <- c(dim.start, dim.subset[1])
      #dim.count <- c(dim.count, (dim.subset[2] - dim.subset[1])+1)
      dim.count <- c(dim.count, dim.subset[2])
    } else {
      dim.start <- c(dim.start, 1)
      dim.count <- c(dim.count, (-1))
    }
    #similar reasoning for decimate
    if (any(names(decimate) %in% name.list[[n]])) {
      decimate_this   <- T
      decimate_idx    <- c(decimate_idx, n)
      decimate_stride <- c(decimate_stride, decimate[[which(names(decimate) %in% name.list[[n]])]][1])
      decimate_offset <- c(decimate_offset, decimate[[which(names(decimate) %in% name.list[[n]])]][2])
      #do the activation of these BEFORE the ordering step
    }
  }
  num.zero <- sum(dim.order == 0)
  dim.order[dim.order == 0] <- seq( (7-num.zero), by=1, length.out=num.zero)
  dim.count <- dim.count[dim.order]
  dim.start <- dim.start[dim.order]
  var.data <- ncvar_get(obj.nc, nc.var, collapse_degen = FALSE,
                        start=dim.start[dim.pres[dim.order]], count=dim.count[dim.pres[dim.order]])
  var.dim <- dim(var.data)
  if (decimate_this) {
    var.data <- decimate_var(var.data, dim.order[decimate_idx], decimate_stride, decimate_offset)
    var.dim[dim.order[decimate_idx] ]<- dim(var.data)[dim.order[decimate_idx]]
  }
  if (force_6_dim) {
    var.dim[var.dim == 0] <- 1
    dim.len[dim.pres] <- var.dim
    dim(var.data) <- dim.len #c(dim(var.data), dim.len[!dim.pres])
    #     #if (any(diff(dim.order)) < 1) {
    #       var.data <- aperm(var.data, perm=dim.order) #dim.order used to get ordered as well, but that might not be right
    #       gc()
    #     #}
  }
  return(var.data)
}

get.dim.names <- function(env=parent.frame()) {
  #What I want this function to do is return the
  #possible vals of each dim name to the parent
  #environment.
  env$x.dims <- c("x", "i", "lon", "longitude") #"none"
  env$y.dims <- c("y", "j", "lat", "latitude")
  env$z.dims <- c("z", "p", "k","plevel", "plev", "height")
  env$time.dims <- c("time")
  env$ens.dims <- c("m", "ensmem", "ensemble", "ens", "station")
  env$n.dims <- c("n", "nsave", "determinant", "nmasks", "bnds", "nvar", "nchar")
  env$bnds.dims <- c("bnds", "bounds")
  return(env)
}

get.metavar.names <- function(env=parent.frame()) {
  #' Returns the names of variables associated with
  #' metadata (either because of dimensions or because
  #' of bookkeeping) to the parent function.
  #' Commonly called in conjunction with get.datavar.names
  env$x.vars <- c("i_offset", "lon_bnds", "lon_bounds")
  env$y.vars <- c("j_offset", "lat_bnds", "lat_bounds")
  env$z.vars <- c("z", "p", "k_offset","plevel", "plev", "height")
  env$time.vars <- c("t_bnds","time_bnds", "time_bounds")
  env$ens.dims <- c("m", "ensmem", "ensemble", "ens", "station_id")
  # climate vars that act like metadata. See the spring index tool.
  env$climvars <- c("fli_wtsum_thold", "fbi_wtsum_thold")
  #env$n.dims <- c("n")
  #env$bnds.dims <- c("bnds", "bounds")
  return(env)
}

get.datavar.names <- function(nc_file) {
  #' gets the names of all vars likely to be data-related from the
  #' input file. It does so by removing any variables determined
  #' to be metadata-related from the input.
  #' Note as of 2022-09: variables also must be numeric
  #' (float or double precision)
  #' and are organized by size (bigger = more dim coverage)
  nc.obj <- nc_open(nc_file)
  vars <- c()
  var_sizes <- c()
  get.metavar.names()
  meta_vars <- c(x.vars, y.vars, z.vars, time.vars, ens.dims, climvars)
  for (n in names(nc.obj$var)) {
    if (! (n %in% meta_vars) ) {
      vprec <- nc.obj$var[[n]]$prec
      #currently only filtering char; may expand to int and double prec
      if (vprec != "char") {
        vars <- c(vars, n)
        var_sizes <- c(var_sizes, prod(nc.obj$var[[n]]$size))
      }
    }
  }
  nc_close(nc.obj)
  #sort so that largest vars are in front
  if (length(vars) > 1) {
    vars <- vars[order(var_sizes, decreasing = TRUE)]
  }
  return(vars)
}

get.selection.dim.names <- function(select, include.none = FALSE) {
  get.dim.names()
  if (!is.na(select[1])) {
    select.names <- c()
    for (el in select) {
      select.el <- switch(el,
                          "spatial" = paste0(c(x.dims, y.dims, z.dims), "$"),
                          "space" = paste0(c(x.dims, y.dims, z.dims), "$"),
                          #"spatial" = paste0("^(?!", time.dims, "$)"), #paste0("^(?!", ens.dims, "$)")),
                          #"space" = paste0("^(?!", time.dims, "$)"), #paste0("^(?!", ens.dims, "$)")),
                          "other" = paste0(n.dims, "$"),
                          "n" = paste0(n.dims, "$"),
                          "temporal" = paste0(time.dims, "$"),
                          "time" = paste0(time.dims, "$"),
                          "ens" = paste0(ens.dims, "$"),
                          "ensemble" = paste0(ens.dims, "$"))
      select.names <- c(select.el, select.names)
      if (el %in% c("space", "spatial")) {
        if (include.none) {
          select.names <- c(select.names, "none$")
        }
      }
    }
  } else {
    select.names <- ""
  }
  select.names <- unique(select.names)
  return(select.names)
}

check.fudge.dimname.match <- function(patt, vec) {
  #Sees if there are overlaps in the sets of dimensions
  #described by patt within vec
  #Excludes bounds vars b/c those don't get used for metadata
  get.dim.names()
  vec <- vec[!(vec %in% bnds.dims)]
  s.out <- sapply(patt, grepl, vec, perl = TRUE)
  if (!is.null(dim(s.out))) {
    s.out <- apply(s.out, 1, sum)
  }
  return(s.out > 0)
}

obtain.fudge.var.dimensions <- function(nc.obj, nc.var, select=NA, is_dim = FALSE,
                                        subset=list(), decimate=list(), verbose = FALSE) {
  #Grabs the dimension object wholesale from the
  #netcdf object
  if (verbose) {print("in obtain.fudge.var.dimensions")}
  get.dim.names()
  name.list <- list(x.dims, y.dims, z.dims, time.dims, ens.dims, n.dims)
  select.dimnames <- get.selection.dim.names(select)
  if (!is_dim) {
    if (is.na(select[1])) {
      dim.select <- nc.obj$var[[nc.var]]$dim
    } else {
      dim.select <- list()
      dimvar.names <- sapply(nc.obj$var[[nc.var]]$dim, "[[", "name")
      dim.match <- check.fudge.dimname.match(select.dimnames,dimvar.names)
      dim.select <- nc.obj$var[[nc.var]]$dim[dim.match == TRUE]
    }
    dim.select.names <- sapply(dim.select, "[[", "name")
    names(dim.select) <- dim.select.names
    out <- dim.select[!is.null(dim.select)]
    for (el in names(out)) {
      out[[el]]$filename <- nc.obj$filename
    }
  } else {
    # godawful hack for read.time.data and subsetting
    print("in hack")
    dim.select.names <- nc.var
    dim.select <- list()
    dim.select[nc.var] = nc.obj$dim[nc.var]
  }
  #If you are specifying a subset, it is neccessary to remove dims belonging to the subset
  for(n in 1:length(name.list) ) {
    if (any(names(subset) %in% name.list[[n]])) {
      n.subset <- subset[[which(names(subset) %in% name.list[[n]])]]
      n.outname <- dim.select.names[which(dim.select.names %in% name.list[[n]])]
      if (any (dim.select.names %in% name.list[[n]])) {
        out[[n.outname]]$vals <- out[[n.outname]]$vals[n.subset[1]:(n.subset[1] + n.subset[2] - 1)]
        out[[n.outname]]$len <- n.subset[2]
      }
    }
    if (any(names(decimate) %in% name.list[[n]])) {
      print("decimate triggered on:")
      print(which(names(decimate) %in% name.list[[n]]))
      n.decimate <- decimate[[which(names(decimate) %in% name.list[[n]])]]
      n.outname <- dim.select.names[which(dim.select.names %in% name.list[[n]])]
      if (any (dim.select.names %in% name.list[[n]])) {
        out[[n.outname]]$vals <- decimate_var(out[[n.outname]]$vals, 1, n.decimate[1], n.decimate[2])
        out[[n.outname]]$len <- length(out[[n.outname]]$vals)
        #         out[[n.outname]]$vals <- out[[n.outname]]$vals[n.subset[1]:(n.subset[1] + n.subset[2] - 1)]
        #         out[[n.outname]]$len <- n.subset[2]
      }
    }
  }
  return(out)
}

obtain.fudge.var.metavars <- function(nc.obj, nc.var, select=NA, subset=list(), decimate=list(), verbose = FALSE) {
  #Get the non-data vars from the input file
  # note: it is assumed that all vars using exactly the
  # same vars as the var of interest are data vars
  if (verbose) {print("in obtain.fudge.metavars")}
  all.varnames <- names(nc.obj$var)
  nc.var.dims <- get.ncdim.names(nc.obj, nc.var, include.none = TRUE)
  select.dimnames <- get.selection.dim.names(select, include.none = TRUE)
  select.vars <- c()
  for(var in all.varnames) {
    var.dims <- get.ncdim.names(nc.obj, var, include.none = TRUE)
    #If not covering exactly the same dims as data var
    if ( !all(all.equal(nc.var.dims, var.dims) == TRUE) ) {
      #And if any of the dims the var uses match the dims of interest
      if (any(check.fudge.dimname.match(select.dimnames, var.dims))) {
        select.vars <- c(select.vars, var)
      }
    }
  }
  out <- lapply(select.vars, "clone.fudge.metavar.object", nc.obj, subset, decimate, verbose)
  names(out) <- select.vars
  for (vname in select.vars) {
    attributes(out[[vname]]) <- c(ncatt_get(nc.obj, vname), attributes(out[[vname]]))
  }
  return(out)
}

clone.fudge.metavar.object <- function(var.name, nc.obj, subset=list(), decimate=list(), verbose = FALSE) {
  #'Returns a list of various data and metadata used by the read/write functions
  #'for writing data.
  if (verbose) {print(paste("in clone.fudge.metavar.object with var", var.name))}
  dim.names <- get.ncdim.names(nc.obj, var.name)
  get.dim.names()
  name.list <-  list(x.dims, y.dims, z.dims, time.dims, ens.dims, n.dims)
  var.start <- rep(as.integer(1), length(dim.names))
  var.count <- rep(as.integer(-1), length(dim.names))
  for(el in names(subset)) {
    n <- sapply(name.list, function(x,y) {any(x%in%y)}, el)
    dim.which <- dim.names %in% unlist(name.list[n])
    var.start[dim.which] <- subset[[el]][1]
    var.count[dim.which] <- subset[[el]][2]#diff(subset[[el]])+1
  }
  var.all <- nc.obj$var[[var.name]]
  var.data <- tryCatch( {ncvar_get(nc.obj, var.name, start=as.integer(var.start),
                                   count=as.integer(var.count),
                                   collapse_degen = FALSE)},
                        error = function(e) {print(e);
                          print("Have you passed a null subset?");
                          stop("Please check the call to the IO functions!")}
  )
  decimate_idx <- c()
  decimate_stride <- c()
  decimate_offset <- c()
  decimate_this <- F
  if (length(decimate) > 0) {
    for (d in 1:length(decimate)) {
      el <- names(decimate)[d]
      n <- which(sapply(name.list, function(x,y) {any(x%in%y)}, el))
      dim.which <- dim.names %in% unlist(name.list[n])
      if ( any(dim.which) ) {
        decimate_this <- T
        decimate_idx <- c(decimate_idx, which(dim.which))
        decimate_stride <- c(decimate_stride, decimate[dim.names[dim.which]][[1]][1])
        decimate_offset <- c(decimate_offset, decimate[dim.names[dim.which]][[1]][2])
      }
    }
  } else {
    decimate_this <- F
  }
  #Then do the decimation calculations
  if (decimate_this) {
    var.data <- decimate_var(var.data, decimate_idx, decimate_stride, decimate_offset)
  }
  if (var.all$prec == "int") {
    var.all$prec <- "integer"
  }
  var.dims <- dim.names
  out <- c(var.all,
           list(vals=var.data, dimnames=dim.names))
  return(out)
}

clone.fudge.var.object <- function(var.name, nc.obj, subset=list(), verbose = FALSE) {
  #'Much the same as the above - this is for when we want to get all that
  #'information, but also want to force our vars into a 6-dimensional framework.
  #'Used primarily by ConvertCalendar, which needs to use aperm on masks
  #'
  #'For now, also using simplifying assumption that there aren't any subsets
  #'being used
  if (verbose) {print("in clone.fudge.var.object")}
  dim.names <- get.ncdim.names(nc.obj, var.name)
  get.dim.names()
  name.list <-  list(x.dims, y.dims, z.dims, time.dims, ens.dims, n.dims)
  var.data <- obtain.fudge.var(obj.nc = nc.obj, nc.var = var.name, subset=subset)
  var.all <- nc.obj$var[[var.name]]
  if (var.all$prec == "int") {
    var.all$prec <- "integer"
  }
  var.dims <- dim.names
  out <- c(var.all,
           list(vals=var.data, dimnames=dim.names))
  return(out)
}

create.fudge.metavar.object <- function(var.name, var.data, ...) {
  #Creates a list of data and metadata used by the read/write functions
  #based on user-supplied input (i.e. what you need for creating time_bnds)
  return(list(vals=var.data, ...))
}

obtain.fudge.var.metadata <- function(nc.obj, nc.var, verbose = FALSE) {
  #'Grabs the single-attribute components
  #'of the attribute table from the input
  #'nc object and variable.
  if (verbose) {print("in obtain.fudge.var.metadata")}
  attnames <- c("standard_name", "long_name", "units",
                "prec","calendar", "filename", "rips",
                "missing_value", "_FillValue")
  att.defaults <- c("", nc.var, "",
                    "float", "","", "",
                    -999, -999)
  var.att.list <- list()
  #Get var atts
  for (n in 1:length(attnames)) {
    tmp.att <- ncatt_get(nc.obj, nc.var, attnames[n])
    if ( !(tmp.att$hasatt)) {
      tmp.att$value <- att.defaults[n]
    }
    var.att.list[n] <- tmp.att$value
  }
  names(var.att.list) <- attnames
  #2020-08: missing-value-specific behavior
  # 99% of the time, NONE of these flags should trip
  # (there is no assign on this; we're switching on presence/absence of var def)
  default_missval <- 1e20
  default_int_missval <- (-2) * 10^9
  if ((var.att.list$missing_value != -999) && (var.att.list[["_FillValue"]] != -999)) {
    # 2020-09
    # if the vars written are of float precision, _FillValue can be float
    # while missing value is double. Since R doesn't have a short float type,
    # (but C does), this is a workaround for vals that are clearly MEANT
    # to be the same.
    valdiff <- as.double(var.att.list$missing_value) - as.double(var.att.list[["_FillValue"]])
    eps <- 1e-8 * as.double(var.att.list$missing_value)
    # 2022-11
    # adding abs() around valdiff and eps because valdiff could be 0 and eps
    # negative and its about the magnitude, not the sign
    if (abs(valdiff) > abs(eps)) {
      stop(paste("Error in obtain.fudge.var.metadata:",
                 "missing_value and _FillValue did not match!",
                 paste("\n\tmissing_value:", as.double(var.att.list$missing_value)),
                 paste("\n\t_FillValue:", as.double(var.att.list[["_FillValue"]]))
      ))
      #quit(status=11) code never gets here; exits with 1 at stop
    }
  } else if (var.att.list$missing_value == -999 && var.att.list$missing_value != default_missval) {
    missval_input_changed <<- TRUE
  } else if (var.att.list[["_FillValue"]] != -999 && var.att.list[["_FillValue"]] != default_missval) {
    missval_input_changed <<- TRUE
  }
  #Do the same thing for time atts (mostly just calendar)
  time.attnames <- c("calendar", "modulo")
  time.att.defaults <- c("none", (-1))
  if ( "time" %in% names(nc.obj$dim)) {
    for (n in 1:length(time.attnames)) {
      tmp.att <- ncatt_get(nc.obj, "time", time.attnames[n])
      if ( !(tmp.att$hasatt)) {
        tmp.att$value <- time.att.defaults[n]
      }
      var.att.list[time.attnames[n]] <- tmp.att$value
    }
  } else {
    #If there is no time dimension, don't try to query it -
    #that gives you an error
    for (n in 1:length(time.attnames)) {
      var.att.list[time.attnames[n]] <- time.att.defaults[n]
    }
  }
  #...and the same thing for climatology.

  #Add backstip for empty rip attribute
  if (var.att.list$rips == "") {
    var.att.list$rips <- get.rip.data(nc.obj)
  }
  #Get global atts
  glob_attnames <- c("frequency", "climatology", "trend")
  glob.defaults <- c("", "", "")
  glob.att.list <- list()
  for (n in 1:length(glob_attnames)) {
    tmp.att <- ncatt_get(nc.obj, 0, glob_attnames[n]) #previously getting from nc var?
    if ( !(tmp.att$hasatt)) {
      tmp.att$value <- glob.defaults[n]
    }
    glob.att.list[n] <- tmp.att$value
  }
  names(glob.att.list) <- glob_attnames
  var.att.list <- c(var.att.list, glob.att.list)
  #optional.atts <- c("comments", "modulo")
  var.att.list["filename"] <- nc.obj$filename
  #And finally - store a list of ALL attributes in the file, in case we
  #need to investigate it later.
  var.att.list["all_atts"] <- list(ncatt_get(nc.obj, nc.var))
  return(var.att.list)
}

get.rip.data <- function(nc.obj) {
  #Tries a few methods for getting the rip information
  #from an input file
  #Try the filename first; accept both alt paterns
  rip <- "ZZ"
  data.list <- unlist(strsplit(basename(nc.obj$filename), split="_"), use.names = FALSE)
  test.rip <- data.list[5]
  if (grepl("r[[:digit:]].*i[[:digit:]]p[[:digit:]]", test.rip) ||
      test.rip %in% c("DSDEPh", "DSCVh")) {
    rip <- test.rip
  }
  if (rip == "ZZ") {
    test.atts <- ncatt_get(nc.obj, 0)
    #Check for "realizations"
    if ("realizations" %in% names(test.atts)) {
      rip <- test.atts["realizations"]
    }else if ( "rip" %in% names(test.atts) ) {
      rip <- test.atts["rip"]
    }else if (sum(c("realization", "initialization_method", "physics_version") %in% names(test.atts)) == 3) {
      rip <- paste0("r", test.atts["realization"], "i",
                   test.atts["initialization_method"],
                   "p", test.atts["physics_version"])
    } else {
      #Absolute backstop: feed a string
      rip <- "unknown.rip"
    }
  }
  return(rip)
}


obtain.fudge.datavar.names <- function(nc.obj, verbose = FALSE) {
  #Get the datavars from the file
  # (note: it's assumed that if a var references 2 of lat, lon or time)
  # it is considered a datavar for reference purposes
  if (verbose) {print("in obtain.fudge.datavar.names")}
  all.varnames <- names(nc.obj$var)
  select.dimnames <- c("lon", "lat", "x", "y","time")
  select.vars <- list()
  for(v in all.varnames) {
    var.dims <- get.ncdim.names(nc.obj, v, include.none = TRUE)
    #If not having at least 2 vars present in data dims
    present_list <- sapply(select.dimnames, function(x) {x %in% var.dims})
    if ( (sum(present_list) > 1) ) {
      select.vars[v] <- list(select.dimnames[present_list])
    }
  }
  return(select.vars)
}
