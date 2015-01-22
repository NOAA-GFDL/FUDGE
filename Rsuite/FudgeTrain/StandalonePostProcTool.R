#'CallPostProcMethod
#'Calls a Post-processing method on a series of downscaled
#'data, transforming it in one way or another. The 
#'Methods generally take two parameters: 
#'@param data: The downscaling data to be processed. 
#'@param args: The arguments to the pot-processing function.
#'
#'@author Carolyn Whitlock, October 2014
#'

CallPostProcMethod <- function(pp.method, data, mask, mask.data, args){
  switch(pp.method, 
         'compare.correct' = postProc_byCompare(data, mask, mask.data, args),
         'Nothing' = postProc_Nothing(data, args), 
         ReturnPostProcMethodError(pp.method))
}

ReturnPostProcMethodError <- function(pp.method){
  stop(paste("Post Process Mehtod Error: method", pp.method, 
             "is not supported for post-processing at this time."))
}
callNothing <- function(data, args){
  #Does absolutely nothing to the downscaling values of the current 
  #function. 
  return(data)
}

postProc_byCompare <- function(data, mask, mask.data, args){
  #TODO: At some point, include the var post-processing option
  #and some sort of units check to go with it.
  if(!is.null(args$compare.factor)){
    correct.factor <- args$compare.factor
    args$compare.factor <- NULL
  }else{
    if(!is.null(args$var)){
      if(args$var=='pr'){
        correct.factor <- 1e-06
      }else{
        correct.factor = 6
      } 
    }else{
      correct.factor <- 6
    }
    if(length(args)!=0) sample.args=args else sample.args=NULL
    out.vals <- ifelse( (mask==1), yes=data, no=mask.data )
    
    return(out.vals)
  }
}

ncks.clone <- function(old.file, var, new.file, force_override=TRUE){
  #Simple R wrapper for ncks, because R works REALLY BADLY with standard input
  commandstr <- paste('ncks -a -h -v', var)
  if(force_override==TRUE){
    commandstr = paste(commandstr, "-O")
  }else{
    if(file.exists(new.file)){
      stop("File Exists Error: the file", new.file, "already exists. Please call function", 
           "with force_override=TRUE or delete the file.")
    }
  }
  commandstr <- paste(commandstr, " '", old.file, "' '", new.file, "'", sep="")
  print(commandstr)
  system(commandstr)
}

postProc.Driver <- function(in.file=NA, var, k.fold, postproc.method, out.dir,
                            tmask.infile=NA, postproc.outfile=NA, 
                            in.dir=NA, tmask.dir=NA, postproc.outdir=NA, lons=NA, lone=NA, ...){
  #R driver script for the post-processing functions to set them up as a 
  library(ncdf4)
  options(error=traceback, warn = 1, showErrorCalls=TRUE)
  ###This works in the terminal, but not in the R code. No idea why. May need to passs as an arg.
  #FUDGEROOT = Sys.getenv(c("BASEDIR"))
  FUDGEROOT = "/home/cew/Code/fudge2014"
  print(FUDGEROOT)
  sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeIO/src/',sep=''), full.names=TRUE), source);
  sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgePreDS/src/',sep=''), full.names=TRUE), source);
  sapply(list.files(pattern="[.]R$", path=paste(FUDGEROOT,'Rsuite/FudgeTrain/src/',sep=''), full.names=TRUE), source);
  ###Start on drive script itself
  if(is.na(in.file)){
    #Assume that a directory means minifiles
    message(paste("Running in iterate mode; searching for lons and lone"))
    for (index in lons:lone){
      message(paste("On lon", index, "of", lone))
      in.file <- list.files(pattern=as.character(index), path=in.dir, full.names=TRUE)
      if(length(in.file) > 1){
        stop(paste("Pattern match error: There is more than one file in the directory", 
                   in.dir, "that matches the pattern", index, ". Please recheck directory."))
      }
      in.suffix <- sub(pattern=".nc", replacement="", x=basename(in.file))
      #Tmask.infile will always point to the same file. 
      #tmask.infile <- list.files(pattern=as.character(index), path=tmask.dir, full.names=TRUE)
      #build postproc.outfile from the netcdf of the input
      postproc.outfile <- paste(out.dir, "PostProcess/", postproc.method, "-",in.suffix, sep="")
#       if(postproc.method=='compare.correct'){
#         #Will break HORRIBLY if we start getting more than one pp method at once
#         #TODO CEW: Fix this in a way that will keep in from breaking. 
#         qc.mask.path <- list.files(pattern=as.character(index), path=qc.mask.path, full.names=TRUE)
#         qc.data.path <- list.files(pattern=as.character(index), path=qc.mask.path, full.names=TRUE)
#       }
      ####CHECK THIS FOR PATH CONSISTENCY
      message('cloning file')
      ncks.clone(old.file=in.file, var=var, new.file=postproc.outfile, force_override=TRUE)
      new.arg.list <- as.list(match.call())
      new.arg.list$in.file = in.file
      new.arg.list$tmask.infile = tmask.infile
      new.arg.list$postproc.outfile = postproc.outfile
      if(postproc.method=='compare.correct'){
        new.arg.list$qc.mask.path <- paste(qc.mask.path, list.files(pattern=as.character(index), path=qc.mask.path), sep="")
        #Okay - it appears that the assignments from this spot are working, but get overridden in the do.call
        #structure. 
        new.arg.list$qc.data.path <- paste(qc.data.path, list.files(pattern=as.character(index), path=qc.data.path), sep="")
        print(qc.data.path)
        print(new.arg.list$qc.data.path)
      }
      message('Calling PostProcess All')
      new.arg.list <- new.arg.list[names(new.arg.list)!=""]
      print(new.arg.list)
      env <- new.env()
      do.call("PostProcessAll",args=new.arg.list, envir=env)
      #PostProcessAll(unlist(new.arg.list))
    }
  }else{
    message("in single file branch")
    ncks.clone(old.file=in.file, var=var, 
               new.file=paste(out.dir, postproc.outfile, sep=""), force_override=TRUE)
    #pp.args <- eval(parse(text=(sub(pattern="postProc.Driver", replacement="list", x=match.call()))))
    pp.args <- as.list(match.call())
    print(pp.args)
    pp.args <- pp.args[names(pp.args)!=""]
    print('printing new pp.args')
    print(mode(pp.args))
    print(names(pp.args))
    do.call("PostProcessAll",args=pp.args)
  }
}

PostProcessAll <- function(in.file, var, k.fold, postproc.method, out.dir,
                           tmask.infile, postproc.outfile, ...){
  print(match.call())
  message('in post-process all')
  print(in.file)
  in.data.nc <- nc_open(in.file)
  data <- ncvar_get(in.data.nc, var, collapse_degen=FALSE)
  args=list()
  if(postproc.method=='compare.correct'){
    train.and.use.same <<- TRUE
    twindow.list <- CreateTimeWindowList(hist.train.mask=tmask.infile, hist.targ.mask=tmask.infile, esd.gen.mask=tmask.infile, 
                                         k=k.fold, method="generic")
    message('seeking mask variables')
    print(qc.mask.path)
    qc.mask.nc <- nc_open(qc.mask.path)
    varnames <- names(qc.mask.nc$var)
    print(varnames)
    print(paste('var_name:', varnames[[which(regexpr('*qc_mask', varnames) > 0 ) ]]))
    qc.mask <- ncvar_get(qc.mask.nc, varnames[[which(regexpr('*qc_mask', varnames) > 0 ) ]], collapse_degen=FALSE)
    qc.data.nc <- nc_open(qc.data.path)
    qc.data <- ncvar_get(qc.data.nc, var, collapse_degen=FALSE)
    postproc.args$var <- var
    output <- TrainDriver(target.masked.in=qc.mask, hist.masked.in=qc.data, fut.masked.in=data, mask.list=twindow.list, ds.method=NULL, k=k.fold,
                          create.postproc.out=TRUE, postproc.method=postproc.method, postproc.args=postproc.args)
    message("Suncessful completion of post-processing step")
  }
  dirpop <- getwd()
  setwd(output.dir)
  message("Writing variables")
  postproc.nc <- nc_open(postproc.outfile, write=TRUE)
  postproc.var <- ncvar_def(paste(var, "_postproc"), 
                            units='boolean', 
                            list(postproc.nc$dim$lon, postproc.nc$dim$lat, postproc.nc$dim$time), 
                            prec='float') #Got error when tried to specify 'short'
  postproc.nc <- ncvar_add(postproc.nc, postproc.var, verbose=FALSE)
  print('post-processing values added')
  ncvar_put(postproc.nc, postproc.var, output$postproc.out, verbose=FALSE)
  nc_close(postproc.nc)
  setwd(dirpop)
}

# ####Instructions for a sample run with the 300th lon file
# in.file <- "/work/cew/downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRtxp1-CDFt-A38L01K00/tasmax/RR/OneD/v20140108/tasmax_day_RRtxp1-CDFt-A38L01K00_rcp85_r1i1p1_RR_20060101-20991231.I300_J31-170.nc"
# var='tasmax'
# k.fold=0
# postproc.method='compare.correct'
# out.dir <- "/home/cew/Code/testing/"
# tmask.infile='/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc'
# qc.mask.path <- "/home/cew/Code/testing/QCMasks/tasmax_day_testing-qc-mask4-1pow5-txp1-GFDL-CDFtv1-A00X01K00_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170-kdAdjust-QCMask.nc"
# qc.data.path <- "/home/cew/Code/testing/tasmax_day_testing-simple.bias-1pow5-txp1-GFDL-CDFtv1-A00X01K00_rcp85_r1i1p1_SCCSC0p1_20060101-20991231.I300_J31-170.nc"
# postproc.args=list('na')
# postproc.outfile <- "post-processed-out4.nc"
# pp.out <- postProc.Driver(in.file, var, k.fold, postproc.method, out.dir, tmask.infile, qc.mask.path=qc.mask.path, 
#                           qc.data.path=qc.data.path, postproc.outfile=postproc.outfile)
#####Testing case for 300th lon file and a directory argument
# in.dir <- "/home/cew/Code/testing/input.test/"
# var='tasmax'
# k.fold=0
# lons=300
# lone=301
# postproc.method='compare.correct'
# out.dir <- "/home/cew/Code/testing/"
# tmask.infile='/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc'
# #####tmask.dir='/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc'
# qc.mask.path <- "/home/cew/Code/testing/QCMask.test/"
# qc.data.path <- "/home/cew/Code/testing/compare.test/"
# postproc.args=list('na')
# postproc.outfile <- "post-processed-out5.nc"
# pp.out <- postProc.Driver(in.dir=in.dir, lons=lons, lone=lone, 
#                           tmask.infile=tmask.infile, out.dir=out.dir,
#                           var=var, k.fold=k.fold, postproc.method=postproc.method, qc.mask.path=qc.mask.path, 
#                           qc.data.path=qc.data.path, postproc.outfile=postproc.outfile)