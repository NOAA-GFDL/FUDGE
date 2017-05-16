make.fudge.globals <- function(){
  #Okay, this might be a simple as grouping all of the
  #available functions in one place to keep them
  #from mucking up the workflow any.
  fudge.title <- sub("  ", " ", paste(get_orig_experiment(MF$metadata[[1]]$filename),
                                      #Yes, it's MF above - that's what it was originally.
                                      #Talk about this with someone else when you do metdata revisions.
                                      "downscaled by FUDGE", FUDGETAG,
                                      ds.experiment))
  fudge.reference <- NA
  fudge.institution <- paste("NOAA/GFDL", "(201 Forrestal Rd, Princeton, NJ, 08540)")
  fudge.insitute.id <- paste("NOAA/GFDL")
  fudgetag = FUDGETAG
  fudge.history <- make.fudge.history("NOAA/GFDL", FUDGETAG, FUDGEROOT)
  fudge.comment <- make.fudge.comment(ds.method, target.var, grid, ds.region, k.fold, 
                                      ds.experiment, 
                                      #label.validation, label.training, 
                                      predictor.vars, fut.pred.rip.list, 
                                      train.string=train.string)
  
  fudge.info.atts <- make.fudge.info(tmasks$filenames, aux.ref,
                                     missval.args,
                                     ds.method, args, 
                                     pre_ds, post_ds)
  fudge.OH.atts <- get.OH.experiment.atts(OH$metadata[[1]]$filename)
  fudge.MF.atts <- get.MF.experiment.atts(MF$metadata[[1]]$filename)
  fudge.file.atts <- get.FUDGE.file.atts(OH$metadata, MH$metadata, MF$metadata)
  atts.all <- c(fudge.info.atts, fudge.OH.atts, fudge.MF.atts, fudge.file.atts)
  #Hey, check on this - is it working as intended?
  fudge.climatology <- MF$metadata[[target.var]]$climatology
  ##############################################################################
  global_att_list <- c(list('title' = fudge.title, 
                            'history' = fudge.history, 
                            'institution' = fudge.institution,
                            "institute_id" = fudge.insitute.id,
                            'comment' = fudge.comment, 
                            'references' = fudge.reference, 
                            'climatology' = fudge.climatology, 
                            'realizations' = paste(fut.pred.rip.list, collapse=","), 
                            'fudgetag' = fudgetag, 
                            'ds_downscaling_method' = ds.ref$method), 
                       atts.all)
  #Check for attributes that may not be present
  #climatology and trend
  clim.nc <- nc_open(MF$metadata[[1]]$filename)
  clim.att <- ncatt_get(clim.nc, 0, 'climatology')
  trend.att <- ncatt_get(clim.nc, 0, 'trend')
  nc_close(clim.nc)
  if (clim.att$hasatt){
    global_att_list$climatology <- clim.att$value
  }
  if (trend.att$hasatt){
    global_att_list$trend <- trend.att$value
  }
  return(global_att_list)
}

get.git.branch <- function(git.dir, verbose=FALSE){
  #'Code for getting the branch information and current commit
  #'of the branch from within R. Returns a string of the branch info.
  #popdir <- getwd()
  #setwd(git.dir)
  #'CEW notes 1-29-16: 
  #'there is an itermittent bug associated with hanging during one
  #'of the system2 calls during this step. I haven't been able to 
  #'duplicate it intentionally (it ALWAYS works the second time), 
  #'but keep an eye out for its presence. 
  #'It's definetly the system2 calls; I was running a profiler during one
  #'of these, and you could see the system2 column incrementing. It got
  #'up to 15 min. before it was stopped. 
  #'Also note: 
  #'system2 does not accept a crtl+c exit; you need to set the process to 
  #'sleep (ctrl+z) and THEN attemtp to kill it via top or ps.
  #'posttble factors to examine (with answers as of 1-31-17): 
  #'   is there more than one branch of git loaded? 
  #'   --nope
  #'   was R 3.2.1 loaded instead of R/2.15.2?
  #'   --yes, intentionally. Trying again with 2.5
  #'   How busy was the analysis node? 
  #'   -- I'm the busiest thing on it - I'm taking up 99.9 %
  #'   Does opening up another terminal and running the command manually still
  #'   give the error?
  #'   -- opening another R session in the same terminal does not; calling the 
  #'   R script from the command line does.
  #'After talking with Tim Yeager on 02-06-17, have come to the conclusion that
  #'it may be a memory issue - large caches in R lead to lack of cache for system, 
  #'which leads to a very long time to wait on the git commands. 
  #'I'm not sure what a permanent fix is going to look like; in the short-mid
  #'term, he suggested using psmem and/or jobs to keep track of memory usage
  #'by the system, and do a find on the git directory when total system
  #'memory was high (get files back in cache).  
  #'Also note that you can kill suspended processes with 'kill %1' in the c-shell
  #'if jobs turns up [1]
  if(verbose){message("Getting git branch information")}
  branch.default <- "Branch timeout; no info returned"
  commit.default <- "Commit timeout; no info returned"
  branch.string <- system2('git', 
                           c('-C', git.dir, 'symbolic-ref HEAD --short'), 
                           stdout=T)
  commit.string <- system2('git', 
                           c('-C', git.dir, 
                             'describe --always --tag'), 
                           stdout=T)
  return(paste(branch.string, commit.string))
}