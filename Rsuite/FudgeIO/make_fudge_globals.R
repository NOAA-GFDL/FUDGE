make.fudge.globals <- function(){
  #Okay, this might be a simple as grouping all of the
  #available functions in one place to keep them
  #from mucking up the workflow any.
    fudge.title <- sub("  ", " ", paste(get_orig_experiment(MF$metadata[[target.var]]$filename),
                         #Yes, it's MF above - that's what it was originally.
                         #Talk about this with someone else when you do metdata revisions.
                         "downscaled by FUDGE", FUDGETAG,
                         ds.experiment))
    fudge.reference <- NA
    fudge.institution <- paste("NOAA/GFDL", "(201 Forrestal Rd, Princeton, NJ, 08540)")
    fudge.insitute.id <- paste("NOAA/GFDL")
    fudgetag = FUDGETAG
    fudge.history <- make.fudge.history("NOAA/GFDL", FUDGETAG)
    fudge.comment <- make.fudge.comment(ds.method, target.var, grid, ds.region, k.fold, 
                                        ds.experiment, label.validation, label.training, 
                                        predictor.vars, fut.pred.rip.list)

    fudge.info.atts <- make.fudge.info(tmasks$filenames,
                                  missval.args, 
                                  ds.method, args, 
                                  pre_ds, post_ds)
    fudge.OH.atts <- get.OH.experiment.atts(OH$metadata[[1]]$filename)
    fudge.MF.atts <- get.MF.experiment.atts(MF$metadata[[1]]$filename)
    atts.all <- c(fudge.info.atts, fudge.OH.atts, fudge.MF.atts)
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
    return(global_att_list)
}