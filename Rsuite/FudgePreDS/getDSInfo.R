#Genereal code for getting ds method attributes from an external spreadsheet
#with known formatting

getDSInfo <- function(ds.method){
#  ds_sheet = "/home/cew/Code/h_testing//FudgeIO_reboot//ds_sheet//ds_att_sheet.csv"
  #ds_sheet = "/home/cew/Code/fudge/Rsuite/aux/ds_att_sheet.csv"
  ds_sheet = file.path(FUDGEROOT, "/Rsuite/aux/ds_att_sheet.csv")
  input <- read.csv(ds_sheet, header=T, stringsAsFactors=F)
  method.names <- input$method_name
  if (! ds.method %in% method.names){
    stop(paste("Error in getDSInfo: method", ds.method, 
               "was not found in the metadata spreadshet!"))
    
  }
  ds.idx <- which(ds.method == method.names)
  print(method.names)
  print(ds.idx)
  print(names(input))
  ds.object <- list('method'         = ds.method, 
                    'downscale_oh'   = eval(parse(text=input$downscale_oh[ds.idx])),
                    'predictors'     = csplit(input$predictor_datasets[ds.idx]), 
                    'target'         = input$target_dataset[ds.idx], 
                    'training'       = csplit(input$train_datasets[ds.idx]),
                    'downscaling'    = input$downscale_dataset[ds.idx],
                    'missval_method' = input$missval_method[ds.idx], 
                    'multipt_method' = input$multipt_method[ds.idx], 
                    'multivar_target'= input$multivar_target[ds.idx],
                    'is_qm'          = eval(parse(text=input$is_qm[ds.idx])), 
                    'is_pairwise'    = eval(parse(text=input$is_pairwise[ds.idx])), 
                    'adjust_pr_in_window' = eval(parse(text=input$adjust_pr_in_window[ds.idx])), 
                    'missval_method' = input$missval_method[ds.idx],
                    'oh_dimlist' = csplit(input$oh_dimlist[ds.idx]), 
                    'mh_dimlist' = csplit(input$mh_dimlist[ds.idx]), 
                    'mf_dimlist' = csplit(input$mf_dimlist[ds.idx]), 
                    'reference' = input$reference[ds.idx])
  #Set properties that depend on logic from sheet
  ds.object['train_and_use_same'] = ds.object$target %in% ds.object$predictors
  #Set delayed assign for getting the target dataset 
  #in the main driver loop and the dimensions of the 
  #target data
  ##Note: turned off for the moment; replaced with an assignment
  ## in the main driver. Being clever is not working.
#   delayedAssign('TARG', eval(parse(text=ds.object$target)))
#   targ_dimstr = switch(ds.object$target, 
#                'OH' = 'oh.dim', 
#                'MH' = 'mh.dim', 
#                'MF' = 'mf.dim')
#   delayedAssign('targ.dim', eval(parse(text='oh.dim')), 
#                 eval.env=globalenv(), assign.env=globalenv())
  return(ds.object)
}

csplit <- function(str){
  #Wrapper for splitting a function by a string
  #and returning a vector of the split
  if (is.null(str) || is.na(str)) {return(NA)}
  out <- unlist(sapply(str, "strsplit", ",", USE.NAMES = F), use.names = F)
  out <- sub(" ", "", out)
  return(out)
}

