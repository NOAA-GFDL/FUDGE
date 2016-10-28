#Driver function for looping over x,y,tmask points
#Currently called from global environment in master driver

loop.xy.data <- function(){
  #This is with the new downscaling object structure
  x.len <- unique(sapply(list(oh.coords$x.start, 
                              mh.coords$x.start, mf.coords$x.start), 'length'))
  y.len <- unique(sapply(list(oh.coords$y.start, 
                              mh.coords$y.start, mf.coords$y.start), 'length'))
  OH.all.dim <- dim(OH$data[[1]])[1:3]
  MF.all.dim <- dim(MF$data[[1]])[4:6]
  output.all <- rep(list(array(NA, dim=c(OH.all.dim, MF.all.dim))), 
                    length(c("ds")))
  print(dim(output.all[[1]]))
  names(output.all) <- c("ds")
  for (ival in 1:x.len){
    message(paste("Starting i loop", ival, 'of', x.len))
    ival <<- ival
    for(jval in 57:57){ #1:y.len
      message(paste("Starting j loop", jval, 'of', y.len))
      jval <<- jval
      OH.slice <- get.xy.slice(OH$data, oh.coords, ival, jval)
      MH.slice <- get.xy.slice(MH$data, mh.coords, ival, jval)
      MF.slice <- get.xy.slice(MF$data, mf.coords, ival, jval)
      ##
      ds.x.start <- oh.coords$x.start[ival]
      ds.x.end   <- oh.coords$x.end[ival]
      ds.y.start <- oh.coords$y.start[jval]
      ds.y.end   <- oh.coords$y.end[jval]
      ds.names <- "ds"
      qc.mask.names <- c()
      #qc.mask.names <- c('tasmax_qcmask_train', 'tasmax_qcmask_tailadj')
      all.names <- c(ds.names, qc.mask.names)
      OH.dim <- dim(OH.slice[[1]])[1:3]
      MF.dim <- dim(MF.slice[[1]])[4:6]
      nmasks <- tmasks$nmasks
      #nmasks is set by the time mask tools
      #...how should this interact with the main driver? 
      #ds as a target is currently being set by the 
      output <- rep(list(array(NA, dim=c(OH.dim, MF.dim, nmasks))), 
                    length(all.names))
      #tmasks <- tmask.list
      time_mask_overlap = tmasks$time_mask_overlap
      ds.names <- c('MF')
      names(output) <- all.names
      
      if ( !any(c( sapply(OH.slice, missing.all.xy), 
                   sapply(MH.slice, missing.all.xy), 
                   sapply(MF.slice, missing.all.xy))
      )){
        
        #2016-10-26: Adding in s3/s5 ajdustment calls outside of 
        #looping by time window
        message('Looking for pre-processing functions to apply')
        if(length(pre.ds) !=0){
          message('Applying S3 Adjustment')
          temp.output <- callS3Adjustment(s3.instructions=pre.ds, 
                                          MH=MH.slice, 
                                          OH=OH.slice,
                                          MF=MF.slice,
                                          #this really needs a metadata call of some sort.
                                          #how to pass it in? Pointer in global environment?
                                          adjust.metadata,
                                          s5.instructions=post.ds, 
                                          missval.args = missval.args)
#           print(summary(temp.output))
#           print(lapply(temp.output, summary))
          #Assign output and remove temporary output
          missval.args <- temp.output$missval.args
          post.ds <- temp.output$s5.list
          OH.slice <- temp.output$input$OH
          MH.slice <- temp.output$input$MH
          MF.slice <- temp.output$input$MF
          if (!is.null(temp.output$input$qc)){
            for (el in names(temp.output$qc)){
              ds[[el]] <- temp.output$qc[[el]]
            }
          }
          remove(temp.output)
        }
#         print(summary(OH.slice))
#         print(summary(MH.slice))
#         print(summary(MF.slice))
        #Loop by time window
        for(w in 1:nmasks){
          print(paste("Starting window", w, "of", nmasks))
          w <<- w
          qc.out <- NULL
          if (tmasks$apply_tmask){
            OH.window <- lapply(OH.slice, "apply.fudge.tmask", tmasks$OH$masks[[w]])
            MH.window <- lapply(MH.slice, "apply.fudge.tmask", tmasks$MH$masks[[w]])
            MF.window <- lapply(MF.slice, "apply.fudge.tmask", tmasks$MF$masks[[w]])
          }else{
            OH.window <- OH.slice
            MH.window <- MH.slice
            MF.window <- MF.slice
          }
          
          #Call section 3 methods; reassign output
          #           if (s3.inloop){
          #                 tmp.s3 <- callS3Adjustment(s3.instructions, 
          #                                            list(data=MH.window, 
          #                                                 metadata=OH$metdata), 
          #                                            list(data=OH.window, 
          #                                                 metadata=OH$metadata), 
          #                                            list(data=MF.window,
          #                                                 metadata=MF$metadata),
          #                                            s5.instructions, missval.args)
          #                 #s3 functions only return data, not metadata
          #                 #(there's only one function that changes metadata, and it is)
          #                 #dealt with by adding elements to the stack
          #                 OH.window <- tmp.s3$OH
          #                 MH.window <- tmp.s3$MH
          #                 MF.window <- tmp.s3$MF
          #                 s5.out <- tmp.s3$s5
          #                 if ('OH' %in% ds.names){
          #                   #Correction to remove leapdays present in OH but not MF
          #                   #This get used for Delta and CFQM
          #                   tmp.ds.time.mask <- apply.any.mask(data = mask.struct[[2]]$masks[[w]], 
          #                                                      mask=delta.mask, 
          #                                                      dim='temporal')
          #                   OH.window <- apply.any.mask(data=OH.window, mask=tmp.ds.time.mask)
          #                 }
          #           }      
          #Call the downsaing method
          print('calling ds')
          tmp.ds <- callDSMethod(ds.method, 
                                 OH.window, MH.window, MF.window, args, 
                                 missvals=ds.ref$missval_method, 
                                 minsample=1, 
                                 missval.args=missval.args,
                                 tmasks=tmasks)
#           if(time_mask_overlap){
#             #ds.out <- apply.any.mask(tmp.ds$ds, trim.time.mask$masks[[w]])
#             ds.out <- list(ds=apply.fudge.tmask(data=tmp.ds$ds, 
#                                                 mask=tmasks$trim$masks[[w]]))
#             qc.names <- names(tmp.ds)[names(tmp.ds)!='ds']
#             for (el in qc.names){
#               if(!is.null(tmp.ds[[el]])){
#                 qc.out <- c(qc.out, 
#                             lapply(tmp.ds[[el]], #all non-ds outputs
#                                    'apply.fudge.tmask',         #gets same mask applied
#                                    mask=tmasks$trim$masks[[w]])
#                 )
#               }
#             }
#          }else{
            ds.out <- list(ds=tmp.ds$ds)
            qc.out <- c(qc.out, tmp.ds$qc)
            
#          }
          #       #Call section 5 methods
          #       tmp.s5 <- callS5Adjustment(s5.instructions=s5.out,
          #                                  data = ds.out, 
          #                                  hist.targ = OH.window,
          #                                  hist.pred = MH.window, 
          #                                  fut.pred  = MF.window)
          #       ds.out <- tmp.s5$ds
          #       qc.out <- c(qc.out, tmp.s5$qc)
          #       #...hm. Is it possible to adapt that OU code that included window
          #       #as an extra dimension and include it for multiple qc masks?
          combine.out <- c(ds.out, qc.out)
          for (out in names(combine.out)){
            print(paste("looping by out with", out))
            if (tmasks$time_mask_overlap){
              output[[out]][,,,,,,w] <- apply.fudge.tmask(data=combine.out[[out]], 
                                                          mask=tmasks$trim$masks[[w]])
            }else{
              output[[out]][,,,,,,w] <- combine.out[[out]]
            }
          }
        }#Loop by w complete
        ##bring together all time levels
        for (el in names(output)){
          tmp.out <- apply(output[[el]], c(1,2,3,4,5,6), sum, na.rm=T)
          output.all[[el]][ds.x.start:ds.x.end, ds.y.start:ds.y.end,,,,] <- tmp.out
        }
        #Once looping by w complete, do the last s5 adjustment
        #Call the Section 5 Adjustments to be applied to post-downscaled output
        message("Calling Section 5 Adjustments")
        if(length(post.ds) !=0){
          temp.postproc <- callS5Adjustment(post.ds,
                                            data = output.all$ds[ds.x.start:ds.x.end, ds.y.start:ds.y.end,,,,],
                                            hist.pred = MH.window,
                                            hist.targ = OH.window,
                                            fut.pred = MF.window)
          #Your return should include a list of two elements - modified ds out, 
          #and a list of qc masks
          output.all$ds[ds.x.start:ds.x.end, ds.y.start:ds.y.end,,,,] <- temp.postproc$ds.out
          if(qc.maskopts$qc.outloop){
            output.all$qc[ds.x.start:ds.x.end, ds.y.start:ds.y.end,,,,] <- temp.postproc$qc.mask
          }
          remove(temp.postproc)
        }
        print(summary(as.vector(output.all$ds)))
        print(dim(output.all$ds))
#         print(summary(output.all$ds[1,1,,1,,]))
#         print(summary(output.all$ds[1,1,,2,,]))
#         print(summary(output.all$ds[1,1,,3,,]))
#         stop('examine output')
      } 
    }#loop by y
  }#loop by x
# print(summary(as.vector(output.all$ds[1,,,1,,])))
# print(summary(as.vector(output.all$ds[1,,,2,,])))
# print(summary(as.vector(output.all$ds[1,,,3,,])))
# print(summary(as.vector(output.all$ds)))
# stop('examine output')
  return(output.all)
}

combine.over.missvals <- function(x){
  #Combines a vector with a set of values that do not overlap into a 
  #single vector of data; if missing values overlap, returns an error.
  #Not currently used by anything because it isn't working; needs either 
  #a version of this or the addition that makes all missing values NA for the
  #full workflow.
  which.notna <- which(!is.na(x))
  if (length(which.notna)==1){
    return(which.notna)
  }else if (length(which.notna)==0){
    return(NA)
  }else{
    stop(paste("Error in combine.over.missvals: more than one val was not missing!"))
  }
}

missing.all.xy <- function(dataset){
  #Convenience function for checking whether there are any 
  #non-missing values at each x,y point to downscale over.
  #Missing all values at any point is enough to disqualify
  #the x,y loop from downscaling.
  return(as.vector(apply(dataset, c(1,2), 
                         function(x){
                           !any(!is.na(x))
                         })
  ))
}


