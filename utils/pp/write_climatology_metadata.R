#!/usr/local/bin/Rscript

library(ncdf4)
library(ncdf4.helpers)
base.path <- Sys.getenv("S8_PATH")
if(base.path==""){
  #If S8_tools not set, assume that you are working from the correct directory
  base.path <- getwd()
}
#Note: if you use this strucutre, it is VITAL that you do not include ANY scripts in the directories that
#you are sourcing. Those scripts will run, and you will be frustrated.
out <- source(paste0(base.path,'/s8_utils/LoadRPackages.R'))
out <- sapply(list.files(pattern="[.]R$", path=paste0(base.path,'/shared_code/FudgeIO/'), full.names=TRUE), source, .GlobalEnv)
out <- source(paste0(base.path,'/shared_code/R_utils.R'));
out <- sapply(list.files(pattern="[.]R$", path=paste0(base.path,'/s8_utils/'), full.names=TRUE), source, .GlobalEnv);


SetAnomMetadata <- function(clim_file, anom_file, fvar, out_file){
  #...I can probably get away with not cloning the metadata.
  clim.dset.name <- get.dataset.name(clim_file)
  anom.dset.name <- get.dataset.name(anom_file)
  clim.nc <- nc_open(clim_file)
  anom.nc <- nc_open(anom_file)
  sbx.att <- ncatt_get(clim.nc, 0, 'Sbx')
  if (sbx.att$hasatt){
    sbx.num <- gsub("[^0-9]", "", sbx.att$value, "")
    sbx.string <- paste("smoothed", sbx.num, "days")
  }else{
    sbx.string <- ""
    sbx.num=1
  }
  clim.datestring <- ncatt_get(clim.nc, 'time', 'modulo_range')
  if (!clim.datestring$hasatt){
    clim.title <- ncatt_get(clim.nc, 0, 'title')
    date.string <- sub('.*(\\d{4})-(\\d{4}).*', '\\1-\\2', clim.title$value)
  }else{
    date.string <- clim.datestring$value
  }
  clim.att <- ncatt_get(anom.nc, 0, 'climatology')
  if (clim.att$hasatt){
    clim.orig <- clim.att$value
  }else{
    clim.orig <- ""
  }
  #And our one-and-only instance of a variable att:
  #Note: not varAnom, because this got renamed in the earlier step
  orig.longname <- ncatt_get(anom.nc, paste0(fvar, "Clim"), 'long_name')$value
  print(orig.longname)
  new.longname <- str_trim(sub("anomaly[ of]*", "", orig.longname, ignore.case=TRUE, perl=TRUE))
  new.longname <- sub(fvar, paste(fvar, "(anom + clim)"), new.longname, ignore.case=TRUE)
  if(!grepl("(anom + clim)", new.longname, fixed=T)){
    new.longname <- paste(new.longname, "(anom + clim)")
  }
  nc_close(clim.nc)
  nc_close(anom.nc)
  
  var_atts <- list(long_name = new.longname)
  global_atts <- list(institution='NOAA-GFDL', 
                      title=paste("Sum of anomaly", anom.dset.name, 
                                  "relative to", date.string, 
                                  "and climatology", clim.dset.name, 
                                  sbx.string), 
                      s8_tag = paste(get.git.branch(base.path)), 
                      climatology_added = clim.orig)
  #Set new metadata atts of the output file
  out.nc <- nc_open(out_file, write = T)
  for (n in names(var_atts)){
    ncatt_put(out.nc, fvar, n, var_atts[[n]])
  }
  for (n in names(global_atts)){
    ncatt_put(out.nc, 0, n, global_atts[[n]])
  }
  nc_close(out.nc)
}

SetResidMetadata <- function(trend_file, resid_file, fvar, trendvar, residvar, out_file){
  #...I can probably get away with not cloning the metadata.
  trend.dset.name <- get.dataset.name(trend_file)
  resid.dset.name <- get.dataset.name(resid_file)
  trend.nc <- nc_open(trend_file)
  resid.nc <- nc_open(resid_file)
  smooth.att <- ncatt_get(trend.nc, 0, 'smooth')
  if (smooth.att$hasatt){
    smooth.num <- gsub("[^0-9]", "", smooth.att$value, "")
    smooth.string <- paste("smoothed", smooth.num, "days")
  }else{
    smooth.string <- ""
    smooth.num=1
  }
  trend.datestring <- ncatt_get(trend.nc, 'time', 'modulo_range')
  if (!trend.datestring$hasatt){
    trend.title <- ncatt_get(trend.nc, 0, 'title')
    date.string <- sub('.*(\\d{4})-(\\d{4}).*', '\\1-\\2', trend.title$value)
  }else{
    date.string <- trend.datestring$value
  }
  trend.att <- ncatt_get(resid.nc, 0, 'trend')
  if (trend.att$hasatt){
    trend.orig <- trend.att$value
  }else{
    trend.orig <- ""
  }
  #And our one-and-only instance of a variable att:
  #Note: not varAnom, because this got renamed in the earlier step
  orig.longname <- ncatt_get(resid.nc, trendvar, 'long_name')$value
  print(orig.longname)
  new.longname <- str_trim(sub("residuals[ of]*", "", orig.longname, ignore.case=TRUE, perl=TRUE))
  new.longname <- sub(fvar, paste(fvar, "(resid + trend)"), new.longname, ignore.case=TRUE)
  if(!grepl("(resid + trend)", new.longname, fixed=T)){
    new.longname <- paste(new.longname, "(resid + trend)")
    
    nc_close(trend.nc)
    nc_close(resid.nc)
    
    var_atts <- list(long_name = new.longname)
    global_atts <- list(institution='NOAA-GFDL', 
                        title=paste("Sum of residuals", resid.dset.name, 
                                    "relative to", date.string, 
                                    "and trend", trend.dset.name, 
                                    smooth.string), 
                        s8_tag = paste(get.git.branch(base.path)), 
                        trend_added = trend.orig)
    #Set new metadata atts of the output file
    out.nc <- nc_open(out_file, write = T)
    for (n in names(var_atts)){
      ncatt_put(out.nc, fvar, n, var_atts[[n]])
    }
    for (n in names(global_atts)){
      ncatt_put(out.nc, 0, n, global_atts[[n]])
    }
    nc_close(out.nc)
  }
}

################################################################################
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0){
  args <- "none"
}
if (args[1] == "anom"){
  SetAnomMetadata(args[2], args[3], args[4], args[5])
}else if (args[1] == 'trend'){
  SetResidMetadata(args[2], args[3], args[4], args[5], args[6], args[7])  
}

