LoadLib <- function(ds.method){
#' Load libraries based on the method used
# Used by driver scripts
# ncdf4 is common to all methods as datasets used in FUDGE are netCDF only at this time.
  library(ncdf4)
  library(PCICt)
  library(udunits2)
  library(ncdf4.helpers)
  if(grepl('CDFt', ds.method)){
        print("Importing CDFt library")       
	library(CDFt)
  }else{
        print("No method-specfic libraries defined in LoadLib.R")  
  }
}  
