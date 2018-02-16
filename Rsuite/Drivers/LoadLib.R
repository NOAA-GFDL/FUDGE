LoadLib <- function(ds.method){
  #' Load libraries based on the method used
  # Used by driver scripts
  # ncdf4 is common to all methods as datasets used in FUDGE are netCDF only at this time.
  library(ncdf4)
  library(PCICt)
  library(udunits2)
  library(ncdf4.helpers)
  library(jsonlite)
  library(plyr)
  #library(abind)
  #library('climod', lib.loc = "/home/d1a/R/x86_64-unknown-linux-gnu-library/3.2/")
  library(climod)
  #Note: This package overloads the '@' assignment for non-s4 objects by assigning
  #attribute editing to that operator. If we starting getting '@' issues, start
  #looking at this load first.
  if(grepl('CDFt', ds.method)){
    print("Importing CDFt library")       
    library(CDFt)
  }else if (any(sapply(c("MBC", "QDM"), grepl, ds.method))){
    print("importing MBC")
    library(MBC)
    #library(MBC, lib.loc = '/home/cew/bin/R-3.1.1/library/', logical.return = T)
  }else{
    print("No method-specfic libraries defined in LoadLib.R")  
  }
}  
