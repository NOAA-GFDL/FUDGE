LoadLib <- function(ds.method){
  #' Load libraries based on the method used
  # Used by driver scripts
  # ncdf4 is common to all methods as datasets used in FUDGE are netCDF only at this time.

  libs <- .libPaths()
	print(libs)
	#libs[length(libs)]
  sys_lib <- libs[length(libs)] #grep("spack",libs, value=TRUE)#libs[length(libs)]
  #print(paste("Getting system packages from: ", sys_lib))
  usr_libs <- c('/home/d1a/R/x86_64-unknown-linux-gnu-library/3.4/',
                '/home/cew/R/x86_64-unknown-linux-gnu-library/3.4/')

  libWrapper <- function(pkg, pkg_lib='none', verbose=F){
    #'Wrapper for loading library functions.
    #'In all cases, we want to try loading from the current system path first;
    #'however,
    if (pkg_lib == 'none'){
      #print(as.character(quote(pkg)))
      library(pkg, character.only = T, lib.loc=sys_lib, verbose=T)
      if (verbose) {print_pkg(pkg, sys_lib)}
      #you MIGHT want to add a command to load from the local libs otherwise,
      #but I think that's going to be prone to posisble issues.
    }else{
      #Assume that if you are passing in a specific path to a package lib,
      #you know that you want to load from this lib (and this lib only)
      library(pkg, character.only = T, lib.loc = pkg_lib, verbose=T)
      if (verbose) {print_pkg(pkg, pkg_lib)}
    }
  }

  #Core packages - we need these for basic
  libWrapper("ncdf4")
  libWrapper("PCICt")
  libWrapper("udunits2")
  libWrapper("ncdf4.helpers")
  libWrapper("jsonlite")
  libWrapper("plyr")

  #ds-method specific packages. If we ever get to that point, s3/s5 opts should
  #probably use something similar.
  if(grepl('CDFt', ds.method)){
    print("Importing CDFt library")
    libWrapper("CDFt")
  }else if (any(sapply(c("MBC", "QDM", "QQDC"), grepl, ds.method))){
    print("importing MBC")
    libWrapper("MBC")
    #library(MBC, lib.loc = '/home/cew/bin/R-3.1.1/library/', logical.return = T)
  }else if (grepl('KDDM', ds.method)){
    libWrapper('climod')
    #libWrapper("climod", pkg_lib = '/home/cew/R/x86_64-unknown-linux-gnu-library/3.4/')
    #Note: This package overloads the '@' assignment for non-s4 objects by assigning
    #attribute editing to that operator. If we starting getting '@' issues, start
    #looking at this load first.
    ## library(quantreg)
    ## libWrapper(KernSmooth) #KernSmooth gets attached, not loaded; can interfere with kde calls
  } else if (grepl('PARM', ds.method)){
    libWrapper("splines") #bs function
    source(file.path(FUDGEROOT, "Rsuite/sec4_DS/PARM/parm_helpers.R"))
  }else{
    print("No method-specfic libraries defined in LoadLib.R")
  }

  #Record the currently loaded packages and their versions
  print(sessionInfo())
}

print_pkg <- function(package, lib){
  print(paste(package,
              packageVersion(package),
              "loaded from", lib))
}
