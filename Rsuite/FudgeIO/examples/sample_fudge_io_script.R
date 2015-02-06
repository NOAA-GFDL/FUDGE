#Simple test code to make sure that behavior of
#read/write fxns is understood

basedir <- "/home/cew/Code/fudge2014/"
setwd(basedir)
source(paste(basedir, "Rsuite/FudgeIO/src/WriteNC.R", sep=""))
source(paste(basedir,'Rsuite/FudgeIO/src/ReadNC.R', sep=""))
source(paste(basedir,'Rsuite/FudgeIO/src/CF.R', sep=""))
source(paste(basedir,'Rsuite/FudgeIO/src/OpenNC.R', sep=""))

#grab an NC object
nc.mine <- OpenNC("/home/cew/Code/climdex/","tasmax_sample_input.nc")
#get a data point
var.single <- ReadNC(nc.mine,"tasmax",c(1,1,1),c(1,1,1))
print(var.single)
# Get a data slice. Note that the file needs to be re-opened
# since ReadNC closes the nc object upon execution
nc.mine <- OpenNC("/home/cew/Code/climdex/","tasmax_sample_input.nc")
var.lonslice <- ReadNC(nc.mine,"tasmax",c(1,1,1),c(1,114,1))
print(length(var.lonslice))
print(dim(var.lonslice)) 
#make alteration to data and write
nc.mine <- OpenNC("/home/cew/Code/climdex/","tasmax_sample_input.nc")
var.new<-ReadNC(nc.mine,"tasmax")
#Making changes to data and changing the shape of the variables
var.new[1,]<-NA
dim(var.new) <- c(1,1,1,114, 3652) 
WriteNC("fudgeio_sample_output.nc",var.new,"tasmax" #Cf-checker removes 'tasmax_tweaked'
        ,1,seq(1:114),1,3652,    #Note indexes from 1, not 0. No checking for degen dims. 
        1969,"","gregorian")
