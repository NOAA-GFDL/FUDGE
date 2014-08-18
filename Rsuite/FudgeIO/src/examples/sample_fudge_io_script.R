#Simple test code to make sure that behavior of
#read/write fxns is understood

source('~/Code/fudge2014/Rsuite/FudgeIO/src/WriteNC.R')
source('~/Code/fudge2014/Rsuite/FudgeIO/src/ReadNC.R')
source('~/Code/fudge2014/Rsuite/FudgeIO/src/CF.R')
source('~/Code/fudge2014/Rsuite/FudgeIO/src/OpenNC.R')

#grab an NC object
nc.mine <- OpenNC("/home/cew/Code/climdex/","tasmax_sample_input.nc")
#get a data point
var.single <- ReadNC(nc.mine,"tasmax",c(1,1,1),c(1,1,1))
print(var.single)
#Get a data slice
var.lonslice <- ReadNC(nc.mine,"tasmax",c(1,1,1),c(1,114,1))
print(length(var.lonslice))
print(dim(var.lonslice)) 
#make alteration to data and write
var.new<-ReadNC(nc.mine,"tasmax")
var.new[1,]<-NA
dim(var.new) <- c(1,1,1,114, 3652)
WriteNC("fudgeio_sample_output.nc",var.new,"tasmax" #Cf-checker removes 'tasmax_tweaked'
        ,1,seq(1:114),1,3652,    #Note indexes from 1, not 0. No dim checking. 
        1969,"","gregorian")
