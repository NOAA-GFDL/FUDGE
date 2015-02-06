library(ncdf4)
sapply(list.files(pattern="[.]R$", path="/home/a1r/gitlab/fudge2014/Rsuite/FudgeIO/src/", full.names=TRUE), source);

#' Example 1
#' print("example 1")
#' nc.object = OpenNC("/home/a1r/gitlab/fudge2014/Rsuite/sampleNC/","tasmax_day_GFDL-HIRAM-C360-COARSENED_amip_r1i1p1_19790101-20081231.I748_J454-567.nc")

#' Example 
nc.object = OpenNC("/home/a1r/gitlab/fudge2014/Rsuite/sampleNC/","tasmax_day_GFDL-HIRAM-C360-COARSENED_amip_r1i1p1_19790101-20081231",
                   ilon="748",jlat="454-567")
var.read <- ReadNC(nc.object,var.name='tasmax',dstart=c(1,1,1),dcount=c(1,1,1))
print(var.read)
