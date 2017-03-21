GetMiniFileName <- function(variable,freq,model,scenario,ds.region,start.period,end.period,
                            i_idx, i_stride, i_offset, j_idx, j_stride, j_offset){
#Constructs minifile names
root.filename <- paste(variable,"_",freq,"_",model,"_",scenario,"_",ds.region,"_",start.period,"-",end.period,sep='')
#suffix.filename <- paste(".I",i.index,"_",j.range.suffix,".nc",sep='')
suffix.filename <- paste0(".I", i_idx + i_offset, "-", i_idx + i_stride + i_offset - 1, 
                          "_J", j_idx + j_offset, "-", j_idx + j_stride + j_offset - 1, 
                          ".nc")
filename = paste(root.filename,suffix.filename,sep='')
print(filename)
#stop('examine this output')
return(filename)
}
