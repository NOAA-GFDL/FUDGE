GetMiniFileName <- function(variable,freq,model,scenario,ds.region,start.period,end.period,
                            i_idx, i_stride, i_offset, i_max, 
                            j_idx, j_stride, j_offset, j_max){
#Constructs minifile names
root.filename <- paste(variable,"_",freq,"_",model,"_",scenario,"_",ds.region,"_",start.period,"-",end.period,sep='')
#suffix.filename <- paste(".I",i.index,"_",j.range.suffix,".nc",sep='')
i_coord = i_idx + i_offset - 1
i_end = min(c(i_coord + i_stride - 1, i_offset + i_max - 1))
j_coord = j_idx + j_offset - 1
j_end = min(c(j_coord + j_stride - 1, j_offset + j_max - 1))  
suffix.filename <- paste0(".I", i_coord, "-", i_end, 
                          "_J", j_coord, "-", j_end, 
                          ".nc")
filename = paste(root.filename,suffix.filename,sep='')
print(filename)
#stop('examine this output')
return(filename)
}
