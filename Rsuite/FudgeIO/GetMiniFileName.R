GetMiniFileName <- function(variable,freq,model,scenario,ds.region,start.period,end.period,i.index,j.range.suffix){
#'Constructs minifile names in a FUDGE-compliant format
#'from the information contained in the runcode. 
root.filename <- paste(variable,"_",freq,"_",model,"_",scenario,"_",ds.region,"_",start.period,"-",end.period,sep='')
suffix.filename <- paste(".I",i.index,"_",j.range.suffix,".nc",sep='')

filename = paste(root.filename,suffix.filename,sep='')
return(filename)
}
