GetMiniFileName <- function(variable,freq,model,scenario,ds.region,start.year,end.year,i.index,j.range.suffix){
#Constructs minifile names
root.filename <- paste(variable,"_",freq,"_",model,"_",scenario,"_",ds.region,"_",start.year,"0101","-",end.year,"1231",sep='')
suffix.filename <- paste(".I",i.index,"_",j.range.suffix,".nc",sep='')

filename = paste(root.filename,suffix.filename,sep='')
return(filename)
}
