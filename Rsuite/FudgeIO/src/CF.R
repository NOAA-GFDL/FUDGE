GetCFName <- function(var.name){
  #'Function to retrieve standard_name,long_name and units for a given variable, if available
  #' TODO : Interface with udunits2 and [cf-units]
  cflist <- ("none") 
  if((var.name == "tasmax") | (var.name == "tasmin")){
     cfname <- "air_temperature"
     cfunits <- "K"
     if(var.name == "tasmax"){
     cflongname <- "Daily Maximum Surface Temperature"	
     }else{
     cflongname <- "Daily Minimum Surface Temperature"
     }
  cflist <- list("cfname" = cfname,"cfunits" = cfunits, "cflongname" = cflongname )		
  }

return(cflist)
}
