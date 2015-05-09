# Aparna Radhakrishnan, 08/08/2014
GetCFName <- function(var.name){
  #'Function to retrieve standard_name,long_name and units for a given variable, if available, 
  #'according to the Climate and Forecast (CF) conventions
  #'@param var.name: The short name of the variable being looked up
  #'@return A list of three elements: 'cfname', the CF-compliant standard name, 
  #''cfunits', the CF-compliant units used, and 'cflongname', the conventional 
  #'long name for the variable in question (longnames aren't standardized, but there
  #'are preferred naming conventions)
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
