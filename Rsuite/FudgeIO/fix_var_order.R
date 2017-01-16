
fix.var.order <- function(predictor.vars, target.var){
 which.target <- which(predictor.vars==target.var)
 if (length(which.target)==0){
   return(predictor.vars)
 }else if (length(which.target)==1){
   print("Fixing var order so that target var is first")
   pred.index <- c(1:length(predictor.vars))
   return(predictor.vars[c(which.target, pred.index[-which.target])])
 }else{
   stop(paste("Error in fix.var.order: more than one match", 
              'in predictor.vars:', 
              paste(predictor.vars, collapse=",")))
 }
}