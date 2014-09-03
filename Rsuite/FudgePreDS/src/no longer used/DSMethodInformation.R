#DSMethodInformation.R
#'A source of global information about DS methods, such as which methods
#'support cross-validaiton or which methods rely on future methods for sourcing. 
#'Since these methods could be used by more than one function, 
#'any funciton expecting to use them should source them somehow. 
#'Making them global is a bad idea from a programming perspective, 
#'but it might be the best option.
#'It's a work in progress. 
#'

#Methods for which cross-validation is not supported
no.crossval.possible <<- c("CDFt", "CDFtv1")

#Methods relying on future data both for training and ESD generation
train.and.use.same <<- c("CDFt", "CDFtv1")