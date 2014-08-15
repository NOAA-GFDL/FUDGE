#ESD.Train.Totally.Fake.R
#Placeholder ESD training code for purposes of testing.

#TODO: Talk to Carlos about the expected output of CDFt
#Will a function always be returned from downscaling methods?

ESD.Train.totally.fake <- function(pred, targ){
  lm.results <- lm(targ ~ pred)
  lm.intercept <- lm.results$coefficients[1]
  lm.slope <- lm.results$coefficients[2]
  trained.function<-function(x){
    return( lm.intercept + x*lm.slope)
  }
  return(trained.function)
}