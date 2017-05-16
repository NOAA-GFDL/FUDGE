
find.thold.orig <- function(OH.frac, OH.var, MH.var){
  OH.idx <- suppressWarnings(!is.na(OH.var))
  MH.idx <- suppressWarnings(!is.na(MH.var))
  first.above.threshold <- quantile(OH.var[OH.idx],  probs=(1.0-OH.frac),na.rm=TRUE)
  small.fraction <- 1.0/sum(OH.idx)
  last.below.threshold <- quantile(OH.var[OH.idx], probs=(1.0-OH.frac-small.fraction), na.rm=TRUE)
  num.zero.in.adjusted <- ((1.0-OH.frac)*sum(MH.idx)) - 1
  threshold.wetday.adj <- quantile(MH.var[MH.idx], names = FALSE,
                                   probs=(num.zero.in.adjusted/sum(MH.idx)), 
                                   na.rm=TRUE)
  return(threshold.wetday.adj)
}

find.thold.kd <- function(OH.frac, OH.var, MH.var){
  MH.sort <- sort(MH.var[!is.na(MH.var)], decreasing=T)
  print(summary(MH.var))
  adj.idx <- floor(OH.frac * length(MH.sort))
  #   print(adj.idx)
  #   print(MH.sort[(adj.idx-3):(adj.idx+3)])
  adj.trace <- mean(MH.sort[c(adj.idx, adj.idx + 1)])
  return(adj.trace)
}
