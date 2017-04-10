QDM_mod <- function (o.c, m.c, m.p, ratio = FALSE, trace = 0.05, trace.calc = 0.5 * 
            trace, jitter.factor = 0, n.tau = NULL, ratio.max = 2, ratio.max.trace = 10 * 
            trace, ECBC = FALSE, ties = "first", subsample = NULL) 
{
  print("Entering QDM mod")
  if (jitter.factor > 0) {
    o.c <- jitter(o.c, jitter.factor)
    m.c <- jitter(m.c, jitter.factor)
    m.p <- jitter(m.p, jitter.factor)
  }
  if (ratio) {
    epsilon <- .Machine$double.eps
    o.c[o.c < trace.calc] <- runif(sum(o.c < trace.calc), 
                                   min = epsilon, max = trace.calc)
    m.c[m.c < trace.calc] <- runif(sum(m.c < trace.calc), 
                                   min = epsilon, max = trace.calc)
    m.p[m.p < trace.calc] <- runif(sum(m.p < trace.calc), 
                                   min = epsilon, max = trace.calc)
  }
  n <- length(m.p)
  if (is.null(n.tau)) 
    n.tau <- n
  tau <- seq(0, 1, length = n.tau)
  if (!is.null(subsample)) {
    quant.o.c <- rowMeans(apply(replicate(subsample, sample(o.c, 
                                                            size = length(tau))), 2, quantile, probs = tau))
    quant.m.c <- rowMeans(apply(replicate(subsample, sample(m.c, 
                                                            size = length(tau))), 2, quantile, probs = tau))
    quant.m.p <- rowMeans(apply(replicate(subsample, sample(m.p, 
                                                            size = length(tau))), 2, quantile, probs = tau))
  }
  else {
    quant.o.c <- quantile(o.c, tau)
    quant.m.c <- quantile(m.c, tau)
    quant.m.p <- quantile(m.p, tau)
  }
  tau.m.p <- approx(quant.m.p, tau, m.p, rule = 2, method='constant', f=1, ties = "ordered")$y
  if (ratio) {
    approx.t.qmc.tmp <- approx(tau, quant.m.c, tau.m.p, rule = 2, method='constant', f=1, ties = "ordered")$y
    delta.m <- m.p/approx.t.qmc.tmp
    delta.m[(delta.m > ratio.max) & (approx.t.qmc.tmp < ratio.max.trace)] <- ratio.max
    mhat.p <- approx(tau, quant.o.c, tau.m.p, rule = 2, method='constant', f=1, ties = "ordered")$y * 
      delta.m
  }
  else {
    delta.m <- m.p - approx(tau, quant.m.c, tau.m.p, rule = 2, method='constant', f=1, ties = "ordered")$y
    mhat.p <- approx(tau, quant.o.c, tau.m.p, rule = 2, method='constant', f=1, ties = "ordered")$y + 
      delta.m
  }
  mhat.c <- approx(quant.m.c, quant.o.c, m.c, rule = 2, method='constant', f=1, ties = "ordered")$y
  if (ratio) {
    mhat.c[mhat.c < trace] <- 0
    mhat.p[mhat.p < trace] <- 0
  }
  if (ECBC) {
    if (length(mhat.p) == length(o.c)) {
      mhat.p <- sort(mhat.p)[rank(o.c, ties.method = ties)]
    }
    else {
      stop("Schaake shuffle failed due to incompatible lengths")
    }
  }
  list(mhat.c = mhat.c, mhat.p = mhat.p)
}