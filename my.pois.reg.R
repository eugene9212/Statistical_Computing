my.pois.reg <- function(x, y, beta0 = NULL, eps = 1.0e-5,max.iter = 100)
{
  if (is.null(beta0)) beta0 <- rep(0, ncol(x))
  beta <- beta0
  
  for (iter in 1:max.iter)
  {
    eta <- x %*% beta
    p <- exp(eta)
    w <- c(p)
    z <- x %*% beta + (y-p)/w
    
    tilde.x <- x * sqrt(w)
    tilde.z <- z * sqrt(w)
    qr.obj <- qr(tilde.x) 
    new.beta <- backsolve(qr.obj$qr, qr.qty(qr.obj, tilde.z))
    
    if (max(new.beta - beta) < eps) break
    beta <- new.beta
  }
  
  if (iter == max.iter) warning("Algorithm may not be converged!")
  obj <- list(est = c(beta), iterations = iter)
  
  beta.mle <- beta
  
  # output
  return(beta.mle) # mle of beta
}