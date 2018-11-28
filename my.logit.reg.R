my.logit.reg <- function(X, y, beta0 = NULL, max.iter = 100, eps = 1.0e-5) 
{
  if (is.null(beta0)) beta0 <- rep(0, ncol(X))
  beta <- beta0
  for (iter in 1:max.iter)
  {
    theta <- X %*% beta
    p <- exp(theta)/ (1 + exp(theta))
    w <- c(p * (1 - p)) 
    z <- X %*% beta + (y-p)/w 
    
    tilde.X <- X * sqrt(w)
    tilde.z <- z * sqrt(w)
    qr.obj <- qr(tilde.X) # QR decompose
    new.beta <- backsolve(qr.obj$qr, qr.qty(qr.obj, tilde.z))
    
    if (max(new.beta - beta) < eps) break
    beta <- new.beta
  }
  if (iter == max.iter) warning("Algorithm may not be converged!")
  obj <- list(est = c(beta), iterations = iter)
}