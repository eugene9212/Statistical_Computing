my.reg <- function(x, y, beta0 = NULL, eps = 1.0e-5,max.iter = 100)
{
  if (is.null(beta0)) beta0 <- rep(0, ncol(x))
  beta <- beta0
  
  for (iter in 1:max.iter){
    mu <- x %*% beta
    z <- x %*% beta + (y-mu)
    
    tilde.x <- x
    tilde.z <- z
    qr.obj <- qr(tilde.x) 
    new.beta <- backsolve(qr.obj$qr, qr.qty(qr.obj, tilde.z))
    
    if (max(new.beta - beta) < eps) break
    beta <- new.beta
  }
  
  if (iter == max.iter) warning("Algorithm may not be converged!")
  
  obj <- list(est = c(beta), iterations = iter)
}