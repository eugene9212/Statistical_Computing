newton <- function(f, df, init = 0, maxiter = 100, eps = 1.0e-8)
{
  # Initialization
  x <- init
  iter <- 0
  
  # While loop until it converges
  while (iter < maxiter) {
    new.x <- x - f(x)/df(x)
    if (abs((new.x - x)/x) < eps) break
    iter <- iter + 1
    x <- new.x
  }
  # warning for max.iter
  if (iter == maxiter) warning("maximum iteration reached!")
  
  # return the result
  obj <- list(sol = new.x, iteration = iter)
  return(obj)
}