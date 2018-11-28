bisection <- function(f, a, b, maxiter = 100, eps = 1.0e-5) {
  
  # Check for initial condition
  if (f(a) * f(b) > 0) stop("f(a) and f(b) must have different signs")
  
  # Initial Set up
  u <- b
  l <- a
  iter <- 0
  
  # While loop until it converges
  while (iter < maxiter) {
    m <- (u + l)/2
    if (f(u) * f(m) < 0) {
      l <- m
    } else if (f(l) * f(m) < 0) {
      u <- m
    } else {
      break
    }
    if (abs(l - u) < eps) break
    iter <- iter + 1
  }
  
  # warning when it reaches max iter
  if (iter == maxiter) warning("maximum iteration reached!")
  
  # return the result
  obj <- list(sol = m, iteration = iter)
  return(obj)
}