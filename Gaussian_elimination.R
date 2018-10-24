# Statistical Computing Method (1) Gaussian Elimination #
# Gaussian Elimination with Parital Pivoting #

GE <- function(A, b){
  n <- dim(A)[1]
  
  # combine A and b
  Ab <- cbind(A, b)
  
  for(i in 1:(n-1)){
    
    # calculate the order
    ord <- order(abs(Ab[i:n,i]), decreasing = TRUE) + (i - 1)
    
    if (i == 1){
      ord <- ord
    } else {
      k <- i-1
      ord <- c(seq(1:k),ord)
    }
    d <- diag(1, nrow = n, ncol = n)    
    
    # create permutation matrix
    P <- d[ord,]
    
    # permute
    Ab <- P %*% Ab
    
    # create elimination matrix
    d[(i+1):n, i] <- outer(-Ab[(i+1):n, i], Ab[i, i], "/")
    
    # eliminate
    Ab <- d %*% Ab
  }
  
  x <- backsolve(Ab[,1:n], Ab[,n+1], upper.tri = TRUE)
  return(x)
}

GE(A,b)



# Practice ----------------------------------------------------------------

rm(list=ls())
A <- matrix(c(1, 2, -1, 0,
              1/2, 1, 0, 1,
              0, 2, -1/2, 3/2,
              1, -1, 3/2, 0), nrow = 4, ncol = 4, byrow = TRUE)
A
b <- matrix(c(1/2, 1, 3/2, 2), nrow = 4, ncol = 1, byrow = FALSE)
b
A <- matrix(c(10, 24, -1, 0,
              12, 11, 0, 1,
              0, 2, -12, 3/2,
              1, -1, 32, 0), nrow = 4, ncol = 4, byrow = TRUE)
A
b <- matrix(c(1/2, 1, 3/2, 2), nrow = 4, ncol = 1, byrow = FALSE)
b

GE(A,b)
