# Statistical Computing Method (2) Cholesky Decomposition #

fn.chol <- function(A)
{
  n <- ncol(A)
  
  # initalization
  Lk <- sqrt(A[1,1])
  
  # from 2nd column,
  k <- 2
  for (k in 2:n) {
    ak <- A[1:(k-1),k]
    l.dfk <- forwardsolve(Lk, ak) # for (k by others) element of L
    l.kk <- sqrt(A[k,k] - crossprod(l.dfk, l.dfk)) # for (k by k) element of L
    Lk <- rbind(cbind(Lk, rep(0, k-1)), c(l.dfk, l.kk))
    print(Lk)
  }
  L <- Lk
  
  return(L)  
}

# Practice ----------------------------------------------------------------

A <- matrix(c(4, 2, 2, 4,
              2, 5, 7, 0,
              2, 7, 19, 11,
              4, 0, 11, 25), nrow = 4, ncol = 4, byrow = TRUE)
A

L <- fn.chol(A)
L
A == L%*%t(L)

# Compare with generic function in R ---------------------------------------
chol(A)
fn.chol(A)
