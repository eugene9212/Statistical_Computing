#### Bisection and Newton Method ####
## Bisection : for root finding
## Newton(1) : for root funding
## Newton(2) : for optimization -> also called "Newton Raphson/Newton," 
#                                since the algorithm for opt. is same as root finding newton method.

rm(list = ls())

#### load packages & R code ####
setwd('C:/Users/eugene/Desktop/Statistical_Computing/Statistical_Computing')
source('bisection.R') # function for bisection
source('newton.R')    # function for root finding

# Create Target function for Root finding
f <- function(x) cos(x) - x

#### Bisection ====================================================================
# for initial points
a <- -10
b <- 10

obj <- bisection(f, a, b)
print(obj)
print(f(obj$sol)) # check whether it's root

# Check by plot
plot(seq(-5,5,0.1), f(seq(-5,5,0.1)))
abline(0,0)
points(obj$sol, 0, col=2)

#### Newton Raphson =================================================================
f <- function(x) cos(x) - x
df <- function(x) -sin(x) - 1

obj2 <- newton(f, df, init = 1)
print(obj2)
print(f(obj2$sol)) # check whether it's root

# Check by plot
points(obj2$sol, 0, col=4)
