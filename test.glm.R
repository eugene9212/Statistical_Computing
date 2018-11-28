#### GLM Code   ####
## Test my.glm with glm() ##

rm(list = ls())

#### load packages & R code ####
setwd('C:/Users/eugene/Desktop/Statistical_Computing/Statistical_Computing')
source('my.logit.reg.R')
source('my.pois.reg.R')
source('my.glm.R')

#### Input Data
set.seed(1)

n <- 100 # sample size
p <- 3   # predictor dimension

x <- matrix(rnorm(n*p), n, p) # generate predictor
x <- cbind(rep(1, n), x)      # design matrix

beta <- rep(1, p+1) # true beta

eta <- x %*% beta   # true eta (linear term)

pi <- exp(eta)/(1 + exp(eta)) # pi = mu = E(y|x)
y <- rbinom(n, 1, pi)         # generate reponse

#  my function based on NR (IWLS)
obj1 <- my.glm(x, y, family="binomial")
hat.beta1 <- obj1$est

# check with R-built-in function, glm  
obj2 <- glm(y ~ x - 1, family = "binomial")
hat.beta2 <- coefficients(obj2)

# compare
print(head(cbind(hat.beta1, hat.beta2)))

