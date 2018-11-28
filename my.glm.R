my.glm <- function(x, y, beta0 = NULL, family, eps = 1e-5, max.iter = 100){
  if (family == "poisson"){
    obj <- my.pois.reg(x, y, beta0 = beta0, eps = eps, max.iter = max.iter)
  } else if (family == "binomial"){
    obj <- my.logit.reg(x, y, beta0 = beta0, eps = eps, max.iter = max.iter)
  } else if (family == "gaussian"){
    obj <- my.reg(x, y, beta0 = beta0, eps = eps, max.iter = max.iter)
  }  else if (family == "gamma"){
    obj <- my.gam.reg(x, y, beta0 = beta0, eps = eps, max.iter = max.iter)
  } else {print("No such familiy distribution exists in this function")}
  return(obj)
}