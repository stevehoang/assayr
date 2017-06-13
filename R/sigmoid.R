
getUpperAsym <- function(fit, curve_id = NULL) {
  if (is.null(curve_id)) {
    coefs <- fit$coefficients
  }
  else {
    coefs <- fit$coefficients[curve_id == gsub("\\D:", "", names(fit$coefficients))]
  }
  return(coefs[3])
}

getLowerAsym <- function(fit, curve_id = NULL) {
  if (is.null(curve_id)) {
    coefs <- fit$coefficients
  }
  else {
    coefs <- fit$coefficients[curve_id == gsub("\\D:", "", names(fit$coefficients))]
  }
  return(coefs[2])
}

getEC50 <- function(fit, curve_id = NULL) {
  if (is.null(curve_id)) {
    coefs <- fit$coefficients
  }
  else {
    coefs <- fit$coefficients[curve_id == gsub("\\D:", "", names(fit$coefficients))]
  }
  return(coefs[4])
}

getHillSlope <- function(fit, curve_id = NULL) {
  if (is.null(curve_id)) {
    coefs <- fit$coefficients
  }
  else {
    coefs <- fit$coefficients[curve_id == gsub("\\D:", "", names(fit$coefficients))]
  }
  b <- coefs[1]
  hill <- (-1 * b) / log(10)
  return(hill)
}

getCurves <- function(range, fit, npoint=100) { # use logt as x in drm; use logt in range for getCurves
  step <- (range[2] - range[1]) / (npoint - 1)
  xs <- seq(range[1], range[2], step)
  ys <- sapply(xs, predDRC, fit)
  res <- data.frame(xs, ys)
  return(res)
}


