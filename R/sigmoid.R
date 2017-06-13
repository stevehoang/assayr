getUpperAsym <- function(fit, curve_id = NULL) {
  if (is.null(curve_id)) {
    coefs <- fit$coefficients
  }
  else {
    coefs <- fit$coefficients[curve_id == gsub("\\D:", "", names(fit$coefficients))]
  }
  return(unname(coefs[3]))
}

getLowerAsym <- function(fit, curve_id = NULL) {
  if (is.null(curve_id)) {
    coefs <- fit$coefficients
  }
  else {
    coefs <- fit$coefficients[curve_id == gsub("\\D:", "", names(fit$coefficients))]
  }
  return(unname(coefs[2]))
}

getEC50 <- function(fit, curve_id = NULL) {
  if (is.null(curve_id)) {
    coefs <- fit$coefficients
  }
  else {
    coefs <- fit$coefficients[curve_id == gsub("\\D:", "", names(fit$coefficients))]
  }
  return(unname(coefs[4]))
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
  return(unname(hill))
}

getCurves <- function(range, fit, logrange = T, npoint=100) {
  if (logrange) {
    range <- log(range)
  }
  step <- (range[2] - range[1]) / (npoint - 1)
  xs <- seq(range[1], range[2], step)
  if (logrange) {
    xs <- exp(xs)
  }
  ys <- sapply(xs, predDRC, fit)
  res <- data.frame(xs, ys)
  return(res)
}

getYBounds <- function(fit, high_x, low_x) {
  y1 <- predDRC(fit, high_x)
  y2 <- predDRC(fit, low_x)
  ys <- sort(c(y1, y2))
  res <- list(boc = ys[1], toc = ys[2])
  return(res)
}
