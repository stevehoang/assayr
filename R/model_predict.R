inversePredLm <- function(y, fit) {
  coefs <- fit$coefficients
  stopifnot((length(coefs) == 2) & names(coefs)[1] == "(Intercept)")
  x <- (y - coefs[1]) / coefs[2]
  x %<>% unname
  return(x)
}

inversePredLL4 <- function(y, fit, curve_id = NULL) {
  if (is.null(curve_id)) {
    coefs <- fit$coefficients
  }
  else {
    coefs <- fit$coefficients[curve_id == gsub("\\D:", "", names(fit$coefficients))]
  }
  b <- coefs[1] %>% unname
  c <- coefs[2] %>% unname
  d <- coefs[3] %>% unname
  e <- coefs[4] %>% unname
  to_log <- ((d-c) / (y - c)) - 1
  logx <- (1/b) * log(to_log) + log(e)
  res <- exp(logx)
  return(res)
}

predDRC <- function(x, fit) {
  val <- predict(fit, data.frame(x)) %>%
    unname()
  return(val)
}
