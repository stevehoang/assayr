#' Predict x from y in a linear model
#'
#' @param y The y value.
#' @param fit An object of class lm with an intercept and a single coefficient.
#' @return The x value corresponding to the given y.
#' @examples
#' fit <- lm(mpg~hp, data=mtcars)
#' inversePredLm(15, fit)
#' @export
inversePredLm <- function(y, fit) {
  coefs <- fit$coefficients
  stopifnot((length(coefs) == 2) & names(coefs)[1] == "(Intercept)")
  x <- (y - coefs[1]) / coefs[2]
  x %<>% unname
  return(x)
}

#' Predict x from y in a 4-parameter log-logistic model
#'
#' @param y The y value.
#' @param fit An object of class drc fit with \code{drm()} and the argument \code{fct = LL.4()}.
#' @return The x value corresponding to the given y.
#' @examples
#' fit <- drc::drm(mpg~hp, data=mtcars, fct=LL.4())
#' inversePredLL4(15, fit)
#' @export
inversePredLL4 <- function(y, fit) {
  coefs <- fit$coefficients
  b <- coefs[1] %>% unname
  c <- coefs[2] %>% unname
  d <- coefs[3] %>% unname
  e <- coefs[4] %>% unname
  to_log <- ((d-c) / (y - c)) - 1
  logx <- (1/b) * log(to_log) + log(e)
  res <- exp(logx)
  return(res)
}

#' Predict y from x in a log-logistic model
#'
#' @param x The x value.
#' @param fit An object of class drc fit with \code{drm()}.
#' @return The y value corresponding to the given x.
#' @examples
#' fit <- drc::drm(mpg~hp, data=mtcars, fct=LL.4())
#' predDRC(150, fit)
predDRC <- function(x, fit) {
  val <- predict(fit, data.frame(x)) %>%
    unname()
  return(val)
}
