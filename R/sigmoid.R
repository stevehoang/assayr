#' Make a robust DRC estimate based on Nguyen et al. 2014
#'
#' @param fit An object of class drc fit with \code{drm()} and the argument \code{fct = LL.4()}.
#' @param formula The formula of the fit.
#' @param conv Numeric value representing the convergence criterion (manhattan distance between weight vectors)
#' @param maxits Numeric value representing maximum number of iterations
#' @param verbose Logical. If TRUE reports the manhattan distance for each iteration.
#' @examples
#' fit <- drc::drm(disp~wt, data=mtcars, fct=drc::LL.4())
#' robustifyDrc(fit, formula(fit))
#' @export
robustifyDrc <- function(fit, formula, conv=0.01, maxits=100, verbose=FALSE) {
    d <- fit$data
    ow <- fit$weights
    d$weights <- .biweightMean(resid(fit))
    mdist <- sum(abs(ow - d$weights))
    if (verbose) {
      print(paste0("Manhattan distance = ", mdist))
    }
    if (mdist > conv & maxits > 0) {
        maxits <- maxits - 1
        fit <- drc::drm(formula, weights=weights, data=d, fct=drc::LL.4())
        robustifyDrc(fit, formula, conv=conv, maxits=maxits, verbose = verbose)
    }
    else { return(fit) }
}

robusty <- function(fit, conv=0.01, maxits=100, verbose = TRUE) {
  mdist <- 1
  of <- as.formula(fit, globalenv())

  while ( maxits > 0 ) {

    if (verbose) { cat("Interaction # ", abs(maxits - 101), "\nDistance", mdist, "\n-") }

    ow <- fit$weights
    d <- fit$data
    d$weights <- .biweightMean(resid(fit))

    fit <- drc::drm(of, weights = weights, data = d, fct=drc::LL.4())
    mdist <- sum(abs(ow - d$weights))

    if (mdist <= conv) {
      cat("Distance ", mdist)
      break }

    maxits <- maxits - 1
  }
  return(fit)
}

.biweightMean <- function(r) {
    mr <- mean(abs(r))
    lim <- 6 * mr
    w <- sapply(r, function(x) {ifelse(abs(x) < lim, (1 - (x/lim)^2)^2, 0)})
    return(w)
  }

#' Get the y value of the upper asymptote in a 4-parmeter log-logistic function
#'
#' @param fit An object of class drc fit with \code{drm()} and the argument \code{fct = LL.4()}.
#' @param CI95 Logical. Report 95% confidence interval.
#' @return The y intercept of the upper asymptote.
#' @examples
#' fit <- drc::drm(disp~wt, data=mtcars, fct=drc::LL.4())
#' getUpperAsym(fit)
#' @export
getUpperAsym <- function(fit, CI95 = FALSE) {
  res <- cbind(confint(fit), fit$coefficients)
  colnames(res)[3] <- "estimate"
  res <- res["d:(Intercept)", ]
  if (!CI95) {
    res <- res["estimate"]
  }
  return(res)
}

#' Get the y value of the lower asymptote in a 4-parmeter log-logistic function
#'
#' @param fit An object of class drc fit with \code{drm()} and the argument \code{fct = LL.4()}.
#' @param CI95 Logical. Report 95% confidence interval.
#' @return The y intercept of the lower asymptote.
#' @examples
#' fit <- drc::drm(disp~wt, data=mtcars, fct=drc::LL.4())
#' getLowerAsym(fit)
#' @export
getLowerAsym <- function(fit, CI95 = FALSE) {
  res <- cbind(confint(fit), fit$coefficients)
  colnames(res)[3] <- "estimate"
  res <- res["c:(Intercept)", ]
  if (!CI95) {
    res <- res["estimate"]
  }
  return(res)
}

#' Get the EC50 value of a 4-parmeter log-logistic function
#'
#' @param fit An object of class drc fit with \code{drm()} and the argument \code{fct = LL.4()}.
#' @param CI95 Logical. Report 95% confidence interval.
#' @return The x value corresponding to point at which the function is rotationally symmetric.
#' @examples
#' fit <- drc::drm(disp~wt, data=mtcars, fct=drc::LL.4())
#' getEC50(fit)
#' @export
getEC50 <- function(fit, CI95 = FALSE) {
  res <- cbind(confint(fit), fit$coefficients)
  colnames(res)[3] <- "estimate"
  res <- res["e:(Intercept)", ]
  if (!CI95) {
    res <- res["estimate"]
  }
  return(res)
}

#' Get the Hill coefficient of a 4-parmeter log-logistic function
#'
#' @param fit An object of class drc fit with \code{drm()} and the argument \code{fct = LL.4()}.
#' @param CI95 Logical. Report 95% confidence interval.
#' @return The Hill coefficient:
#' \eqn{y=c+\frac{d-c}{1+10^{(ln(\tilde{e})-x) \times Hill}}}.
#' Where c = upper asymptote, d = lower asymptote, and \eqn{\tilde{e}} = the EC50 value.
#' @examples
#' fit <- drc::drm(disp~wt, data=mtcars, fct=drc::LL.4())
#' getEC50(fit)
#' @export
getHillSlope <- function(fit, CI95=FALSE) {
  res <- cbind(confint(fit), fit$coefficients)
  colnames(res)[3] <- "estimate"
  res <- res["b:(Intercept)", ]
  res <- (-1 * res) / log(10)
  if (!CI95) {
    res <- res["estimate"]
  }
  return(res)
}

#' Generate y values in increments of a given x range.
#'
#' @param range A 2-element vector giving the x range.
#' @param fit An object of class drc fit with \code{drm()}.
#' @param logrange Logical. If TRUE increments are in log space.
#' @param npoint Number of increments in the x range.
#' @return A data frame of x and y values.
#' @examples
#' fit <- drc::drm(disp~wt, data=mtcars, fct=drc::LL.4())
#' curve <- getCurves(c(2,5), fit, logrange = F)
#' plot(ys~xs, data=curve)
#' @export
getCurves <- function(range, fit, logrange = TRUE, npoint=100) {
  range %<>% sort
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

#' Get the bottom and top of a log-logistic curve in a given x range.
#'
#' @param fit An object of class drc fit with \code{drm()}.
#' @param high_x The high end of the x range.
#' @param low_x The low end of the x range.
#' @return A list reporting the top and bottom of the curve (toc, boc)
#' @examples
#' fit <- drc::drm(disp~wt, data=mtcars, fct=drc::LL.4())
#' getYBounds(fit, 2, 5)
#' @export
getYBounds <- function(fit, high_x, low_x) {
  y1 <- predDRC(fit, high_x)
  y2 <- predDRC(fit, low_x)
  ys <- sort(c(y1, y2))
  res <- list(boc = ys[1], toc = ys[2])
  return(res)
}
