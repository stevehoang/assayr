#' @title Build plot-ready confidence intervals
#' @description Wrapper for \code{confint()} that handles the output munging for models of class "lm", "lmerMod" and "lmerModLmerTest".
#' @param mod A model object of class "lm", "lmerMod", or "lmerModLmerTest".
#' @param level Numeric (0-1). The confidence level required.
#' @param all Logical. Return only the first fixed effect on RHS of the model formula (FALSE) or all fixed effects (TRUE).
#' @examples
#' mod <- lm(Sepal.Length ~ Species, data = iris)
#' ci <- makeCIs(mod)
#'
#' mods <- mtcars %>% split(.$cyl) %>% purrr::map(~ lm(mpg ~ hp, data = .))
#' cis <- purrr::map(mods, makeCIs)
#' @export
makeCIs <- function(mod, level = .95, all = FALSE) {
    if (class(mod) == "lm") {
        if (attributes(mod$terms)$intercept == 1) {
            stop("Use model without intercept for easier plotting")
        }
        if (length(attributes(mod$terms)$term.labels) > 1 & all == T) {
            warning("More than 1 predictor variable, watch for extra rows in return or set all = F")
        }
        ci <- as.data.frame(confint(mod, level = level))
        plot_pred <- attributes(mod$terms)$term.labels[1]
        ci <- dplyr::mutate(ci, est = coef(mod), temp = gsub(plot_pred, "", rownames(ci)))
        if (all == F) {
            not_wanted <- attributes(mod$terms)$term.labels[-1]
            not_wanted <- not_wanted[!grepl(plot_pred, not_wanted)]
            for (p in not_wanted) {
                ci %<>% dplyr::filter(!grepl(p, temp))
            }
        }
    }
    if (class(mod) == "merModLmerTest" | class(mod) == "lmerMod") {
        num_sigma <- length(attributes(mod)$flist) + 1
        ci <- as.data.frame(confint(mod))[-(1:num_sigma),]
        if (rownames(ci)[1] == "(Intercept)") {
            stop("Use model without intercept for easier plotting")
        }
        ci$est <- nlme::fixef(mod)
        plot_pred <- colnames(attributes(mod)$frame)[2]
        ci$temp <- gsub(plot_pred, "", rownames(ci))
        if (all == F) {
            not_wanted <- colnames(attributes(mod)$frame)[c(-1,-2)]
            not_wanted <- not_wanted[!grepl(plot_pred, not_wanted)]
            for (p in not_wanted) {
                ci %<>% dplyr::filter(!grepl(p, temp))
            }
        }
    }
    colnames(ci) <- c("lwr","upr", "est", plot_pred)
    return(ci)
}
