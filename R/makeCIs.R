# conveince wrapper for quickly building plot ready confidence intervals
# works with lm() or lmer() models

makeCIs <- function(mod, all = F) {
    if (class(mod) == "lm") {
        if (attributes(mod$terms)$intercept == 1) {
            stop("Consider model with intercept = 0 for easier plotting")
        }
        if (length(attributes(mod$terms)$term.labels) > 1 & all == T) {
            warning("More than 1 predicting variable, watch for extra rows in return or set all = F")
        }
        ci <- as.data.frame(confint(mod))
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
            stop("Consider model with intercept = 0 for easier plotting")
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
