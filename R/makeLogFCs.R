# conveince wrapper for calculating fold changes based on multcomp::glht()
# works on glht objects (ht) with flexible log base

makeLogFCs <- function(ht, base = 2) {
    if (class(ht) == "glht") {
        if (attributes(ht$model)$class == "lm") {
            if (sum(!grepl("log", (ht$model$call$formula[[2]]))) < 1) {
                warning("No log characters detected in ht$model$call$formula;
                        Make sure response values are in log2 space for FC calculations")
            }
            if (attributes(ht$model$terms)$intercept == 1) {
                stop("Redo model with intercept = 0 for easier plotting")
            }
            plot_pred <- (attributes(ht$model$terms)$term.labels)[1]
        }
        if (attributes(ht$model)$class == "merModLmerTest" | attributes(ht$model)$class == "lmerMod") {
            if (sum(!grepl("(log)|(ln)", attributes(ht$model)$call$formula[[2]])) < 1) {
                warning("No log characters detected in ht$model$call$formula;
                        Make sure response values are in log2 space for FC calculations")
            }
            if (names(ht$coef)[1] == "(Intercept)") {
                stop("Redo model with intercept = 0 for easier plotting")
            }
            plot_pred <- ht$focus
        }
        ht_df <- confint(ht)$confint %>%
            as.data.frame() %>%
            dplyr::rename(est = Estimate) %>%
            dplyr::mutate(contrast = gsub(plot_pred, "", rownames(.)),
                   pct_label = paste0(as.character(round((1-base^est)*-100)),"%"),
                   pct_fill = round((1-base^est)*-100),
                   pval = summary(ht)$test$pvalues %>% round(., 3))
    }
    return(ht_df)
}
