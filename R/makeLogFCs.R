#' @title Calculate log fold changes and associated p-values.
#' @description Wrapper for calculating fold changes based on \code{multcomp::glht()} object.
#' @param ht An object of class "glht" from \code{multcomp::glht}.
#' @param base A numeric value specifying the log based used in the model. Default is 10.
#' @param round_to A numeric value specifying the number of decimal places in percent change columns in result.
#' @param conf_level A numeric value between 0 and 1 for confidence level. Default is .95.
#' @return A data frame with est (estimate change), lwr, upr (confidence bounds), contrast (predictor levels being compared), pct_label (text, for plotting labels), pct_fill (numeric, for plotting scales), and a p-value adjusted for multiple comparrisons but \code{multcomp::glht}.
#' @examples
#' mod <- lm(Sepal.Length ~ 0 + Species, data = iris) # use model with intercept set to 0
#' ht <- multcomp::glht(mod, linfct = mcp(Species = "Dunnet"))
#' fcs <- makeLogFCs(ht)
#' @export
makeLogFCs <- function(ht, base = 10, round_to = 2, conf_level = .95) {
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
                        Make sure response values are in log space for FC calculations")
            }
            if (names(ht$coef)[1] == "(Intercept)") {
                stop("Redo model with intercept = 0 for easier plotting")
            }
            plot_pred <- ht$focus
        }
        ht_df <- confint(ht, conf_level)$confint %>%
            as.data.frame() %>%
            dplyr::rename(est = Estimate) %>%
            dplyr::mutate(contrast = gsub(plot_pred, "", rownames(.)),
                   pct_label = paste0(as.character(round((1-base^est)*-100, round_to)),"%"),
                   pct_fill = round((1-base^est)*-100, round_to),
                   pval = summary(ht)$test$pvalues %>% signif(., 2))
    }

    return(ht_df)
}
