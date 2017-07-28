#' CoA DRC Tables
#'
#' Generate a DRC stats table.
#'
#' @param tib A tibble or data.frame with PureHoney data including vars(tx_cmpd, curve_plot).
#' @param analytes A character vector of the target/analytes to be plotted.
#' @param y_var A character with the \code{tib} column name to be used for the y-axis. Default is "conc_incell_uM", "conc_corrected" may also be useful.
#' @param grouping_var A character string representing the column used for grouping experiments.
#' @param drm_error Logical. Determines if drc() convergence failure results in a error (TRUE) or warning (FALSE)
#' @param ec50 Logical. Show EC50.
#' @param Hill Logical. Show Hill coefficient.
#' @param up_asym Logical. Show the upper asymptote.
#' @param low_asym Logical. Show the lower asymptote.
#' @param cis Logical. Show 95\% CI of stats.
#' @param robust Logical. Use robust curve estimation.
#'
#' @return A plot object.
#'
#' @examples
#' pah <- filter(samps2, run == "PAH0503") # tib
#' drcPhotoBooth(pah)
#' @export
drcTableShine <- function(tib,
                           analytes = c("Acetyl-CoA",
                                        "Isobutyryl-CoA",
                                        "Propionyl-CoA"),
                           y_var = "conc_incell_uM",
                           grouping_var = "tx_run",
                           drm_error = FALSE,
                           ec50 = TRUE,
                           Hill = FALSE,
                           up_asym = FALSE,
                           low_asym = FALSE,
                           cis = FALSE,
                           robust = TRUE) {
  ## Filter
  tib %<>% filter(heavy == "TRUE")
  tib$curve_plot %<>% gsub("\\-CoA$", "", .)
  analytes %<>% gsub("\\-CoA$", "", .)
  tib_dr <- dplyr::filter(tib, curve_plot %in% analytes)

  ## Input Handling
  if (!is.numeric(tib_dr$tx_conc)) {
    # warning("Coercing tx_conc to numeric...")
    tib_dr$tx_conc %<>% as.character() %>%
      as.numeric()
  }

  if (min(tib_dr$tx_conc) == 0) {
    tib_dr %<>% split(.[[grouping_var]]) %>%
      purrr::map_df(~ dplyr::mutate(., tx_conc = ifelse(tx_conc == 0,
                                                        assayr::newZeros(tx_conc),
                                                        tx_conc)))
  }

  if (!is.factor(tib_dr$curve_plot)) {
    # warning("Coercing curve_plot to factor...")
    tib_dr$curve_plot %<>% as.factor()
  }

  if (!is.factor(tib_dr[[grouping_var]])) {
    # warning(paste("Coercing", grouping_var, "to factor..."))
    tib_dr[[grouping_var]] %<>% as.factor()
  }

  # Calculate Dose Response
  form <- paste(y_var, "~", "tx_conc") %>% as.formula

  drs <- tib_dr %>% split(list(.[[grouping_var]], .$targ), drop = T) %>%
    purrr::map(~ drc::drm(form, data = ., fct = drc::LL.4(), control = drc::drmc(errorm = drm_error)))

  if (robust) {
    drs %<>% purrr::map(~ robustifyDrc(., form))
  }

  ranges <- tib_dr %>% split(list(.[[grouping_var]], .$curve_plot), drop = T) %>%
    purrr::map(~ range(.$tx_conc))

  curves <- purrr::map2_df(drs, ranges,
                           ~ assayr::getCurves(range = .y, fit = .), .id = "split")

  curves %<>% tidyr::separate(split, c(grouping_var, "targ"), sep = "\\.")
  patt <- tib_dr$curve_plot %>% unique
  patt <- paste(patt, collapse = "|")
  patt <- paste0(".*(", patt, ").*")
  curves %<>% dplyr::mutate(curve_plot = gsub(patt, "\\1", targ))
  # Conditional calcs

  ec_cis <- tibble::tibble(split = names(drs),
                           ec50_val = purrr::map_dbl(drs, ~ assayr::getEC50(., CI95 = T)["estimate"]),
                           ec50_ci_low = purrr::map_dbl(drs, ~ assayr::getEC50(., CI95 = T)["2.5 %"]),
                           ec50_ci_high = purrr::map_dbl(drs, ~ assayr::getEC50(., CI95 = T)["97.5 %"])) %>%
    tidyr::separate(split, c(grouping_var, "targ"), sep = "\\.") %>%
    dplyr::mutate(curve_plot = gsub(patt, "\\1", targ)) %>%
    dplyr::mutate(ec50_val = round(ec50_val, 2),
                  ec50_ci_low = round(ec50_ci_low, 2),
                  ec50_ci_high = round(ec50_ci_high, 2))

  hill_est <- tibble::tibble(split = names(drs),
                             hill = purrr::map_dbl(drs, ~ assayr::getHillSlope(., CI95 = T)["estimate"]),
                             hill_ci_low = purrr::map_dbl(drs, ~ assayr::getHillSlope(., CI95 = T)["2.5 %"]),
                             hill_ci_high = purrr::map_dbl(drs, ~ assayr::getHillSlope(., CI95 = T)["97.5 %"])) %>%
    tidyr::separate(split, c(grouping_var, "targ"), sep = "\\.") %>%
    dplyr::mutate(curve_plot = gsub(patt, "\\1", targ)) %>%
    dplyr::mutate(hill = round(hill, 2),
                  hill_ci_low = round(hill_ci_low, 2),
                  hill_ci_high = round(hill_ci_high, 2))

  up_asym_est <- tibble::tibble(split = names(drs),
                                ua = purrr::map_dbl(drs, ~ assayr::getUpperAsym(., CI95 = T)["estimate"]),
                                ua_ci_low = purrr::map_dbl(drs, ~ assayr::getUpperAsym(., CI95 = T)["2.5 %"]),
                                ua_ci_high = purrr::map_dbl(drs, ~ assayr::getUpperAsym(., CI95 = T)["97.5 %"])) %>%
    tidyr::separate(split, c(grouping_var, "targ"), sep = "\\.") %>%
    dplyr::mutate(curve_plot = gsub(patt, "\\1", targ)) %>%
    dplyr::mutate(ua = round(ua, 2),
                  ua_ci_low = round(ua_ci_low, 2),
                  ua_ci_high = round(ua_ci_high, 2))

  low_asym_est <- tibble::tibble(split = names(drs),
                                 la = purrr::map_dbl(drs, ~ assayr::getLowerAsym(., CI95 = T)["estimate"]),
                                 la_ci_low = purrr::map_dbl(drs, ~ assayr::getLowerAsym(., CI95 = T)["2.5 %"]),
                                 la_ci_high = purrr::map_dbl(drs, ~ assayr::getLowerAsym(., CI95 = T)["97.5 %"])) %>%
    tidyr::separate(split, c(grouping_var, "targ"), sep = "\\.") %>%
    dplyr::mutate(curve_plot = gsub(patt, "\\1", targ)) %>%
    dplyr::mutate(la = round(la, 2),
                  la_ci_low = round(la_ci_low, 2),
                  la_ci_high = round(la_ci_high, 2))

  tab <- Reduce(merge, list(ec_cis, hill_est,
                            up_asym_est, low_asym_est))

  tab %<>% dplyr::select(-targ)
  tab %<>% rowwise() %>%
    mutate(cmpd = strsplit(tx_run, split = "_")[[1]][1],
           run = strsplit(tx_run, split = "_")[[1]][2]) %>%
    dplyr::select(-tx_run) %>%
    rename(analyte = curve_plot) %>%
    dplyr::select(cmpd, run, analyte, ec50_val,
                  ec50_ci_high, ec50_ci_low,
                  hill, hill_ci_high, hill_ci_low,
                  ua, ua_ci_high, ua_ci_low, la,
                  la_ci_high, la_ci_low)

  colnames(tab) %<>% gsub("ec50", "EC50", .)
  colnames(tab) %<>% gsub("_val", "", .)
  colnames(tab) %<>% gsub("hill", "Hill", .)
  colnames(tab) %<>% gsub("ua", "upper asym", .)
  colnames(tab) %<>% gsub("la", "lower asym", .)
  colnames(tab) %<>% gsub("_ci_high", " 97.5%", .)
  colnames(tab) %<>% gsub("_ci_low", " 2.5%", .)

  if (!ec50) {
    tab %<>% dplyr::select(-dplyr::matches("EC50"))
  }
  if (!hill) {
    tab %<>% dplyr::select(-dplyr::matches("Hill"))
  }
  if (!up_asym) {
    tab %<>% dplyr::select(-dplyr::matches("upper"))
  }
  if (!low_asym) {
    tab %<>% dplyr::select(-dplyr::matches("lower"))
  }
  if (!cis) {
    tab %<>% dplyr::select(-dplyr::matches("%"))
  }
  return(tab)
}

