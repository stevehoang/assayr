#' CoA DRC Curve Plots
#'
#' Plot one or more dose response curves.
#'
#' @param tib A tibble or data.frame with PureHoney data including vars(tx_cmpd, curve_plot).
#' @param analytes A character vector with a subset of \code{unique(tib$targ)} for the target/analytes to be plotted.
#' @param y_var A character with the \code{tib} column name to be used for the y-axis. Default is "conc_incell_uM", "conc_corrected" may also be useful.
#' @param limits A named list with the names matching \code{unique(tib$curve_plot)} and values of numeric vectors with length of 2, describind the y-axis bound for each `curve_plot`. If there is a target present in \code{targs} that is not in \code{limits} the limits will be calculated with \code{ggplot2}'s defualt behavior.
#' @param grouping_var A character string representing the column used for grouping experiments.
#' @param drm_error Logical. Determines if drc() convergence failure results in a error (TRUE) or warning (FALSE)
#' @param ec50 Logical. Show EC50.
#' @param ec50ci Logical. Show 95\% CI of EC50.
#' @param Hill Logical. Show Hill coefficient.
#' @param up_asym Logical. Show the upper asymptote.
#' @param low_asym Logical. Show the lower asymptote.
#' @param robust Logical. Use robust curve estimation.
#'
#' @return A plot object.
#'
#' @examples
#' pah <- filter(samps2, run == "PAH0503") # tib
#' drcPhotoBooth(pah)
#' @export
drcPhotoBoothShine <- function(tib,
                               analytes = c("Acetyl-CoA",
                                            "Isobutyryl-CoA",
                                            "Propionyl-CoA"),
                               y_var = "conc_incell_uM",
                               limits = list("Acetyl" = c(0,55),
                                             "Isobutyryl" = c(0,45),
                                             "Propionyl" = c(0,65)),
                               grouping_var = "tx_run",
                               drm_error = FALSE,
                               ec50 = TRUE,
                               ec50ci = FALSE,
                               Hill = FALSE,
                               up_asym = FALSE,
                               low_asym = FALSE,
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

  if (ec50 | ec50ci) {
    ec_cis <- tibble::tibble(split = names(drs),
                     ec50_val = purrr::map_dbl(drs, ~ assayr::getEC50(., CI95 = T)["estimate"]),
                     ec50_ci_low = purrr::map_dbl(drs, ~ assayr::getEC50(., CI95 = T)["2.5 %"]),
                     ec50_ci_high = purrr::map_dbl(drs, ~ assayr::getEC50(., CI95 = T)["97.5 %"])) %>%
      tidyr::separate(split, c(grouping_var, "targ"), sep = "\\.") %>%
      dplyr::mutate(curve_plot = gsub(patt, "\\1", targ)) %>%
      dplyr::mutate(ec50_val = round(ec50_val, 2),
             ec50_ci_low = round(ec50_ci_low, 2),
             ec50_ci_high = round(ec50_ci_high, 2))
  }
  if (Hill) {
    hill_est <- tibble::tibble(split = names(drs),
                       hill = purrr::map_dbl(drs, ~ assayr::getHillSlope(.))) %>%
      tidyr::separate(split, c(grouping_var, "targ"), sep = "\\.") %>%
      dplyr::mutate(curve_plot = gsub(patt, "\\1", targ)) %>%
      dplyr::mutate(hill = round(hill, 2))
  }

  if (up_asym) {
    up_asym_est <- tibble::tibble(split = names(drs),
                          ua = purrr::map_dbl(drs, ~ assayr::getUpperAsym(.))) %>%
      tidyr::separate(split, c(grouping_var, "targ"), sep = "\\.") %>%
      dplyr::mutate(curve_plot = gsub(patt, "\\1", targ)) %>%
      dplyr::mutate(ua = round(ua, 2))
  }

  if (low_asym) {
    low_asym_est <- tibble::tibble(split = names(drs),
                           la = purrr::map_dbl(drs, ~ assayr::getLowerAsym(.))) %>%
      tidyr::separate(split, c(grouping_var, "targ"), sep = "\\.") %>%
      dplyr::mutate(curve_plot = gsub(patt, "\\1", targ)) %>%
      dplyr::mutate(la = round(la, 2))
  }

  # Plot
  cp <- dplyr::select(tib_dr, tx_run, curve_plot) %>%
    unique()
  lims <- tidyr::gather(as.data.frame(limits))
  lims %<>% set_colnames(c("curve_plot", y_var))
  lims %<>% merge(cp, by = "curve_plot")

  bounds <- as.data.frame(limits) %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("mins", "maxes")) %>%
    tibble::rownames_to_column("curve_plot")

  tib_dr$dummy <- tib_dr[[y_var]]

  tib_dr %<>% merge(bounds, by = "curve_plot") %>%
    dplyr::rowwise() %>%
    dplyr::filter(dummy < maxes) %>%
    dplyr::filter(dummy > mins)

  curves %<>% merge(bounds, by = "curve_plot") %>%
    dplyr::rowwise() %>%
    dplyr::filter(ys < maxes) %>%
    dplyr::filter(ys > mins)

  p <- ggplot2::ggplot(tib_dr, ggplot2::aes_(x = ~tx_conc, y = as.name(y_var))) +
    ggplot2::scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000),
                  labels = scales::comma) +
    ggplot2::annotation_logticks(sides = "b") +
    ggplot2::geom_line(ggplot2::aes(x = xs, y = ys), data = curves, size = 1) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::geom_blank(ggplot2::aes(x=NULL), data = lims) +
    assayr::theme_assayr() +
    ggplot2::theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust=1)) +
    ggplot2::labs(y = "intracellular concentration (uM)",
         x = "compound concentration (uM)") +
    ggplot2::facet_grid(curve_plot ~ tx_run, scales = "free_y")

  # if (length(unique(tib_dr$tx_run)) > 1) {
  #   p <- p + ggplot2::facet_grid(curve_plot ~ tx_run, scales = "free_y")
  # } else {
  #   p <- p + ggplot2::facet_grid(curve_plot~., scales = "free_y")
  # }

  if (ec50ci) {
    p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = ec50_ci_low), data = ec_cis,
                        linetype = "dotted") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = ec50_ci_high), data = ec_cis,
                 linetype = "dotted")
  }

  if (ec50ci) {
    p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = ec50_val), data = ec_cis,
                        linetype = "dashed") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = ec50_ci_low), data = ec_cis,
                 linetype = "dotted") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = ec50_ci_high), data = ec_cis,
                 linetype = "dotted") +
      ggplot2::geom_label(ggplot2::aes(x = ec50_val, y=Inf,
                     label = paste0(ec50_val, " [", ec50_ci_low, "]","^", ec50_ci_high)), data = ec_cis,
                 vjust = 1, parse = T)
  }

  if (ec50 & !ec50ci) {
    p <-  p + ggplot2::geom_vline(ggplot2::aes(xintercept = ec50_val), data = ec_cis,
                         linetype = "dashed") +
      ggplot2::geom_label(ggplot2::aes(x = ec50_val, y=Inf,
                     label = ec50_val), data = ec_cis,
                 vjust = 1, alpha = 0.5)
  }

  if (up_asym) {
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = ua), data = up_asym_est,
                        linetype = "dashed") +
      ggplot2::geom_label(ggplot2::aes(x = Inf, y=ua, label = ua), data = up_asym_est,
                 hjust = 1, alpha = 0.5)
  }

  if (low_asym) {
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = la), data = low_asym_est,
                        linetype = "dashed") +
      ggplot2::geom_label(ggplot2::aes(x = Inf, y=la, label = la), data = low_asym_est,
                 hjust = 1, alpha = 0.5)
  }

  if (Hill) {
    p <- p + ggplot2::geom_label(ggplot2::aes(x = Inf, y=Inf, label = paste0("Hill coef = ", hill)), data = hill_est,
                 hjust = 1, vjust = 1, alpha = 0.5)
  }

  return(p)
}

