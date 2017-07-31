#' @title CoA Bar Plot for Shiny
#' @description Plot each cmpd from a PureHoney tibble for the hemoshine server.
#' @param tib A tibble or data.frame with PureHoney data including vars(tx_cmpd, curve_plot).
#' @param grouping_var A character string specifying the column in \code{tib} that specifies the experimental grouping.
#' @param y_var A character with the column name to be used for the y-axis.
#' @param analytes A character vector providing the analytes to be plotted.
#' @param species A character describing which labeled analytes to plot. Accepts "both" (default), "C13", or "C12".
#' @param limits A named list with the names matching \code{unique(tib$curve_plot)} and values of numeric vectors with length of 2, describind the y-axis bound for each `curve_plot`.
#' @param x_max Numeric. The highest dose considered in the calculation.
#' @return A cowplot with analytes as rows and treatments as columns
#' @examples
#' pah <- filter(samps2, run == "PAH0503") # tib
#' coaPhotoBooth(pah)
#' @export
coaPhotoBoothShine <- function(tib,
                               y_var = "conc_incell_uM",
                               grouping_var = "tx_run",
                               analytes = c("Acetyl-CoA",
                                            "Isobutyryl-CoA",
                                            "Propionyl-CoA"),
                               species = "both",
                               limits = list("Acetyl" = c(0, 55),
                                             "Isobutyryl" = c(0,45),
                                             "Propionyl" = c(0,65)),
                               x_max = Inf) {


  # bc of bug in theme_void() https://github.com/tidyverse/ggplot2/issues/2058
  # ggplot2::theme_set(ggplot2::theme_bw())
  analytes %<>% gsub("-CoA$", "", .)
  tib$curve_plot %<>% gsub("-CoA$", "", .)
  tib %<>% dplyr::filter(!c_bool)
  tib <- dplyr::filter(tib, tolower(curve_plot) %in% tolower(analytes))

  if(!is.finite(x_max)) {
    x_max <- Inf
  }

  if (!is.factor(tib$curve_plot)) {
    tib$curve_plot %<>% as.factor()
  }

  if (!is.factor(tib[[grouping_var]])) {
    tib[[grouping_var]] %<>% as.factor()
  }

  if (species == "C13") {
    tib %<>% dplyr::filter(heavy == T)
  }
  if (species == "C12") {
    tib %<>% dplyr::filter(heavy == F)
  }

  # Plot
  cp <- dplyr::select(tib, tx_run, curve_plot) %>%
    unique()
  lims <- tidyr::gather(as.data.frame(limits))
  lims %<>% set_colnames(c("curve_plot", y_var))
  lims %<>% merge(cp, by = "curve_plot")

  bounds <- as.data.frame(limits) %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("mins", "maxes")) %>%
    tibble::rownames_to_column("curve_plot")

  tib$dummy <- tib[[y_var]]

  tib %<>% merge(bounds, by = "curve_plot") %>%
    dplyr::rowwise() %>%
    dplyr::filter(dummy < maxes) %>%
    dplyr::filter(dummy > mins)

  # tib$heavy %<>% as.character() %>% as.logical() %>% `!`() %>% factor(levels = c("TRUE", "FALSE"))
  tib %<>% rowwise() %>%
    mutate(iso_label = ifelse(heavy == "TRUE", "C13", "C12"))
  cust_fill <- c("blue", "grey") %>% set_names(c("C13", "C12"))
  cust_color <- c("navyblue", "grey20") %>% set_names(c("C13", "C12"))

  tib$tx_conc %<>% as.character() %>%
    as.numeric()

  tib %<>% dplyr::filter(tx_conc <= x_max)

  levs <- tib$tx_conc %>%
    sort() %>%
    unique()

  tib$tx_conc %<>%
    as.character() %>%
    as.numeric() %>%
    factor(levels = levs)

  p <- ggplot2::ggplot(tib, ggplot2::aes_(x = ~tx_conc, y=as.name(y_var))) +
    # ggplot2::scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000),
                           # labels = scales::comma) +
    ggplot2::geom_point(size = 2, alpha = .5, position = ggplot2::position_dodge(width = .75),
                        aes(color = iso_label, fill = iso_label, group = iso_label), shape = 21) +
    ggplot2::stat_summary(geom = "bar", fun.y = mean, position = "dodge", width = .75, alpha = .5,
                          aes(color = iso_label, fill = iso_label, group = iso_label)) +
    ggplot2::stat_summary(fun.data = "mean_se", geom = "errorbar", width = .15,
                          position = ggplot2::position_dodge(width = .75), show.legend = F,
                          aes(group = iso_label)) +
    ggplot2::scale_fill_manual(values = cust_fill) +
    ggplot2::scale_color_manual(values = cust_color) +
    # ggplot2::scale_shape_manual(values = 21, name = NULL, labels = c("Inside Curve", "Outside Curve")) +
    ggplot2::geom_blank(ggplot2::aes(x=NULL), data = lims) +
    assayr::theme_assayr() +
    ggplot2::theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust=1)) +
    ggplot2::facet_grid(curve_plot ~ tx_run, scales = "free_y") +
    ggplot2::labs(y = "intracellular concentration (uM)",
                  x = "compound concentration (uM)",
                  color = NULL,
                  fill = NULL)
  return(p)
}
