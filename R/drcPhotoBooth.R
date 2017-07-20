#' @title CoA DRC Curves Plot for Compound Response
#' @description Plot each selected targets for each cmpd from a PureHoney tibble separatly in a (n-curve)x1 \code{cowplot::plot_grid()} with the standard RNO theme.
#' @param tib A tibble or data.frame with PureHoney data including vars(tx_cmpd, curve_plot).
#' @param targs A character vector with a subset of \code{unique(tib$targ)} for the target/analytes to be plotted.
#' @param y_var A character with the \code{tib} column name to be used for the y-axis. Default is "conc_incell_uM", "conc_corrected" may also be useful.
#' @param grouping_var A character with the \code{tib} column name to be used for splitting the data into plots. Default is "tx_cmpd", "tx_total" may also be useful.
#' @param output_path A character with valid file path. Default is current working directory.
#' @param limits A named list with the names matching \code{unique(tib$curve_plot)} and values of numeric vectors with length of 2, describind the y-axis bound for each `curve_plot`. If there is a target present in \code{targs} that is not in \code{limits} the limits will be calculated with \code{ggplot2}'s defualt behavior.
#' @return A set of .png images, one for each in \code{unique(tib$tx_cmpd)}. Default it to make a new directory as a container for the .pngs
#' @examples
#' pah <- filter(samps2, run == "PAH0503") # tib
#' drcPhotoBooth(pah)
#' @export
drcPhotoBooth <- function(tib,
                          targs = c("13C-Isobutyryl-CoA", "13C-Propionyl-CoA"),
                          y_var = "conc_incell_uM",
                          grouping_var = "tx_cmpd",
                          output_path = "./",
                          new_folder = TRUE,
                          limits = list("Isobutyryl" = c(0,45),
                                        "Propionyl" = c(0,45)),
                          drm_error_allow = FALSE,
                          ec50 = TRUE) {
  ## Filter / Shiny reactive elemen
  tib_dr <- dplyr::filter(tib, targ %in% targs)

  # capture some meta info
  run <- unique(tib_dr$run)[1]
  ph_id <- unique(tib_dr$plate_id)[1]

  if (new_folder) {
    output_path <- paste0(output_path, run, "_PH", ph_id, "_DRCs/")
    system( paste("mkdir", output_path) ) }

  ## Input Handling
  if (!is.numeric(tib_dr$tx_conc)) {
    warning("Cohercing tx_conc to numeric...")
    tib_dr$tx_conc %<>% as.character() %>%
      as.numeric() }

  if (min(tib_dr$tx_conc) == 0) {
    tib_dr %<>% split(.[[grouping_var]]) %>%
      purrr::map_df(~ dplyr::mutate(., tx_conc = ifelse(tx_conc == 0,
                                          assayr::newZeros(tx_conc),
                                          tx_conc) ) ) }

  if (!is.factor(tib_dr$curve_plot)) {
    warning("Cohercing curve_plot to factor...")
    tib_dr$curve_plot %<>% as.factor() }

  if (!is.factor(tib_dr[[grouping_var]])) {
    warning(paste("Cohercing", grouping_var, "to factor..."))
    tib_dr[[grouping_var]] %<>% as.factor() }

  # Calculate Dose Response
  form <- paste(y_var, "~", "tx_conc") %>% as.formula

  drs <- tib_dr %>% split(list(.[[grouping_var]], .$curve_plot), drop = T) %>%
    map(~ drc::drm(form, data = ., fct = drc::LL.4(), control = drc::drmc(errorm = drm_error_allow)))

  ranges <- tib_dr %>% split(list(.[[grouping_var]], .$curve_plot), drop = T) %>%
    purrr::map(~ range(.$tx_conc))

  curves <- purrr::map2_df(drs, ranges,
                    ~ assayr::getCurves(range = .y, fit = .), .id = "split")

  curves %<>% tidyr::separate(split, c(grouping_var, "curve_plot"), sep = "\\.")

  # Conditional calcs
  if (ec50) {
    ec_coefs <- tibble(split = names(drs),
                       ec50 = purrr::map_dbl(drs, assayr::getEC50)) %>%
      separate(split, c(grouping_var, "curve_plot"), sep = "\\.") %>%
      mutate(ec50 = round(ec50, 2))
  }

  # more to come for the shiny app

  ## Plot Now
  # bc of bug in theme_void() https://github.com/tidyverse/ggplot2/issues/2058
  ggplot2::theme_set(ggplot2::theme_bw())

  for (cmpd in levels(tib_dr[[grouping_var]])) {
    plots <- list()
    tib_dr1 <- tib_dr[ tib_dr[[grouping_var]] == cmpd, ] %>% droplevels()
    num_rows <- length(unique(tib_dr1$targ))
    curves1 <- curves[ curves[[grouping_var]] == cmpd, ] %>% droplevels()

    i <- 1
    for (cp in levels(tib_dr1$curve_plot)) {
      tib_dr2 <- dplyr::filter(tib_dr1, curve_plot == cp)
      curves2 <- dplyr::filter(curves1, curve_plot == cp)

      plots[[i]] <- ggplot2::ggplot(tib_dr2, ggplot2::aes(tx_conc, conc_incell_uM)) +
        ggplot2::geom_line(data = curves2, ggplot2::aes(xs, ys)) +
        ggplot2::geom_point() +
        ggplot2::scale_x_log10(breaks = drcBreaks, labels = drcLabels) +
        ggplot2::facet_wrap(~ curve_plot, scales = "free") +
        ggplot2::labs(title = NULL,
             x = "[Cmpd] uM",
             y = "[Analyte] uM intracellular") +
        ggplot2::theme_classic(base_size = 18) +
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1),
                       strip.background = ggplot2::element_rect(linetype = "blank"),
                       plot.title = ggplot2::element_text(size = ggplot2::rel(1)),
                       axis.text = ggplot2::element_text(size = ggplot2::rel(0.75)),
                       axis.text.x = ggplot2::element_text(angle = 0, vjust = .5, hjust = .5, debug = F),
                       panel.grid.minor = ggplot2::element_line(colour="grey90", size=0.5),
                       panel.grid.major = ggplot2::element_line(colour="grey90", size=0.5),
                       legend.position = "none",
                       legend.box = "horizontal")

        if (ec50) {
          ec_coefs2 <- ec_coefs[ ec_coefs[[grouping_var]] == cmpd, ] %>%
            filter(curve_plot == cp)
          plots[[i]] <- plots[[i]] +
            ggplot2::geom_vline(data = ec_coefs2, ggplot2::aes(xintercept = ec50), linetype = 2) +
            ggplot2::geom_label(data = ec_coefs2, ggplot2::aes(label = ec50), x = -Inf, y = Inf,
                                hjust = 0, vjust = 1)

        }
      i <- i + 1
    }

    title <- cowplot::ggdraw() + cowplot::draw_label(paste(run, cmpd), fontface= "bold")

    png(filename = paste0(output_path, run, "_PH", ph_id, "_", cmpd, ".png"),
        width = 4,
        height = num_rows*3, units = "in", res = 300)

    print( cowplot::plot_grid(title,
                              cowplot::plot_grid(plotlist = plots,
                                                 ncol = 1),
                              ncol = 1,
                              rel_heights = c(.05, .9) ) )
    dev.off()

  }
}
