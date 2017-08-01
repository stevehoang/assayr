#' DRC Curves in a standardized format
#' 
#' Plot each selected targets for each cmpd from a PureHoney tibble separatly in a (n-curve)x1 \code{cowplot::plot_grid()} with \code{theme_assayr()}.
#' This function is wrapper for the plot outputting and expects values to be already caluclated.
#' 
#' @param dr_tib A tibble with the target to be plotted
#' @param coef_tib A tibble with the \code{drm()} fit coefficents.
#' @param curves_tib A tibble with the estimate curve data from \code{getCurves()}
#' @param y_var A character with the \code{tib} column name to be used for the y-axis. Default is "conc_incell_uM", "conc_corrected" may also be useful.
#' @param grouping_var A character with the \code{tib} column name to be used for splitting the data into plots. Default is "tx_cmpd", "tx_total" may also be useful.
#' @param output_path A character with valid file path. Default is current working directory.
#' @param new_folder A boolean whether to create a new folder automatically.
#' @param limits A named list with the names matching \code{unique(tib$curve_plot)} and values of numeric vectors with length of 2, describind the y-axis bound for each `curve_plot`. If there is a target present in \code{targs} that is not in \code{limits} the limits will be calculated with \code{ggplot2}'s defualt behavior. Not used currently.
#' @return A directory of .png images, one for each in \code{unique(tib$tx_cmpd)}.
#' @examples
#' pah <- filter(samps2, run == "PAH0503") # tib
#' drcPhotoBooth(pah)
#' @export
drcPhotoBooth <- function(dr_tib,
                          coef_tib,
                          curves_tib,
                          y_var = "conc_incell_uM",
                          grouping_var = "tx_cmpd",
                          output_path = "./",
                          new_folder = TRUE,
                          limits = list("Isobutyryl" = c(0,45),
                                        "Propionyl" = c(0,45))) {
  while (dev.cur() != 1) {
    dev.off()
  }
    
  # capture some meta info
  run <- unique(dr_tib$run)[1]
  ph_id <- unique(dr_tib$plate_id)[1]

  if (new_folder) {
    output_path <- paste0(output_path, run, "_PH", ph_id, "_DRCs/")
    system( paste("mkdir", output_path) ) }

  ## Input Handling
  if (!is.numeric(dr_tib$tx_conc)) {
    warning("Cohercing tx_conc to numeric...")
    dr_tib$tx_conc %<>% as.character() %>%
      as.numeric() }

  if (min(dr_tib$tx_conc) == 0) {
    dr_tib %<>% split(.[[grouping_var]]) %>%
      purrr::map_df(~ dplyr::mutate(., tx_conc = ifelse(tx_conc == 0,
                                          assayr::newZeros(tx_conc),
                                          tx_conc) ) ) }

  if (!is.factor(dr_tib$curve_plot)) {
    warning("Cohercing curve_plot to factor...")
    dr_tib$curve_plot %<>% as.factor() }
  
  if (!is.factor(dr_tib$targ)) {
      warning("Cohercing targ to factor...")
      dr_tib$targ %<>% as.factor() }

  if (!is.factor(dr_tib[[grouping_var]])) {
    warning(paste("Cohercing", grouping_var, "to factor..."))
    dr_tib[[grouping_var]] %<>% as.factor() }

  # Calculations are handled in R script, this is plotting wrapper

  ## Plots
  
  # bc of bug in theme_void() https://github.com/tidyverse/ggplot2/issues/2058
  ggplot2::theme_set(ggplot2::theme_bw())

  for ( cmpd in levels(dr_tib[[grouping_var]]) ) {
    plots <- list()
    dr_tib1 <- dr_tib[ dr_tib[[grouping_var]] == cmpd, ] %>% droplevels()
    coef_tib1 <- coef_tib[ coef_tib[[grouping_var]] == cmpd, ] %>% droplevels()
    curves_tib1 <- curves_tib[ curves_tib[[grouping_var]] == cmpd, ] %>% droplevels()
    num_rows <- length(unique(dr_tib1$targ))

    i <- 1
    for ( t in levels(dr_tib1$targ) ) {
      dr_tib2 <- dplyr::filter(dr_tib1, targ == t)
      curves_tib2 <- dplyr::filter(curves_tib1, targ == t)
      coef_tib2 <- dplyr::filter(coef_tib1, targ == t)

      plots[[i]] <- ggplot2::ggplot(dr_tib2, ggplot2::aes(tx_conc, conc_incell_uM)) +
        ggplot2::geom_line(data = curves_tib2, ggplot2::aes(xs, ys)) +
        ggplot2::geom_point() +
          geom_vline(data = coef_tib2, aes(xintercept = est), linetype = 2) +
          geom_vline(data = coef_tib2, aes(xintercept = lwr), linetype = 3) +
          geom_vline(data = coef_tib2, aes(xintercept = upr), linetype = 3) +
          geom_label(data = coef_tib2,
                     aes(label = paste0("Coef [lwr]^upr:", est, " [", lwr, "]","^", upr), x = est),
                     y = Inf, vjust = 1, parse = T, alpha = .5) +
        ggplot2::scale_x_log10(breaks = drcBreaks, limits = c(NA, max(dr_tib2$tx_conc))) +
        ggplot2::facet_wrap(~ targ, scales = "free") +
        ggplot2::labs(title = NULL,
                      y = paste0("[", t, "] ", ifelse(y_var == "conc_incell_uM", "uM", "nM")),
                      x = NULL) +
        theme_assayr() +
        theme(axis.text = element_text(size = rel(.5)),
              axis.text.x = element_text(angle = 45, vjust = .75, hjust = .75),
              axis.title = element_text(size = rel(.75)))

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
