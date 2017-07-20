#' @title CoA Bar Plot for Compound Dose Response
#' @description Plot each cmpd from a PureHoney tibble separatly in a (n-curve)x1 \code{cowplot::plot_grid()} with the standard RNO theme.
#' @param tib A tibble or data.frame with PureHoney data including vars(tx_cmpd, curve_plot).
#' @param y_var A character with the column name to be used for the y-axis.
#' @param output_path A character with valid file path. Default is current working directory.
#' @param limits A named list with the names matching \code{unique(tib$curve_plot)} and values of numeric vectors with length of 2, describind the y-axis bound for each `curve_plot`.
#' @return A set of .png images, one for each in \code{unique(tib$tx_cmpd)}. Default it to make a new directory as a container for the .pngs
#' @examples
#' pah <- filter(samps2, run == "PAH0503") # tib
#' coaPhotoBooth(pah)
#' @export
coaPhotoBooth <- function(tib,
                          y_var = "conc_incell_uM",
                          grouping_var = "tx_cmpd",
                          output_path = "./",
                          new_folder = TRUE,
                          limits = list("Acetyl" = c(0, 55),
                                        "Isobutyryl" = c(0,45),
                                        "Propionyl" = c(0,65))) {
    
    # bc of bug in theme_void() https://github.com/tidyverse/ggplot2/issues/2058
    ggplot2::theme_set(ggplot2::theme_bw())
    
    cust_fill <- c("blue", "grey") %>% set_names(c("TRUE", "FALSE"))
    cust_color <- c("navyblue", "grey20") %>% set_names(c("TRUE", "FALSE"))

    run <- unique(tib$run)
    ph_id <- unique(tib$plate_id)[1]

    if (new_folder) {
        output_path <- paste0(output_path, run, "_PH", ph_id, "_Cmpds/")
        system( paste("mkdir", output_path) )
    }

    if (!is.factor(tib$curve_plot)) {
        warning("Cohercing curve_plot %>% as.factor()")
        tib$curve_plot %<>% as.factor()
    }

    if (!is.factor(tib[[grouping_var]])) {
        warning(paste("Cohercing", grouping_var, "%>% as.factor()"))
        tib[[grouping_var]] %<>% as.factor()
    }

    for (cmpd in levels(tib[[grouping_var]])) {
        plots <- list()
        tib1 <- tib[ tib[[grouping_var]] == cmpd, ] %>%
            droplevels()
        num_rows <- length(unique(tib1$curve_plot))

        i <- 1
        for (cp in levels(tib1$curve_plot)) {
            tib2 <- dplyr::filter(tib1, curve_plot == cp)

            plots[[i]] <- ggplot2::ggplot(tib2, ggplot2::aes_(~tx_conc, as.name(y_var), color = ~heavy,
                                           fill = ~heavy, shape = ~c_bool)) +
                ggplot2::geom_point(size = 2, alpha = .5, position = ggplot2::position_dodge(width = .75)) +
                ggplot2::stat_summary(geom = "bar", fun.y = mean, position = "dodge", width = .75, alpha = .5) +
                ggplot2::stat_summary(fun.data = "mean_se", geom = "errorbar", width = .15,
                             position = ggplot2::position_dodge(width = .75), show.legend = F) +
                ggplot2::scale_fill_manual(values = cust_fill) +
                ggplot2::scale_color_manual(values = cust_color) +
                ggplot2::scale_shape_manual(values = c(21, 23), name = NULL, labels = c("Inside Curve", "Outside Curve")) +
                ggplot2::scale_y_continuous(limits = limits[[cp]],
                                            breaks = function(limits){ seq(0, limits[2], 10) }) +
                ggplot2::labs(title = NULL,
                     y = paste0("[", cp, "] uM"),
                     x = NULL) +
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


