#' @title QC Plot for Shiny
#' @description Plot a variety of vizualization depending on user input.
#' @param tib A tibble or data.frame with PureHoney data, from reactive obeject \code{d()}
#' @param plot_type A character string for determing plot to produce, from \code{input$qc_opts}
#' @return A cowplot with analytes as rows and treatments as columns
#' @examples
#' # not ready
#' @export
qcShine <- function(tib,
                    plot_type = "recov_plate") {
  
  if (plot_type == "recov_plate") {
    internal$row %<>% factor(levels = rev(LETTERS))
    internal$column %<>% factor(levels = unique(gtools::mixedsort(internal$column)))
    
    for (p in gtools::mixedsort(unique(internal$plate_run))) {
      df <- filter(internal, plate_run == p)
      
      p1 <- ggplot(df, aes(x = column, y = row)) +
        geom_tile(aes(fill = conc)) +
        geom_label(aes(label = paste0(contents, "\n", round(recov * 100, digits = 1), "%"), color = tx_cmpd),
                   fontface = "bold", size = 3, fill = "white") +
        scale_fill_viridis(direction = 1,name = "[SIL] nM ",
                           breaks = as.numeric(round(quantile(range(df$conc), c(.1, .5, .9)), 0))) +
        scale_color_d3(name = "Tx:") +
        theme(legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal") +
        guides(fill = guide_colorbar(order = 1), color = guide_legend(order = 2)) +
        labs(title= paste(p_title ,p, unique(internal$targ), "IS recovery"),
             x = NULL, y = NULL)
      
      p2 <- ggplot(df, aes(recov, color = tx_cmpd)) +
        geom_density() +
        scale_color_d3() +
        scale_x_continuous(limits = c(0,1)) +
        guides(color = "none") +
        labs(subtitle = paste0(p," recovery distribution"),
             y = "density",
             x="recovery fraction")
      
      return(
        cowplot::plot_grid(plotlist = list(p1, p2), nrow = 2, rel_heights = c(1,.7)) +
        theme_void() +
        theme(axis.text.x = NULL)
      )
    }
  }

  }