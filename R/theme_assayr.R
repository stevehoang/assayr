#' @title Complete theme for assay report default
#' @description This is a complete theme build off of \code{ggplot2::theme_classic()}. See \code{?theme_classic} for details.
#' @param base_size base font size
#' @param base_family	base font family
#' @examples
#' ggplot(mtcars, aes(mpg, hp, color = as.factor(cyl))) +
#' geom_point() +
#' theme_assayr()
#' @export
theme_assayr <- function(base_size = 18, base_family = "")
{
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1),
                    strip.background = ggplot2::element_rect(linetype = "blank"),
                    plot.title = ggplot2::element_text(size = ggplot2::rel(1)),
                    axis.text = ggplot2::element_text(size = ggplot2::rel(0.75)),
                    axis.text.x = ggplot2::element_text(angle = 0, vjust = .5, hjust = .5),
                    panel.grid.minor = ggplot2::element_line(colour="grey90", size=0.5),
                    panel.grid.major = ggplot2::element_line(colour="grey90", size=0.5),
                    legend.position = "top",
                    legend.box = "horizontal",
                    complete = TRUE)
}

