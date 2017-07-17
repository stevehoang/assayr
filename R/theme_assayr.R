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
  theme_classic(base_size = base_size, base_family = base_family) +
              theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
                    strip.background = element_rect(linetype = "blank"),
                    plot.title = element_text(size = rel(1)),
                    axis.text = element_text(size = rel(0.75)),
                    axis.text.x = element_text(angle = 0, vjust = .5, hjust = .5),
                    panel.grid.minor = element_line(colour="grey90", size=0.5),
                    panel.grid.major = element_line(colour="grey90", size=0.5),
                    legend.position = "top",
                    legend.box = "horizontal",
                    complete = TRUE)
}

