#' Convert from anybase to decimal
#'
#' @param value The starting value
#' @param symbols A vector containing all of the symbols used in the origin place
#' value system listed in sequential order.
#' @param zero_indexed Logical. Set to TRUE if the first value in \code{symbols}
#' represents 1, FALSE if 0.
#' @return \code{value} converted to decimal
#' @examples
#' anyBaseToDecimal("A")
#' anyBaseToDecimal("ABC")
#' anyBaseToDecimal("FF", symbols = c(0:9, LETTERS[1:6]), zero_indexed=T) #hex to decimal
#' @export
anyBaseToDecimal <- function(value, symbols=LETTERS, zero_indexed=F) {
  symbols %<>% as.character()
  value %<>% as.character() %>%
    strsplit(split = "") %>%
    `[[`(1) %>%
    rev %>%
    match(symbols)
  if (zero_indexed) {value %<>% `-`(1)}
  res <- value %*% (length(symbols) ^ (seq_along(value) - 1)) %>% drop
  return(res)
}

#' @title Export plots
#'
#' @description Takes a list of plot objects and builds a folder with those plots as individual png files
#' @param plot_list List containing plot objects
#' @param dir_path Path for output directory that will be created
#' @param overwrite Logical indicating whether to overwrite an existing directory with the same path. Defaults to \code{TRUE} for easy analysis interation. Adjust to \code{FALSE} to add individual images to existing directory, may throw errors about existing files.
#' @param session_info Logical whether to include a \code{gtools::textplot} with the output of \code{sessionInfo()}. Useful for reproducible research.
#' @param height Numeric object describing height of graphics device. Default unit is inches.
#' @param width Numeric object describing width of graphics device. Default unit is inches.
#' @param units Character object, for unit used in height and width, "in" (inches by default). Can be "px", "mm", or "cm".
#' @return A directory with the elements of \code{plot_list} as png files
#' @examples
#' plots <- mtcars %>% split(.$cyl) %>%
#'            purrr::map(~ ggplot(., aes(mpg, disp)) +
#'                           geom_point())
#' outputPlotsAsPdfs(plots, "~/Desktop/mtcars_plots_by_cyl.pdf")
#' @export
outputPlotsAsPngs <- function(plot_list, dir_path, overwrite = TRUE, session_info = TRUE,
                              height = 8.5, width = 11, units = "in") {
# exports a list of plots into as a folder of .pngs
# used plot_title from ggplot objs and element name from non-ggplot obs as png file name
# default is overwriting existing directoy, usually what you want for plot iterations

  if (overwrite) {
    if(file.exists(dir_path)) {
      system(paste("rm -r", dir_path))
    }
  }
  if (!file.exists(dir_path)) {
    system(paste("mkdir", dir_path))
  }
  if (session_info) {
    sessionInfo() %>% capture.output() %>% gplots::textplot()
    title("Session_Info")
    plot_list[["SessionInfo"]] <- recordPlot()
  }
  for (i in 1:length(plot_list)) {
    png_title <- NULL
    if (class(plot_list[[i]])[1] == "gg") {
        png_title <- plot_list[[i]]$labels$title
    }
    if (length(png_title) < 1) {
        png_title <- names(plot_list)[i]
    }
    png_title %<>% gsub(" |\\:|/", "_", .)
    png(filename = paste0(dir_path, "/", i, "_", png_title,
                          ".png"), units = units, height = height, width = width,
        res = 300)
    print(plot_list[[i]])
    dev.off()
}

}

#' @title Export plots
#'
#' @description Takes a list of plot objects and builds a pdf document with those plots.
#' @param plot_list List containing plot objects.
#' @param pdf_path Path for output pdf that will be created. Should end in ".pdf".
#' @param overwrite Logical indicating whether to overwrite an existing directory with the same path. Defaults to \code{TRUE} for easy analysis interation. Adjust to \code{FALSE} to add individual images to existing directory, may throw errors about existing files.
#' @param session_info Logical whether to include a \code{gtools::textplot} with the output of \code{sessionInfo()}. Useful for reproducible research.
#' @param height Numeric object describing height of graphics device. Default unit is inches.
#' @param width Numeric object describing width of graphics device. Default unit is inches.
#' @return A directory with the elements of \code{plot_list} as png files
#' @examples
#' plots <- mtcars %>% split(.$cyl) %>%
#'            purrr::map(~ ggplot(., aes(mpg, disp)) +
#'                           geom_point())
#' outputPlotsAsPngs(plots, "~/Desktop/mtcars_plots_by_cyl/")
#' @export
outputPlotsAsPdfs <- function(plot_list, pdf_path, overwrite = TRUE, session_info = TRUE,
                              height = 8.5, width = 11) {
  # exports a list of plots into as a folder of .pngs
  # used plot_title from ggplot objs and element name from non-ggplot obs as png file name
  # default is overwriting existing directoy, usually what you want for plot iterations

  if (overwrite) {
    if(file.exists(pdf_path)) {
      system(paste("rm", pdf_path))
    }
  }
  if (session_info) {
    sessionInfo() %>% capture.output() %>% gplots::textplot()
    title("Session_Info")
    plot_list[["SessionInfo"]] <- recordPlot()
  }
  pdf(pdf_path, height = height, width = width)
  purrr::walk(plot_list, print)
  dev.off()

}


#' Fill NAs in vector with neighboring values
#'
#' @description Back- or forward-fill NAs in a vector
#' @param vector The starting vector
#' @param reverse Logical. The direction to traverse \code{vector}
#' @return The back (forward) filled vector.
#' @examples
#' x <- c("A", rep(NA, 8), "Z")
#' fillNAs(x)
#' fillNAs(x, reverse=T)
#' @export
fillNAs <- function(vector, reverse = F) {
# fills NA values with previous non-NA values
# works in forward and reverse
    if (reverse) {
        seq <- length(vector):1
    }
    if (!reverse) {
        seq <- 1:length(vector)
    }
    for (i in seq) {
        if (!is.na(vector[i])) {
            j <- vector[i]
        }
        if (is.na(vector[i])) {
            vector[i] <- j
        }
    }
    return(vector)
}

#' Make a vector of serially halved values.
#'
#' @param highest The highest value in the geometric progression.
#' @param number The number of values in addition to \code{highest} in the sequence.
#' @return A vector containing the geometric progression
#' @examples
#' makeSerialDilution(10, 5)
#' @export
makeSerialDilution <- function(highest, number) {
# calculates serial dilution series
# set highest concentration and number of diluttions
  dils <- 2 ^ (-1 * seq(0, number, 1))
  std_conc <- dils * highest
  return(std_conc)
}

#' Define nice breaks for dose response curves
#'
#' A helper function to be used in \code{ggplot() + scale_x_continuous(..., breaks = drcBreaks)} for the compound concentration.
#' @param limits Default \code{ggplot2} numeric object of length 2, defining upper and lower bounds for a scale.
#' @return A numeric vector with the break makes to be plotted.
#' @examples
#' ggplot(data = ph_drc, aes(tx_conc, conc_incell_uM) +
#' geom_curve() +
#' scale_x_continuous(breaks = drcBreaks, labels = drcLabels)
#' @export
drcBreaks <- function(limits) {
  breaks <- c(.01, .1, .3, 1, 3, 10, 30, 100, 300)
  breaks <- breaks[breaks >= limits[1] & breaks <= limits[2]]
  return(breaks)
}

#' Overwrite lowest value label with "0" for dose response curves
#'
#' A helper function to be used in \code{ggplot() + scale_x_continuous(..., labels = drcLabels)} for relabeling the lowest compound concentration from its log-approximate value. This function is the correction for \code{newZeros()}.
#' @param limits Default \code{ggplot2} numeric object of length 2, defining upper and lower bounds for a scale.
#' @return A numeric vector with the the lowest value set to 0.
#' @examples
#' ggplot(data = ph_drc, aes(tx_conc, conc_incell_uM) +
#' geom_curve() +
#' scale_x_continuous(breaks = drcBreaks, labels = drcLabels)
#' @export
drcLabels <- function(breaks) {
  labels <- breaks
  labels[1] <- 0
  return(labels)
}


#' Geometric mean
#'
#' Calculate the geometric mean.
#' @param x A numeric vector whose geometric mean is to be calulated
#' @return The geometric mean of \code{x}.
#' @examples
#' x <- rpois(10, lambda = 10)
#' geom_mean(x)
#' @export
geom_mean <- function(x) {
  x <- stats::na.omit(x)
  res <- mean(log(x))
  res <- exp(res)
  return(res)
}

#' Geometric standard error of the mean
#'
#' Calculate the geometric standard error of the mean.
#' @param x A numeric vector whose geometric standard error of the mean is to be calulated.
#' @return The geometric standard error of the mean of \code{x}.
#' @examples
#' x <- rpois(10, lambda = 10)
#' geom_mean_se(x)
#' @export
geom_mean_se <- function(x, mult = 1) {
  x <- stats::na.omit(x)
  x <- log(x)
  se <- mult * sqrt(stats::var(x)/length(x))
  mean <- mean(x)
  res <- data.frame(y = exp(mean), ymin = exp(mean - se), ymax = exp(mean + se))
  return(res)
}
