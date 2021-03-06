% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{drcBreaks}
\alias{drcBreaks}
\title{Define nice breaks for dose response curves}
\usage{
drcBreaks(limits)
}
\arguments{
\item{limits}{Default \code{ggplot2} numeric object of length 2, defining upper and lower bounds for a scale.}
}
\value{
A numeric vector with the break makes to be plotted.
}
\description{
A helper function to be used in \code{ggplot() + scale_x_continuous(..., breaks = drcBreaks)} for the compound concentration.
}
\examples{
ggplot(data = ph_drc, aes(tx_conc, conc_incell_uM) +
geom_curve() +
scale_x_continuous(breaks = drcBreaks, labels = drcLabels)
}
