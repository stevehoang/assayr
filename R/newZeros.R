#' @title Convert concentrations of zero to non-zero values in dose response curves
#' @description Tool to adjust concentration values of zero to non-zero values based on the average gap between concentraions in dose response curve. Adjust zero concentrations to be two average gaps less than the lower non-zero concentration. Useful for plotting on with `scale_x_log10()`
#' @param vec A numeric vector with the concentrations in a dose response curve
#' @param is_log Default is FALSE. Are the concentration values in `vec` log transformed?
#' @return A numeric value to be assigned as the new non-zero concentration to represent actual concentrations of zero.
#' @examples
#' drc_concs <- c(0, .3, 1, 3, 10, 30, 100)
#' drc_concs[drc_concs == 0] <- newZeros(drc_concs)
#' drc_concs
#' @export
newZeros <- function(vec, is_log = F) {

    vec %<>% unique() %>% sort()
    vec <- vec[vec != 0]

    if (!is_log) { vec %<>% log10() }

    avg_gap <- ( vec - dplyr::lead(vec) ) %>%
        mean(na.rm = T) %>%
        abs()

    nz <- min(vec) - 2*avg_gap

    if (!is_log) { nz %<>% 10^. }

    return(nz)

}
