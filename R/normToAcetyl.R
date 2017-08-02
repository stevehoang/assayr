#' @title Normalize to acetyl-CoA
#' @description Create a column in a PH analysis, \code{to_acoa_ratio}, that gives the ratio of analytes to acetyl-CoA
#' @param x A tibble or data frame with PureHoney data including vars(tx_cmpd, curve_plot).
#' @return A data frame.
#' @export
normToAcetyl <- function(x) {
  acoa <- dplyr::filter(x, targ == "12C-Acetyl") %>%
    dplyr::select(plate_run, sample_id, conc_corrected) %>%
    dplyr::rename(acoa_conc = conc_corrected)
  other <- dplyr::filter(x, targ != "12C-Acetyl")
  res <- merge(other, acoa) %>%
    dplyr::mutate(to_acoa_log2_ratio = log2(conc_corrected / acoa_conc)) %>%
    dplyr::select(-acoa_conc)
}
