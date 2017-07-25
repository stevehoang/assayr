#' CoA data from PureHoney.
#'
#' Analyzed data from PureHoney plates 541 and 542, this data has been through the QC process and
#' is ready to be analyzed and plotted for results visualization.
#'
#' @format A data frame with 30 variables:
#' \describe{
#' \item{\code{run}}{study run id}
#' \item{\code{sample_id}}{study level unique sample id}
#' \item{\code{cell_type}}{type of cell}
#' \item{\code{donor}}{cell donor name}
#' \item{\code{donor_pheno}}{phenotype of cell donor}
#' \item{\code{media}}{base media used}
#' \item{\code{tx_run}}{paste of \code{tx_cmpd} and \code{run}}
#' \item{\code{tx_treat}}{challenge condition}
#' \item{\code{tx_cmpd}}{compound id}
#' \item{\code{tx_conc}}{concentration of \code{tx_cmp} in uM}
#' \item{\code{imaging_plate}}{culture/imaging plate id}
#' \item{\code{imaging_well}}{culture/imaging well id}
#' \item{\code{assay_plate}}{assay plate id}
#' \item{\code{assay_prun}}{assay read/run id}
#' \item{\code{assay_well}}{assay well id}
#' \item{\code{plate_run}}{paste of \code{assay_plate} and \code{assay_prun}}
#' \item{\code{contents}}{\code{assay_plate} level sample id}
#' \item{\code{targ}}{analyte/target measured}
#' \item{\code{iso_label}}{is \code{targ} isotopicly labeled?}
#' \item{\code{curve_id}}{standard curve used}
#' \item{\code{curve_plot}}{\code{targ} base name, used for plotting both species together}
#' \item{\code{conc}}{concentration \code{targ} in nM}
#' \item{\code{in_range}}{is \code{conc} in the range of detection?}
#' \item{\code{recov}}{fraction of internal standard measured}
#' \item{\code{conc_corrected}}{\code}{conc / recov}
#' \item{\code{nuc_well}}{count of nuclei per \code{imaging_plate:imaging_well}}
#' \item{\code{cell_vol_well_L}}{\code{nuc_well * (3.4 * 10^-12)} for hepatocytes}
#' \item{\code{conc_nmols}}{\code{conc * (.0001 + cell_volume_well_L)}}
#' \item{\code{conc_incell_nM}}{\code{conc_nmols / cell_volume_well_L}}
#' \item{\code{conc_incell_uM}}{\code{conc_incell_nM / 1000}}
#' }
#'
#' For futher details talk to Nate (nathan.day@hemoshear.com)
"coa"

#' Fitted dose response models
#'
#' Three model fits for Isobutryl-CoA dose response from PureHoney plate 547.
#'
#' @format A list with three \code{drc} objects
#'
"drcs"
