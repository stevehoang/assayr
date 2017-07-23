#' Generic parser for Biorad CFX thermocycler
#'
#' Reads the defualt output csv from CFX Manager 3.1.1621.0826
#'
#' @param file Path to a BioRad CFX output csv.
#' @param capture A single capture regex for extracting plate id from file.
#'
#' @examples
#' f <- boxr::box_dl(198532377451, local_dir = "~/.Trash/") %>%
#'        readCFX(capture = ".*_(\\d)\\.csv")
#'
#' @export
readCFX <- function(file,
                    capture = "") {

  result <- read.csv(file) %>%
    .[,1:2] %>%
    magrittr::set_colnames(c("well", "cq")) %>%
    .[(grep("Well$", .$well)+1):nrow(.),] %>%
    dplyr::mutate(plate = gsub(capture, "\\1", file))

  return(result)
}
