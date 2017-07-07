#' @title PureHoney Assay Result Reader
#' @description High level wrapper function that that imports the raw data output from PureHoney as a tibble.
#' @param raw_file Path to .csv file with PureHoney raw output.
#' @param plate_id Character vector describing the plate id. Typically just the full file name.
#' @examples
#' raw <- dir(".", pattern = "\\.csv", full.names = T) %>%
#'          map_df(~ readPHRaw(., plate_id = .))
#' @export
readPHRaw <- function(raw_file, plate_id) {

    out_ncol_max <- max(count.fields(file = raw_file, sep = ","))
    output <- read.table(file = raw_file, sep = ",", fill = T,
                         col.names = 1:out_ncol_max, stringsAsFactors = F)

    species_ids <- grep("XIC", output[,1],value = T)
    species_ids %<>% gsub("XIC = xic-", "", .)
    plate_starts <- grep("XIC", output[,1]) + 2
    plate_stops <- plate_starts + 7

    data <- list()
    for (c in 1:length(species_ids)) {
        data[[c]] <- output[c(plate_starts[c]:plate_stops[c]),2:13] %>%
            assayr::meltPlate() %>%
            dplyr::mutate(targ = species_ids[c],
                          plate_id = plate_id) %>%
            dplyr::rename(raw = contents)
    }
    return( dplyr::bind_rows(data) )
}
