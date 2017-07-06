# Parser for PureHoney csv outputs
# automatically grab target for respective plate reads

#' @export
readPHRaw <- function(f, plate_id) {

    out_ncol_max <- max(count.fields(file = f, sep = ","))
    output <- read.table(file = f, sep = ",", fill = T,
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
