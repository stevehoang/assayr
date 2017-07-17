#' @title PureHoney Assay Layout Reader
#' @description High level wrapper function that that imports the assay layout for PureHoney as a tibble. Uses fixed offset from document markers to capture specific regions, prone to errors if assay document template is altered.
#' @param layout_file Path to .xlsx file with PureHoney layout and sample data.
#' @param plate_ids Optional argument to extract only specific plate(s) from multi-plate document. Expects character vector.
#' @param meta_cols Numeric vector defining the columns to extract for meta section. Default is most common arrangment.
#' @param meta_names Character vector defining column names for meta section. Length must match \code{legnth(meta_cols)}. Default is most common arrangment.
#' @param skip Number of rows to ignore when looking for plates. Useful for avoiding standard curve references in the header.
#' @examples
#' meta <- readPHAssay("CoA plates 100000537_100000538_RNO02004_PAH0119_6-29-17.xlsx")
#' @export
readPHAssay <- function(layout_file,
                        plate_ids = NULL,
                        meta_cols = c(1,3:5),
                        meta_names = c("contents", "sample_id", "imaging_plate", "imaging_well"),
                        skip = 10) {

    read <- read.xlsx(layout_file, colNames = F)

    plates <- grep("^1000\\d*", read[,1]) %>%
        set_names(grep("^1000\\d*", read[,1], value = T))

    if (!is.null(plate_ids)) {
        plates %<>% .[grepl(plate_ids, names(plates))]
    }

    plates %<>% .[. > skip]
    layout <- map_df(plates,
                     ~ meltPlate(read[c((.+3) : (.+10)),2:13]),
                     .id = "plate")

    meta <- map_df(plates,
                   ~ read[c((.+13) : (.+84)), meta_cols], .id = "plate") %>%
        set_names(c("plate", "contents", "sample_id", "imaging_plate", "imaging_well"))

    meta %<>% full_join(layout, by = c("plate", "contents")) %>%
        filter(!is.na(row))
    return(meta)

}
