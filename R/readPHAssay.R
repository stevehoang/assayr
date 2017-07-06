# Pure Honey Assay Layout Parser

#'
#'@export
readPHAssay <- function(layout_file,
                        plate_ids = NULL,
                        meta_cols = c(1,3:5),
                        meta_names = c("contents", "sample_id", "imaging_plate", "imaging_well")) {
    
    read <- read.xlsx(layout_file, colNames = F)
    
    plates <- grep("^1000\\d*", read[,1]) %>%
        set_names(grep("^1000\\d*", read[,1], value = T))
    
    if (!is.null(plate_ids)) {
        plates %<>% .[grepl(plate_ids, names(plates))]
    }
    
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
