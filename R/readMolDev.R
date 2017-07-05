# generic parser for Molecular Devices Widefield scope output
# detects standard measurements (ie nuclear counts, smaa/nile intensisty, etc)
# automatically extracts plate information (some additonal cleaning may be required)

#' @export
readMolDev <- function(scope_txt, measurements = c("stell  nuc count", "mac nuc count", "smaa area",
                                                     "integrated int", "Nuclear Count",
                                                     "Vesicle", "Cell Count")) {
    if (grepl("\\.txt", scope_txt)) {
        out_ncol_max <- max(count.fields(file = scope_txt, sep = "\t"))
        output <- read.table(file = scope_txt, sep = "\t", fill = T, col.names = 1:out_ncol_max, stringsAsFactors = F)
    }
    if (grepl("\\.xlsx", scope_txt)) {
        output <- openxlsx::read.xlsx(xlsxFile = scope_txt, colNames = F)
    }
    if (!grepl("xlsx|txt", scope_txt)) {
        stop("This function is only set up to handle xlsx or txt files as inputs")
    }
    # output read based on ncols
    output <- as.data.frame(lapply(output, function(x) gsub(".\\dATF", "", x)), stringsAsFactors = F)
    # grab meta info and plate dims
    meta_info <- grep(".*Name \\[Plate Info\\].*", output[,1], value = T)
    run_names <- gsub(".*=(\\D+\\d+-?\\d?).*", "\\1", meta_info)
    plate_nums <- gsub(".*Plate *(\\d*|\\d*b).*","\\1", meta_info, ignore.case = T)
    plate_starts <- grep("(Well Name|Plate ID)", output[,1]) + 1
    plate_stops <- grep("Calculation applied", output[,1]) - 2
    plate_stops <- plate_stops[-1]
    plate_stops <- c(plate_stops, nrow(output))
    # id row for variables of interest (ultimately colnames)
    id_row <- droplevels(output[(plate_starts[1]-1),])
    id_row <- lapply(id_row, as.character)
    well_name <- grep("Well Name", id_row)
    site_id <- grep("Site ID", id_row)
    # based on measurement vector in function arguement
    measures <- sapply(measurements, grep, id_row) %>% unlist()
    columns_we_want <- c(well_name, site_id, measures)
    c_names <- id_row[columns_we_want]
    # loop and make plates dfs
    plates <- data.frame(stringsAsFactors = F)
    for (p in 1:length(plate_starts)) {
        p_start <- plate_starts[p]
        p_stop <- plate_stops[p]
        p_num <- plate_nums[p]
        r_name <- run_names[p]
        p_df <- output[p_start:p_stop,]
        p_df <- dplyr::select(p_df, columns_we_want)
        colnames(p_df) <- c_names
        p_df$plate <- p_num
        p_df$run <- r_name
        p_df %<>% dplyr::select(run, plate, everything())
        plates <- rbind(plates, p_df)
    }
    # polish colnames and uid
    colnames(plates) <- make.names(colnames(plates)) %>% gsub("Cell", "", .) %>%
        gsub("Custom.Module.", "", .) %>%
        gsub("\\.\\.", "", .)
    plates[,measures] %<>% sapply(function(x) as.numeric(x))
    plates$uid <- paste(plates$plate, plates$Well.Name, plates$Site.ID, sep = "_")
    colnames(plates) %<>% gsub("^Well.*", "well", .) %>% gsub("^Site.*", "site", .) %>%
        gsub("^Nuclear.*", "nuc_count", .) %>% gsub("^Vesicle.*", "nile_int", .)
    return(plates)
}
