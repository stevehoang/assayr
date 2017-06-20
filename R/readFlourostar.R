# generic parser for Omega FlouroStar plate analyzer
# detects normal columns (ie raw, blank_raw, machine_fit, etc)
# works regardless of start row and column order
# optional caputure expression for grabbing plate ids from file names
# optional column drops based on chr vector of column names

readFlourostar <- function(f, plate_capture = "", col_drops = NA) {
    df <- openxlsx::read.xlsx(f, sheet = 1)
    # grab original length of plate
    df_len <- nrow(df)
    # check for col id row
    id_pos <- grep("^Well", df[,1])
    id_row <- unlist(df[id_pos,])
    wanted <- c(".*fit.*", "^Blank.*", "^Raw", ".*Col", ".*Row", "Content", "Standard")
    ids <- sapply(wanted, grep, id_row) %>% unlist
    c_names <- id_row[ids]
    df <- df[(id_pos + 1):df_len, ] %>%
        magrittr::set_names(id_row)
    colnames(df) %<>% gsub(".*fit.*", "mach_fit", .)
    colnames(df) %<>% gsub("^Blank.*", "b_raw", .)
    colnames(df) %<>% gsub("^Raw.*1.*", "raw1", .)
    colnames(df) %<>% gsub("^Raw.*2.*", "raw2", .)
    colnames(df) %<>% gsub("^Raw.*", "raw", .)
    colnames(df) %<>% gsub(".*Col", "col", .)
    colnames(df) %<>% gsub(".*Row", "row", .)
    colnames(df) %<>% gsub("Content", "contents", .)
    colnames(df) %<>% gsub("Standard.*", "std_conc", .)
    df$plate <- gsub(plate_capture, "\\1", f, ignore.case = T)
    desired_order <- c("plate", "row", "col", "contents", "std_conc", "raw","raw1", "raw2", "b_raw", "mach_fit")
    if (!is.na(col_drops)) {
        drops <- lapply(col_drops, grep, desired_order) %>% unlist
        drops <- drops*-1
        desired_order <- desired_order[drops]
    }
    possible_order <- desired_order[desired_order %in% colnames(df)]
    if(length(possible_order) < length(desired_order)) {
        warning("Missing desired columns; careful with output for rbinds")
    }
    df <- df[,possible_order]
    numbers <- (grep("content", possible_order) + 1) : (length(possible_order))
    if (length(numbers) > 1) {
        df[,numbers] <- lapply(df[,numbers], as.numeric)
    }
    if (length(numbers) == 1) {
        df[,numbers] %<>% as.numeric()
    }
    return(df)
}
