#' Generic parser for Omega FluoStar plate analyzer
#'
#' Versital parser that orders standard columns (ie raw, blank_raw, machine_fit, etc)
#' automatically for \code{rbind()}-ing. Verbose with warnings.
#'
#' @param file Path to a BMG FLUOstart Omega plate reader output xlsx.
#' @param capture A single capture regex for extracting plate id from file.
#' @param col_drops An optional character vector with the names of columns to be dropped.
#'
#' @examples
#' f <- boxr::box_dl(198532377451, local_dir = "~/.Trash/") %>%
#'        readFLUOstar(capture = ".*_(\\d)\\.csv")
#'
#' @export
readFLUOstar <- function(file,
                         capture = "",
                         col_drops = NA) {

    df <- openxlsx::read.xlsx(f, sheet = 1)

    df_len <- nrow(df)
    id_pos <- grep("^Well", df[,1])
    id_row <- unlist(df[id_pos,])
    wanted <- c(".*fit.*", "^Blank.*", "^Raw", ".*Col", ".*Row", "Content", "Standard")
    ids <- sapply(wanted, grep, id_row) %>% unlist
    c_names <- id_row[ids]
    df <- df[(id_pos + 1):df_len, ] %>%
        magrittr::set_names(id_row)

    # clean col names
    names(df) %<>% gsub(".*fit.*", "mach_fit", .) %>%
      gsub("^Blank.*", "b_raw", .) %>%
      gsub("^Raw.*1.*", "raw1", .) %>%
      gsub("^Raw.*2.*", "raw2", .) %>%
      gsub("^Raw.*", "raw", .) %>%
      gsub(".*Col", "col", .) %>%
      gsub(".*Row", "row", .) %>%
      gsub("Content", "contents", .) %>%
      gsub("Standard.*", "std_conc", .)

    df$plate <- gsub(capture, "\\1", f, ignore.case = T)

    desired_order <- c("plate", "row", "col", "contents", "std_conc", "raw",
                       "raw1", "raw2", "b_raw", "mach_fit")

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
