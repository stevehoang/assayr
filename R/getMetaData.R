#' Import meta data
#'
#' Reads meta data from sample databases on Box via \code{boxr::box_read_excel()}
#'
#' @param study_id Character vector in the standard study name format (i.e. "RNO0101-1" or "RNO0101").
#' @param fully_parse Logical indicating whether to try and filter the database to a specific run; Set \code{FALSE} to troubleshoot.
#' @return A data frame containing the meta information from a given study or a large data frame with all the meta information from all studies in a program.
#' @examples
#' ## Requires boxR credentials
#' # returns only meta data from study RNO0738
#' meta <- getMetaData("RNO0738")
#' # return meta data from multiple studies
#' meta <- getMetaData(c("HEM0100-5", "HEM0100-6"))
#'
#' # returns all studies' meta data from RNO07 project
#' all_meta <- getMetaData("RNO0738", fully_parse = F)
#'@export
getMetaData <- function(study_id, fully_parse = TRUE) {
    # manually adjusted for new experiments
    box_files <- c(101838632791, 161176997524, 161176997524, 33980749645,
                   185483159790, 71120788417) %>%
        set_names(c("PAH", "PAU", "PAG", "RNO",
                    "HEM", "RSQ"))
    # manually map for sheet names of interest
    sheet_names <- c("PAH", "PAU_Huh7", "PAG_HepG2", "RNO07 Static",
                     "RNO20 (PBMC)", "HEM01", "RSQ05 MF") %>%
        set_names(c("PAH", "PAU", "PAG", "RNO07",
                    "RNO20", "HEM", "RSQ05"))

    result <- tibble() # container var
    for ( si in study_id ) {

        program <- gsub("^(\\D{3}).*", "\\1", si)

        box_file <- box_files[program]

        if ( program %in% c("RNO", "RSQ") ) {
            program <- gsub("^(\\D{3}\\d{2}).*", "\\1", si)
        }

        box_which <- sheet_names[program]

        meta <- box_read_excel(box_file, which = box_which, col_names = T, col_types = "text")

        if (fully_parse) {
            meta %<>% filter(`STUDY NAME` == si)
        }
        result %<>% bind_rows(meta)
    }

    return(result)
}

