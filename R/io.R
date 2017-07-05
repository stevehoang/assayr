#' Melt a data frame from plate format to long format
#'
#' @param plt A data frame representing an assay plate.
#' @return The melted data frame.
#' @examples
#' plate <- as.data.frame(matrix(rnorm(96), nrow = 8))
#' meltPlate(plate)
#' @export
meltPlate <- function(plt) {
  colnames(plt) <- as.character(1:ncol(plt))
  plt$row <- LETTERS[1:nrow(plt)]
  plt_m <- tidyr::gather(plt, key = "column",value= "contents", -row)
  plt_m$column %<>% as.character()
  return(plt_m)
}

#' Read xlsx data in plate format and convert to long format
#'
#' @description Read in plate format data from an xlsx file and return a melted
#' data frame with the contents and well coordinates
#' @param f A character string -- the xlsx file.
#' @param sheet The name or index of the sheet.
#' @param rows a numeric vector of the rows to read
#' @param cols a numeric vector of the columns to read
#' @return A melted data frame.
#' @export
meltPlateXlsx <- function(f, sheet, rows, cols) {
  if (is.character(cols)) {
    temp <- paste(cols, collapse = "") %>%
      strsplit(split = "") %>%
      `[[`(1)
    stopifnot(all(temp %in% LETTERS))
    cols <- sapply(cols, anyBaseToDecimal)
  }

  plt <- openxlsx::read.xlsx(xlsxFile = f, sheet = sheet, cols = cols,
                             rows = rows, colNames = F, skipEmptyCols = F)
  plt <- plt[, min(cols):ncol(plt)]
  if (ncol(plt) < length(cols)){
      plt[, (ncol(plt)+1):length(cols)] <- NA
  }
  plt_m <- meltPlate(plt)
  return(plt_m)
}


