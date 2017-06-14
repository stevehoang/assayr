meltPlate <- function(plt) {
  colnames(plt) <- as.character(1:ncol(plt))
  plt$row <- LETTERS[1:nrow(plt)]
  plt_m <- tidyr::gather(plt, key = "column",value= "contents", -row)
  plt_m$column %<>% as.character()
  return(plt_m)
}

meltPlateXlsx <- function(f, sheet, rows, cols) {
  if (is.character(cols)) {
    temp <- paste(cols, collapse = "") %>%
      strsplit(split = "") %>%
      `[[`(1)
    stopifnot(all(temp %in% LETTERS))
    cols <- sapply(cols, anyBaseToDecimal)
  }
  rows <- seq(rows[1], rows[2], 1)
  cols <- seq(cols[1], cols[2], 1)
  plt <- openxlsx::read.xlsx(xlsxFile = f, sheet = sheet, cols = cols,
                             rows = rows, colNames=F)
  plt_m <- meltPlate(plt)
  return(plt_m)
}



