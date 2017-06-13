anyBaseToDecimal <- function(value, symbols=LETTERS, zero_indexed=F) {
  symbols %<>% as.character()
  value %<>% as.character() %>%
    strsplit(split = "") %>%
    `[[`(1) %>%
    rev %>%
    match(symbols)
  if (zero_indexed) {value %<>% `-`(1)}
  res <- value %*% (length(symbols) ^ (seq_along(value) - 1)) %>% drop
  return(res)
}
