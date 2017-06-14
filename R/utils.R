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

plot_list_to_png <- function(plot_list, dir_path, overwrite = T) {
# exports a list of plots into as a folder of .pngs
# used plot_title from ggplot objs and element name from non-ggplot obs as png file name
# default is overwriting existing directoy, usually what you want for plot iterations
    
    if(file.exists(dir_path)) {
        system(paste("rm -r", dir_path))
    }
    
    system(paste("mkdir", dir_path))
    
    for (i in 1:length(plot_list)) {
        # want to use existing plot title as file name for png
        if (class(plot_list[[i]])[1] == "gg") { # handles ggplots (ie most of 'em) 
            png_title <- plot_list[[i]]$labels$title
        }
        if (length(png_title) < 1) {
            png_title <- names(plot_list)[i]
        }
        png_title %<>% gsub(" |\\:", "_", .)
        # open device to plot
        png(filename = paste0(dir_path, "/", i, "_", png_title,".png"),
            units = "in", height = 8.5, width = 11, res = 300)
        print(plot_list[[i]])
        dev.off()
    }
}

na_filler <- function(vector, reverse = F) {
# fills NA values with previous non-NA values
# works in forward and reverse
    if (reverse) {
        seq <- length(vector):1
    }
    if (!reverse) {
        seq <- 1:length(vector)
    }
    for (i in seq) {
        if (!is.na(vector[i])) {
            j <- vector[i]
        }
        if (is.na(vector[i])) {
            vector[i] <- j
        }
    }
    return(vector)
}

serialMaker <- function(highest, number) {
# calculates serial dilution series
# set highest concentration and number of diluttions
    std_conc <- c(highest)
    for(i in 1:number) {
        no <- std_conc[i] / 2
        std_conc <- c(std_conc, no)
    }
    return(std_conc)
}