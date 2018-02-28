library(tidyverse)
rm(list = ls())

read_iop <- function(file) {
  df <- data.table::fread(file) %>%
    setNames(gsub("\\(", "_", names(.))) %>%
    setNames(gsub("\\)", "", names(.))) %>%
    setNames(make.names(names(.), unique = TRUE)) %>% 
    as_tibble()
  return(df)
}