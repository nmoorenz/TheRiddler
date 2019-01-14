library(tidyverse)

tbl <- tibble(x = 1e3:(2e3 - 1))

tbl <- tbl %>% mutate(
                      y = as.character(x), 
                      z = str_sort(y))


