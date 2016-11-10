library(magrittr)
library(readr)
library(plyr)

# load all global data
df.meds <- read_csv("data/data-meds.csv", col_names = TRUE)
df.comps <- read_csv("data/data-comps.csv", col_names = TRUE)

# source all global functions
list.files("global-functions/", full.names = TRUE) %>%
  lapply(source)