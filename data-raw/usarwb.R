# libraries
library(tidyverse)
library(readxl)
library(readr)

# --- IO Data for real wage bundle
usarwb <- read_excel(
  path="~/MyRProjects/niotables/data-raw/USA_NIOT_nov16.xlsx",
  sheet = "National IO-tables"
) %>%
  dplyr::filter(
    Origin=="Domestic"
  ) %>%
  dplyr::select(
    c("Year","Code","CONS_h")
  ) %>%
  as.data.frame()

usethis::use_data(usarwb, overwrite = TRUE)
