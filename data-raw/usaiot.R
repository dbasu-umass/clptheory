# libraries
library(tidyverse)
library(readxl)
library(readr)

# --- IO Data for input-output matrix
# Source: WIOD 2016 Release
usaiot <- read_excel(
  path="~/MyRProjects/clptheory/data-raw/USA_NIOT_nov16.xlsx",
  sheet = "National IO-tables"
) %>%
  dplyr::filter(
    Origin=="Domestic"
  ) %>%
  dplyr::select(
    -c("Description","Origin",
       "CONS_h", "CONS_np","CONS_g","GFCF","INVEN","EXP","GO")
  ) %>%
  as.data.frame()

usethis::use_data(usaiot, overwrite = TRUE)
