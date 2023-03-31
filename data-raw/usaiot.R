# libraries
library(tidyverse)
library(readxl)
library(readr)

# --- IO Data for input-output matrix
# Source: WIOD 2016 Release (https://data.oecd.org/conversion/exchange-rates.htm?fbclid=IwAR2Bn6JlF8WfAbVhwDEiKFhG0_rsTu-iLq1PW47o1ujebwc7CWF0e0wUx_A)
usaiot <- read_excel(
  path="~/Dropbox/WIOD/Data/USA_NIOT_nov16.xlsx",
  sheet = "National IO-tables"
) %>% 
  dplyr::filter(
    Origin=="Domestic"
  ) %>%
  as.data.frame()

usethis::use_data(usaiot, overwrite = TRUE)
