# libraries
library(tidyverse)
library(readxl)
library(readr)

# --------- Read SEA data
# Source: WIOD 2016 Release (https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/PJ2M1C)
d1_1 <- readxl::read_excel(
  path="~/MyRProjects/clptheory/data-raw/WIOD_SEA_Nov16.xlsx",
  sheet = "DATA"
) %>% 
  tidyr::pivot_longer(
    !c("country", "variable", "code", "description"),
    names_to = "year",
    values_to = "value"
  ) %>%
  tidyr::pivot_wider(
    names_from = "variable",
    values_from = "value"
  ) %>%
  as.data.frame()

# Read Nominal Exchange Rate data
# Source: OECD (https://data.oecd.org/conversion/exchange-rates.htm?fbclid=IwAR2Bn6JlF8WfAbVhwDEiKFhG0_rsTu-iLq1PW47o1ujebwc7CWF0e0wUx_A)
d1_2 <- readxl::read_excel(
  path="~/MyRProjects/clptheory/data-raw/Exchange_Rates_OECD.xlsx"
) %>%
  dplyr::mutate(
    year = as.character(TIME)
  ) %>%
  as.data.frame()

# Merge the two data sets
d1_3 <- dplyr::left_join(d1_1, d1_2, by = c("country", "year"))

# ---- Keep data for USA
usasea <- d1_3 %>%
  dplyr::filter(country=="USA") %>%
  dplyr::select(
    -c("INDICATOR","SUBJECT","MEASURE","FREQUENCY","TIME")
    ) %>%
  as.data.frame()

# Remove intermediate data sets
remove(d1_1)
remove(d1_2)
remove(d1_3)

usethis::use_data(usasea, overwrite = TRUE)
