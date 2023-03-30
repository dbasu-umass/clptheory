# libraries
library(tidyverse)
library(readxl)
library(readr)

# --------- Read SEA data
# Source: WIOD 2016 Release
d1_1 <- readxl::read_excel(
  path="~/MyRProjects/clptheory/data-raw/WIOD_SEA_Nov16.xlsx",
  sheet = "DATA"
) %>%
  dplyr::select(-description) %>%
  pivot_longer(
    !c("country", "variable", "code"),
    names_to = "year",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = "variable",
    values_from = "value"
  ) %>%
  as.data.frame()

# ------- Read Nominal Exchange Rate data
# Source: OECD
d1_2 <- readxl::read_excel(
  path="~/MyRProjects/clptheory/data-raw/Exchange_Rates_OECD.xlsx"
) %>%
  mutate(
    year = as.character(TIME)
  ) %>%
  as.data.frame()

# ------- Merge the two data sets
d1_3 <- left_join(d1_1, d1_2, by = c("country", "year"))

# ----------- Create Variables for SEA
d1 <- d1_3  %>%
  # Convert variables into US dollars
  mutate(
    # Compensation of employees (million USD)
    COMP_USD = COMP/NOMEXCH,
    # Total compensation (million )
    LAB_USD = LAB/NOMEXCH,
    VA_USD = VA/NOMEXCH,
    GO_USD = GO/NOMEXCH
  ) %>%
  dplyr::mutate(
    # Nominal wage rate (dollar/hour)
    WAGE = COMP_USD/H_EMPE,
    # Wage share in value added
    WSHARE = LAB_USD/VA_USD
  ) %>%
  group_by(country,year) %>%
  mutate(
    # Average nominal wage (across industries) in dollar/hour
    AVGWAGE = mean(WAGE, na.rm = TRUE),
    # Minimum nominal wage (across industries) in dollar/hour
    MINWAGE = min(WAGE, na.rm=TRUE),
    # Average wage share (across industries)
    AVGWSHARE = mean(WSHARE, na.rm = TRUE),
    # Total profit share (over all industries)
    PSHARE_TOTAL = 1-(sum(LAB_USD)/sum(VA_USD))
  ) %>%
  dplyr::mutate(
    # Relative wage
    relwage1 = WAGE/AVGWAGE,
    relwage2 = WAGE/MINWAGE,
    # Labor input in hours accounting for skill
    # Unit: million hours of simple labor
    HRS1 = H_EMPE*relwage1,
    HRS2 = H_EMPE*relwage2,
    # Labor input in number of employees accounting for skill
    EMP1 = EMPE*relwage1,
    EMP2 = EMPE*relwage2
  ) %>%
  arrange(country,year) %>%
  as.data.frame()

# ---- Keep data for USA
usasea <- d1 %>%
  dplyr::filter(country=="USA") %>%
  as.data.frame()

# Remove intermediate data sets
remove(d1_1)
remove(d1_2)
remove(d1_3)
remove(d1)


usethis::use_data(usasea, overwrite = TRUE)
